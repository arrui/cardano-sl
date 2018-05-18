{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module InputSelection.Evaluation (
    evaluateInputPolicies
    -- * Interpreter
  , IntStats(..)
  , IntState -- Opaque
  , initIntState
  , intPolicy
  ) where

import           Universum

import           Control.Lens ((%=), (.=), (<<+=))
import           Control.Lens.TH (makeLenses)
import           Data.Conduit
import qualified Data.Conduit.Lift as Conduit
import qualified Data.Text as Text
import qualified Data.Text.Buildable
import qualified Data.Text.IO as Text
import           Formatting (bprint, build, sformat, (%))
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath ((</>))
import           System.IO (SeekMode (..), hSeek)
import           Text.Printf (printf)

import           InputSelection.Generator (Event (..))
import qualified InputSelection.Generator as Gen
import           InputSelection.Policy (CanRunPolicy (..), InputSelectionPolicy,
                                        PrivacyMode (..))
import qualified InputSelection.Policy as Policy
import           Util.Histogram (Histogram, BinSize(..))
import qualified Util.Histogram as Histogram
import           UTxO.DSL

{-------------------------------------------------------------------------------
  Interpreter running statistics
-------------------------------------------------------------------------------}

data IntStats = IntStats

initIntStats :: IntStats
initIntStats = IntStats

{-------------------------------------------------------------------------------
  Interpreter state
-------------------------------------------------------------------------------}

data IntState h a = IntState {
      _intStateUtxo       :: Utxo h a
    , _intStatePending    :: Utxo h a
    , _intStateStats      :: IntStats
    , _intStateFreshHash  :: Int

      -- | Change address
      --
      -- NOTE: At the moment we never modify this; we're not evaluating
      -- privacy, so change to a single address is fine.
    , _intStateChangeAddr :: a
    }

makeLenses ''IntState

initIntState :: Utxo h a -> a -> IntState h a
initIntState utxo changeAddr = IntState {
      _intStateUtxo       = utxo
    , _intStatePending    = utxoEmpty
    , _intStateStats      = initIntStats
    , _intStateFreshHash  = 1
    , _intStateChangeAddr = changeAddr
    }

instance Monad m => CanRunPolicy (StateT (IntState h a) m) a where
  genChangeAddr = use intStateChangeAddr
  genFreshHash  = intStateFreshHash <<+= 1

{-------------------------------------------------------------------------------
  Interpreter proper
-------------------------------------------------------------------------------}

-- | Interpreter for events, evaluating a policy
--
-- Turns a stream of events into a stream of UTxOs after each event.
intPolicy :: forall h a m. (Hash h a, Monad m)
          => InputSelectionPolicy h a (StateT (IntState h a) m)
          -> IntState h a -- Initial state
          -> ConduitT (Event h a) (Utxo h a) m IntStats
intPolicy policy initState =
    fmap (view intStateStats) $
      Conduit.execStateC initState $
        awaitForever $ \event -> do
          lift $ go event
          yield =<< use intStateUtxo
  where
    go :: Event h a -> StateT (IntState h a) m ()
    go (Deposit new) =
        intStateUtxo %= utxoUnion new
    go NextSlot = do
        -- TODO: May want to commit only part of the pending transactions
        pending <- use intStatePending
        intStateUtxo    %= utxoUnion pending
        intStatePending .= utxoEmpty
    go (Pay outs) = do
        utxo <- use intStateUtxo
        mtx  <- policy utxo outs
        case mtx of
          Right tx -> do
            intStateUtxo    %= utxoRemoveInputs (trIns tx)
            intStatePending %= utxoUnion (trUtxo tx)
          Left _err ->
            -- TODO: record some stats
            return ()

{-------------------------------------------------------------------------------
  Observing the UTxO
-------------------------------------------------------------------------------}

utxoHistogram :: BinSize -> Utxo h a -> Histogram
utxoHistogram binSize =
    Histogram.discretize binSize . map fromIntegral . outputs
  where
    outputs :: Utxo h a -> [Value]
    outputs = map (outVal . snd) . utxoToList

-- | Internal state used in 'observeUtxo'
data ObserveUtxoState = ObserveUtxoState {
      -- | Counter used to number the files we generate
      _ousStep      :: !Int

      -- | Accumulated maximum values in all bins
    , _ousMaxValues :: !Histogram
    }

makeLenses ''ObserveUtxoState

initObserveUtxoState :: BinSize -> ObserveUtxoState
initObserveUtxoState binSize = ObserveUtxoState {
      _ousStep      = 0
    , _ousMaxValues = Histogram.empty binSize
    }

-- | Sink that observes UTxOs
--
-- We return a histogram with the maximum values in all bins.
observeUtxo :: forall h a m. MonadIO m
            => Handle   -- ^ Handle to write gnuplot script instructions to
            -> FilePath -- ^ Prefix for the files to create
            -> BinSize  -- ^ Binsize for the UTxO histogram
            -> ConduitT (Utxo h a) Void m Histogram
observeUtxo hGnuplot prefix binSize =
    fmap _ousMaxValues $
      Conduit.execStateC (initObserveUtxoState binSize) $
        awaitForever (lift . go)
  where
    go :: Utxo h a -> StateT ObserveUtxoState m ()
    go utxo = do
        ousMaxValues %= Histogram.max hist
        ix <- ousStep <<+= 1
        let filename = printf "%08d" ix
            filepath = prefix </> filename
        liftIO $ Histogram.writeFile filepath hist
        liftIO $ Text.hPutStrLn hGnuplot $ sformat
          ( "set output '" % build % ".png'\n"
          % "plot '" % build % "' using 1:2 with boxes\n"
          )
          filename
          filename
      where
        hist = utxoHistogram binSize utxo


{-------------------------------------------------------------------------------
  Run evaluation
-------------------------------------------------------------------------------}

evaluatePolicy :: Hash h a
               => FilePath
               -> BinSize
               -> InputSelectionPolicy h a (StateT (IntState h a) IO)
               -> IntState h a
               -> ConduitT () (Event h a) IO ()
               -> IO ()
evaluatePolicy prefix binSize policy initState generator = do
    createDirectoryIfMissing False prefix
    withFile gnuplotScript WriteMode $ \hGnuplot -> do
      -- Leave space for plot instructions
      -- (which we know only after we have seen all histograms)
      Text.hPutStrLn hGnuplot $ Text.replicate 200 " "

      (stats, maxValues) <- runConduit $
        generator                  `fuse`
        intPolicy policy initState `fuseBoth`
        observeUtxo hGnuplot prefix binSize

      let (xRange, yRange) = Histogram.range maxValues

      hSeek hGnuplot AbsoluteSeek 0
      Text.hPutStrLn hGnuplot $ sformat
          ( "set grid\n"
          % "set xrange " % build % "\n"
          % "set yrange " % build % "\n"
          % "set boxwidth " % build % "\n"
          % "set term png\n"
          )
          xRange
          yRange
          binSize

      putStrLn $ sformat
        ( "Written '" % build % "'.\n"
        % "Statistics: " % build
        )
        gnuplotScript
        stats
  where
    gnuplotScript = prefix </> "mkframes.gnuplot"

evaluateInputPolicies :: FilePath -> IO ()
evaluateInputPolicies prefix = do
    evaluatePolicy
      (prefix </> "exact")
      (BinSize 10)
      Policy.exactSingleMatchOnly
      (initIntState utxoEmpty ())
      (Gen.test Gen.defTestParams)

    evaluatePolicy
      (prefix </> "trivialOff")
      (BinSize 10)
      (Policy.random PrivacyModeOff)
      (initIntState utxoEmpty ())
      (Gen.trivial 1000 100 1000)
    evaluatePolicy
      (prefix </> "trivialOn")
      (BinSize 10)
      (Policy.random PrivacyModeOn)
      (initIntState utxoEmpty ())
      (Gen.trivial 1000 100 1000)



{-

input selection
coin selection

bitcoinj coin selection? ("multiple classes" -- multiple policies)

https://github.com/bitcoin/bitcoin/issues/7664

See ApproximateBestSubset in wallet.cpp.

sweep?



-}


{-
http://murch.one/wp-content/uploads/2016/11/erhardt2016coinselection.pdf
"An Evaluation of Coin Selection Strategies", Master’s Thesis, Mark Erhardt

2.3.4
A transaction output is labeled as dust when its value is similar to the cost of
spending it.Precisely, Bitcoin Core sets the dust limit to a value where spending an
2.3. Transactions 7
output would exceed 1/3 of its value. T

https://youtu.be/vnHQwYxB08Y?t=39m


https://www.youtube.com/watch?v=IKSSWUBqMCM

companies; hundreds of entries in UTxO
individuals: tens of entries

batch payments!
  (Harding, BitcoinTechTalk -- safe up to 80% of transaction fees)

coin selection --
  relatively minor importance
    batching, better representation (SegWit), .. much larger impact
    coin selection only a few percent savings

* FIFO is actually a reasonable strategy (!)
* So is random
    self correcting -- if you have a large amount of small inputs,
    they'll be more likely to be picked!
    (i.e, if 90% of the wallet is small inputs, 90% change of picking them!)

Branch&Bound seems to do exhaustive search (backtracking algorithm) to find
exact matches, coupled with random selection.

A Traceability Analysis of Monero’s Blockchain
https://eprint.iacr.org/2017/338.pdf
-}

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance Buildable IntStats where
  build IntStats = bprint "<TODO: statistics>"
