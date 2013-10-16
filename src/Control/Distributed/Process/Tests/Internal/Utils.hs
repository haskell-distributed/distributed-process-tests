-- | General testing support.
module Control.Distributed.Process.Tests.Internal.Utils where

#if ! MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , takeMVar
  , putMVar
  )

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable ()
import Test.HUnit (Assertion)
import Test.HUnit.Base (assertBool)

import Network.Transport.TCP

-- these utilities have been cribbed from distributed-process-platform
-- we should really find a way to share them...

mkNode :: String -> IO LocalNode
mkNode port = do
  Right (transport1, _) <- createTransportExposeInternals
                                    "127.0.0.1" port defaultTCPParameters
  newLocalNode transport1 initRemoteTable

-- | A mutable cell containing a test result.
type TestResult a = MVar a

synchronisedAssertion :: Eq a
                      => String
                      -> LocalNode
                      -> a
                      -> (TestResult a -> Process ())
                      -> MVar ()
                      -> Assertion
synchronisedAssertion note localNode expected testProc lock = do
  result <- newEmptyMVar
  _ <- forkProcess localNode $ do
         acquire lock
         finally (testProc result)
                 (release lock)
  assertComplete note result expected
  where acquire lock' = liftIO $ takeMVar lock'
        release lock' = liftIO $ putMVar lock' ()

delayedAssertion :: Eq a
                 => String
                 -> LocalNode
                 -> a
                 -> (TestResult a -> Process ())
                 -> Assertion
delayedAssertion note localNode expected testProc = do
  result <- newEmptyMVar
  _ <- forkProcess localNode $ testProc result
  assertComplete note result expected

assertComplete :: Eq a => String -> MVar a -> a -> IO ()
assertComplete msg mv a = do
  b <- takeMVar mv
  assertBool msg (a == b)

stash :: TestResult a -> a -> Process ()
stash mvar x = liftIO $ putMVar mvar x
