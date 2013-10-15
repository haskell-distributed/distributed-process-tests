-- Run tests using the TCP transport.

module Main where

import TEST_SUITE_MODULE (tests)

import Network.Transport.Test (TestTransport(..))
import Network.Socket (sClose)
import Network.Transport.TCP
  ( createTransportExposeInternals
  , TransportInternals(socketBetween)
  , defaultTCPParameters
  )
import Test.Framework (defaultMain)

import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    Right (transport, internals) <-
      createTransportExposeInternals "127.0.0.1" "8080" defaultTCPParameters
    defaultMain =<< tests TestTransport
      { testTransport = transport
      , testBreakConnection = \addr1 addr2 -> do
          sock <- socketBetween internals addr1 addr2
          sClose sock
          threadDelay 10000
      }
