{-# LANGUAGE LambdaCase #-}
module Main where

import           Lib                            ( run )

import           System.Environment             ( getArgs )

main :: IO ()
main = getArgs >>= \case
    [apiKey, accountPubKey] -> run apiKey accountPubKey
    _ ->
        error
            "solana-staking-csvs: expected 2 arguments - <API_KEY> <ACCOUNT_PUBKEY>"
