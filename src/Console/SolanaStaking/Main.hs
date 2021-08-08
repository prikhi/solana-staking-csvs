{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-| This module contains the @main@ function used by the exectuable.

-}
module Console.SolanaStaking.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Control.Monad                  ( (<=<)
                                                , forM
                                                , unless
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , liftIO
                                                , runReaderT
                                                )
import           Data.Bifunctor                 ( bimap
                                                , second
                                                )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , argPos
                                                , cmdArgs
                                                , def
                                                , explicit
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Console.SolanaStaking.Api
import           Console.SolanaStaking.CoinTracking
                                                ( makeCoinTrackingImport )
import           Console.SolanaStaking.Csv      ( makeCsvContents )
import           Paths_solana_staking_csvs      ( version )

import qualified Data.ByteString.Lazy          as LBS


-- | Pull staking rewards data for the account & print a CSV to stdout.
--
-- TODO: concurrent/pooled requests, but w/ 100 req/s limit
run :: Args -> IO ()
run Args {..} = either (error . show) return <=< runner $ do
    -- Grab all stake accounts
    stakes <- saResults <$> (getAccountStakes >>= raiseAPIError)
    -- Grab rewards for each stake account
    (stakeErrors, stakeRewards) <-
        fmap (bimap concat concat . unzip) . forM stakes $ \sa -> do
            second (map (sa, )) <$> getAllStakeRewards (saPubKey sa)
    unless (null stakeErrors) . liftIO $ do
        hPutStrLn stderr "Got errors while fetching stake rewards:"
        mapM_ (hPutStrLn stderr . ("\t" <>) . show) stakeErrors
    -- Write the CSV
    let orderedRewards = sortOn (srTimestamp . snd) stakeRewards
        outputFile     = fromMaybe "-" argOutputFile
    if argCoinTracking
        then liftIO $ makeCoinTrackingImport outputFile orderedRewards
        else do
            let output = makeCsvContents orderedRewards
            if outputFile == "-"
                then liftIO $ LBS.putStr output
                else liftIO $ LBS.writeFile outputFile output
  where
    runner :: ReaderT Config (ExceptT APIError IO) a -> IO (Either APIError a)
    runner = runExceptT . flip runReaderT (mkConfig argApiKey argPubKey)


-- | CLI arguments supported by the executable.
data Args = Args
    { argApiKey       :: String
    -- ^ Solana Beach API Key
    , argPubKey       :: String
    -- ^ Delegator's PubKey.
    , argOutputFile   :: Maybe String
    -- ^ Optional output file. 'Nothing' or @'Just' "-"@ means print to
    -- 'System.IO.stdout'.
    , argCoinTracking :: Bool
    -- ^ Flag to enable writing/printing files formatted for CoinTracking
    -- Imports.
    }
    deriving (Show, Read, Eq, Data, Typeable)

-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec

argSpec :: Args
argSpec =
    Args
            { argApiKey       = def &= argPos 0 &= typ "API_KEY"
            , argPubKey       = def &= argPos 1 &= typ "ACCOUNT_PUBKEY"
            , argOutputFile   =
                Nothing
                &= help "File to write the export to. Default: stdout"
                &= explicit
                &= name "output-file"
                &= name "o"
                &= typ "FILE"
            , argCoinTracking = False
                                &= help "Generate a CoinTracking Import file."
                                &= explicit
                                &= name "cointracking"
            }
        &= summary
               (  "solana-staking-csvs v"
               <> showVersion version
               <> ", Pavan Rikhi 2021"
               )
        &= program "solana-staking-csvs"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of your Solana Staking Rewards."
