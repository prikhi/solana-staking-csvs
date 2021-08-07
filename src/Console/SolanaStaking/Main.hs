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
                                                , forM_
                                                , unless
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , runExceptT
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , liftIO
                                                , runReaderT
                                                )
import           Data.Bifunctor                 ( second )
import           Data.List                      ( sortOn )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , argPos
                                                , cmdArgs
                                                , def
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
import           Paths_solana_staking_csvs      ( version )

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


-- | Pull staking rewards data for the account & print a CSV to stdout.
--
-- TODO: concurrent/pooled requests, but w/ 100 req/s limit
run :: Args -> IO ()
run Args {..} = either (error . show) return <=< runner $ do
    -- Grab all stake accounts
    stakes <- saResults <$> (getAccountStakes >>= raiseAPIError)
    -- Grab rewards for each stake account
    (stakeErrors, stakeRewards) <-
        fmap (foldr (\(e_, r_) (e, r) -> (e_ <> e, r_ <> r)) ([], []))
        . forM stakes
        $ \sa -> do
              second (map (sa, )) <$> getAllStakeRewards (saPubKey sa)
    unless (null stakeErrors) . liftIO $ do
        hPutStrLn stderr "Got errors while fetching stake rewards:"
        mapM_ (hPutStrLn stderr . ("\t" <>) . show) stakeErrors
    -- Build ordered list of [(acc, reward, time)]
    let orderedRewards = sortOn (srTimestamp . snd) stakeRewards
    -- Print out the CSV
    liftIO $ T.putStrLn "time,amount,stakeAccount,epoch"
    forM_ orderedRewards $ \(stakeAccount, reward) ->
        let formattedTime =
                T.pack
                    $ formatTime defaultTimeLocale "%F %T%Z"
                    $ posixSecondsToUTCTime
                    $ srTimestamp reward
            formattedAmount = renderLamports $ srAmount reward
            output          = T.intercalate
                ","
                [ formattedTime
                , formattedAmount
                , fromStakingPubKey (saPubKey stakeAccount)
                , T.pack . show $ srEpoch reward
                ]
        in  liftIO $ T.putStrLn output
  where
    runner :: ReaderT Config (ExceptT APIError IO) a -> IO (Either APIError a)
    runner = runExceptT . flip runReaderT (mkConfig argApiKey argPubKey)


-- | CLI arguments supported by the executable.
data Args = Args
    { argApiKey :: String
    -- ^ Solana Beach API Key
    , argPubKey :: String
    -- ^ Delegator's PubKey.
    }
    deriving (Show, Read, Eq, Data, Typeable)

-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec

argSpec :: Args
argSpec =
    Args { argApiKey = def &= argPos 0 &= typ "API_KEY"
         , argPubKey = def &= argPos 1 &= typ "ACCOUNT_PUBKEY"
         }
        &= summary
               (  "solana-staking-csvs v"
               <> showVersion version
               <> ", Pavan Rikhi 2021"
               )
        &= program "solana-staking-csvs"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of your Solana Staking Rewards."
