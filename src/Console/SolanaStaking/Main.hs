{-| This module contains the @main@ function used by the exectuable.

-}
module Console.SolanaStaking.Main
    ( run
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
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Console.SolanaStaking.Api

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


-- | Pull staking rewards data for the account & print a CSV to stdout.
--
-- TODO: concurrent/pooled requests, but w/ 100 req/s limit
run :: String -> String -> IO ()
run apiKey accountPubKey = either (error . show) return <=< runner $ do
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
    runner = runExceptT . flip runReaderT (mkConfig apiKey accountPubKey)
