{-# LANGUAGE RecordWildCards #-}
{-| Currently a very rough, incomplete Solana Beach API along with the
CLI's 'run' function for fetching rewards & printing out a CSV.

-}
module Lib where

import           Control.Lens
import           Control.Monad                  ( forM
                                                , forM_
                                                , join
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , liftIO
                                                , runReaderT
                                                )
import           Data.Aeson                     ( FromJSON
                                                , Result(..)
                                                , Value
                                                , fromJSON
                                                )
import           Data.Aeson.Lens
import           Data.Maybe                     ( mapMaybe )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Network.HTTP.Req

import           Data.Scientific                ( FPFormat(Fixed)
                                                , formatScientific
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Vector                   as V

-- | Pull staking rewards data for the account & print a CSV to stdout.
--
-- TODO: concurrent/pooled requests, but w/ 100 req/s limit
run :: String -> String -> IO ()
run apiKey accountPubKey = runner $ do
    -- Grab all stake accounts
    Success stakes <- getAccountStakes
    let stakeAddrs =
            stakes
                ^.. key "data"
                .   values
                .   key "pubkey"
                .   key "address"
                .   _String
    -- Grab rewards for each stake account
    stakeRewards <-
        fmap (mconcat . mapMaybe resultMaybe)
        . forM stakeAddrs
        $ \stakeAddress -> do
              liftIO . putStrLn $ T.unpack stakeAddress
              fmap (^. _Array) <$> getStakeRewards stakeAddress
    -- Find blocktime for each reward
    rewardData <- fmap V.catMaybes . forM stakeRewards $ \reward -> do
        let mbSlot :: Maybe Integer
            mbSlot   = truncate <$> (reward ^? key "effectiveSlot" . _Number)
            mbAmount = reward ^? key "amount" . _Number
        mbBlock <- mapM (fmap resultMaybe . getBlock) mbSlot
        let mbTimestamp =
                join mbBlock >>= (^? key "blocktime" . key "absolute" . _Number)
            mbBlockTime =
                posixSecondsToUTCTime . fromInteger . truncate <$> mbTimestamp
        return $ (,) <$> mbBlockTime <*> mbAmount
    -- Print out the CSV
    liftIO $ T.putStrLn "time,amount"
    forM_ rewardData $ \(time, amount) ->
        let
            formattedTime = formatTime defaultTimeLocale "%F %T%Z" time
            formattedAmount =
                formatScientific Fixed (Just 9) $ amount * 0.000000001
            output =
                T.intercalate "," $ map T.pack [formattedTime, formattedAmount]
        in
            liftIO $ T.putStrLn output
  where
    runner :: ReaderT Config IO a -> IO a
    runner = flip runReaderT (mkConfig apiKey accountPubKey)
    resultMaybe :: Result a -> Maybe a
    resultMaybe = \case
        Error   _ -> Nothing
        Success v -> Just v


-- API

-- | Solana Beach API Configuration
data Config = Config
    { cApiKey        :: T.Text
    -- ^ Your API Key.
    -- Get one here: https://github.com/solana-beach/api
    , cAccountPubKey :: T.Text
    -- ^ TODO: probably drop this when solana-beach-api is extracted to
    -- separate package.
    }
    deriving (Show, Read, Eq)

-- | Create a program config from the API key & the target account's
-- pubkey.
mkConfig :: String -> String -> Config
mkConfig (T.pack -> cApiKey) (T.pack -> cAccountPubKey) = Config { .. }

-- | Base URL to Solana Beach's API
baseUrl :: Url 'Https
baseUrl = https "api.solanabeach.io" /~ ("v1" :: T.Text)


-- TODO: All the following functions should return discrete types instead
-- of generic JSON values.

-- | Get the staking accounts for the 'cAccountPubKey'.
getAccountStakes :: ReaderT Config IO (Result Value)
getAccountStakes = do
    pubkey <- asks cAccountPubKey
    getReq (baseUrl /~ ("account" :: T.Text) /~ pubkey /~ ("stakes" :: T.Text))

-- | Get the staking reards with a staking account's pubkey.
getStakeRewards :: T.Text -> ReaderT Config IO (Result Value)
getStakeRewards stakeAccountPubkey = do
    getReq
        (  baseUrl
        /~ ("account" :: T.Text)
        /~ stakeAccountPubkey
        /~ ("stake-rewards" :: T.Text)
        )

-- | Get information about a specific block number.
getBlock :: Integer -> ReaderT Config IO (Result Value)
getBlock blockNum = do
    getReq (baseUrl /~ ("block" :: T.Text) /~ blockNum)


-- | Generic GET request to the Solana Beach API.
getReq :: FromJSON a => Url 'Https -> ReaderT Config IO (Result a)
getReq endpoint = do
    apikey <- asks cApiKey
    let authHeader = header "Authorization" $ "Bearer: " <> encodeUtf8 apikey
    fromJSON . responseBody <$> runReq
        defaultHttpConfig
        (req GET endpoint NoReqBody (jsonResponse @Value) authHeader)
