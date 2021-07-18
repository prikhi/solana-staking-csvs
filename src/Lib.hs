{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Currently a very rough, incomplete Solana Beach API along with the
CLI's 'run' function for fetching rewards & printing out a CSV.

-}
module Lib where

import           Control.Monad                  ( forM
                                                , forM_
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , liftIO
                                                , runReaderT
                                                )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , withObject
                                                )
import           Data.List                      ( nub
                                                , sortOn
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Scientific                ( FPFormat(Fixed)
                                                , Scientific
                                                , formatScientific
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , posixSecondsToUTCTime
                                                )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req               ( (/~)
                                                , GET(GET)
                                                , NoReqBody(NoReqBody)
                                                , Scheme(Https)
                                                , Url
                                                , defaultHttpConfig
                                                , header
                                                , https
                                                , jsonResponse
                                                , req
                                                , responseBody
                                                , runReq
                                                )

import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

-- | Pull staking rewards data for the account & print a CSV to stdout.
--
-- TODO: concurrent/pooled requests, but w/ 100 req/s limit
run :: String -> String -> IO ()
run apiKey accountPubKey = runner $ do
    -- Grab all stake accounts
    stakes <- saResults <$> getAccountStakes
    -- Grab rewards for each stake account
    (stakeRewards :: [(StakingAccount, [StakeReward])]) <-
        fmap reverse . forM stakes $ \sa -> do
            (sa, ) <$> getStakeRewards (saPubKey sa)
    -- Build map of Slot->Time
    let slots = nub $ concatMap (map srSlot . snd) stakeRewards
    slotTimes <- fmap M.fromList . forM slots $ \slot -> do
        block <- getBlock slot
        return (slot, bBlockTime block)
    -- Build ordered list of [(acc, reward, time)]
    let
        orderedRewards =
            mapMaybe
                    (\(acc, rw) ->
                        (acc, rw, ) <$> M.lookup (srSlot rw) slotTimes
                    )
                . sortOn (\(_, rw) -> srSlot rw)
                $ concatMap (\(acc, rs) -> map (acc, ) rs) stakeRewards
    -- Print out the CSV
    liftIO $ T.putStrLn "time,amount,stakeAccount,epoch"
    forM_ orderedRewards $ \(stakeAccount, reward, time) ->
        let
            formattedTime =
                T.pack
                    $ formatTime defaultTimeLocale "%F %T%Z"
                    $ posixSecondsToUTCTime time
            formattedAmount = renderLamports $ srAmount reward
            output          = T.intercalate
                ","
                [ formattedTime
                , formattedAmount
                , fromStakingPubKey (saPubKey stakeAccount)
                , T.pack . show $ srEpoch reward
                ]
        in
            liftIO $ T.putStrLn output
  where
    runner :: ReaderT Config IO a -> IO a
    runner = flip runReaderT (mkConfig apiKey accountPubKey)


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
getAccountStakes :: ReaderT Config IO StakingAccounts
getAccountStakes = do
    pubkey <- asks cAccountPubKey
    getReq (baseUrl /~ ("account" :: T.Text) /~ pubkey /~ ("stakes" :: T.Text))

-- | Single Result Page of Staking Accounts Query.
data StakingAccounts = StakingAccounts
    { saResults    :: [StakingAccount]
    -- ^ The returned staking accounts
    , saTotalPages :: Integer
    -- ^ The total number of pages for the Account's PubKey.
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON StakingAccounts where
    parseJSON = withObject "StakingAccounts" $ \o -> do
        saResults    <- o .: "data"
        saTotalPages <- o .: "totalPages"
        return StakingAccounts { .. }

-- | A single Staking Account.
data StakingAccount = StakingAccount
    { saPubKey        :: StakingPubKey
    -- ^ The Staking Accounts PubKey
    , saLamports      :: Lamports
    -- ^ The Balance of the Staking Account
    , saValidatorName :: T.Text
    -- ^ The Name of the Staking Account's Validator
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON StakingAccount where
    parseJSON = withObject "StakingAccount" $ \o -> do
        saPubKey        <- o .: "pubkey" >>= (.: "address")
        saLamports      <- o .: "lamports"
        saValidatorName <-
            o
            .:  "data"
            >>= (.: "stake")
            >>= (.: "delegation")
            >>= (.: "validatorInfo")
            >>= (.: "name")
        return StakingAccount { .. }

-- | A PubKey for a Staking Account.
newtype StakingPubKey =
    StakingPubKey { fromStakingPubKey :: T.Text } deriving (Show, Read, Eq, Generic, FromJSON)

-- | An amount of Lamports, each of which represent 0.000000001 SOL.
newtype Lamports =
    Lamports { fromLamports :: Integer } deriving (Show, Read, Eq, Generic, FromJSON)

-- | Render an amount of 'Lamports' as text, converting it to SOL.
renderLamports :: Lamports -> T.Text
renderLamports =
    T.pack
        . formatScientific Fixed (Just 9)
        . (* 0.000000001)
        . fromInteger
        . fromLamports


-- | Get the staking reards with a staking account's pubkey.
getStakeRewards :: StakingPubKey -> ReaderT Config IO [StakeReward]
getStakeRewards (StakingPubKey stakeAccountPubkey) = do
    getReq
        (  baseUrl
        /~ ("account" :: T.Text)
        /~ stakeAccountPubkey
        /~ ("stake-rewards" :: T.Text)
        )

-- | A Staking Reward Payment.
data StakeReward = StakeReward
    { srEpoch  :: Integer
    -- ^ The Epoch the reward was paid.
    , srSlot   :: Integer
    -- ^ The 'Block' number of the reward.
    , srAmount :: Lamports
    -- ^ The total number of 'Lamports' awarded.
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON StakeReward where
    parseJSON = withObject "StakeReward" $ \o -> do
        srEpoch  <- o .: "epoch"
        srSlot   <- o .: "effectiveSlot"
        srAmount <- o .: "amount"
        return StakeReward { .. }

-- | Get information about a specific block number.
getBlock :: Integer -> ReaderT Config IO Block
getBlock blockNum = do
    getReq (baseUrl /~ ("block" :: T.Text) /~ blockNum)

-- | A single block on the Solana blockchain.
data Block = Block
    { bNumber    :: Integer
    -- ^ The blocks number.
    , bBlockTime :: POSIXTime
    -- ^ The blocks absolute timestamp.
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON Block where
    parseJSON = withObject "Block" $ \o -> do
        bNumber    <- o .: "blocknumber"
        bBlockTime <-
            o
            .:  "blocktime"
            >>= fmap (fromInteger . (truncate @Scientific))
            .   (.: "absolute")
        return Block { .. }


-- | Generic GET request to the Solana Beach API.
getReq :: FromJSON a => Url 'Https -> ReaderT Config IO a
getReq endpoint = do
    apikey <- asks cApiKey
    let authHeader = header "Authorization" $ "Bearer: " <> encodeUtf8 apikey
    responseBody <$> runReq
        defaultHttpConfig
        (req GET endpoint NoReqBody jsonResponse authHeader)
