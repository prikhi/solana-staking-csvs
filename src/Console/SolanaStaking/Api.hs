{-| Solana Beach API requests & responses.

TODO: Extract into a @solana-beach-api@ package.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Console.SolanaStaking.Api
    ( -- * Configuration
      Config(..)
    , mkConfig
      -- * Requests / Responses
    , APIResponse(..)
    , APIError(..)
    , raiseAPIError
      -- ** Get Stake Accounts
    , getAccountStakes
    , StakingAccounts(..)
    , StakingAccount(..)
      -- ** Get Staking Rewards
    , getAllStakeRewards
    , getStakeRewards
    , StakeReward(..)
      -- ** Get Block
    , getBlock
    , Block(..)
      -- * General API Types
    , Lamports(..)
    , renderLamports
    , StakingPubKey(..)
    ) where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , runExceptT
                                                )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , asks
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(parseJSON)
                                                , Value(Object)
                                                , withObject
                                                )
import           Data.Scientific                ( FPFormat(Fixed)
                                                , Scientific
                                                , formatScientific
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time.Clock.POSIX          ( POSIXTime )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req               ( (/:)
                                                , (/~)
                                                , GET(GET)
                                                , NoReqBody(NoReqBody)
                                                , Option
                                                , Scheme(Https)
                                                , Url
                                                , defaultHttpConfig
                                                , header
                                                , https
                                                , jsonResponse
                                                , queryParam
                                                , renderUrl
                                                , req
                                                , responseBody
                                                , runReq
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import qualified Data.Text                     as T

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
baseUrl = https "api.solanabeach.io" /: "v1"


-- | Get the staking accounts for the 'cAccountPubKey'.
getAccountStakes
    :: (MonadIO m, MonadReader Config m) => m (APIResponse StakingAccounts)
getAccountStakes = do
    pubkey <- asks cAccountPubKey
    getReq (baseUrl /: "account" /~ pubkey /: "stakes") mempty

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


-- | Get the staking rewards with a staking account's pubkey.
getStakeRewards
    :: (MonadIO m, MonadReader Config m)
    => StakingPubKey
    -> Maybe Integer
    -> m (APIResponse [StakeReward])
getStakeRewards (StakingPubKey stakeAccountPubkey) mbEpochCursor = do
    getReq (baseUrl /: "account" /: stakeAccountPubkey /: "stake-rewards")
           (queryParam "cursor" mbEpochCursor)

-- | Get all the staking rewards for the given account.
--
-- The API's @stake-rewards@ route only returns a maximum of 5 rewards, so
-- we have to use the earliest epoch as the @cursor@ in an additional
-- request to see if there are any more rewards.
getAllStakeRewards
    :: forall m
     . (MonadIO m, MonadReader Config m)
    => StakingPubKey
    -> m ([APIError], [StakeReward])
getAllStakeRewards pubkey =
    getStakeRewards pubkey Nothing >>= runExceptT . raiseAPIError >>= go
        ([], [])
  where
    go
        :: ([APIError], [StakeReward])
        -> Either APIError [StakeReward]
        -> m ([APIError], [StakeReward])
    go (errs, rws) = \case
        Left  err     -> return (err : errs, rws)
        Right rewards -> if length rewards < 5
            then return (errs, rewards <> rws)
            else
                let minEpoch = minimum $ map srEpoch rewards
                in  getStakeRewards pubkey (Just minEpoch)
                    >>= runExceptT
                    .   raiseAPIError
                    >>= go (errs, rewards <> rws)



-- | A Staking Reward Payment.
data StakeReward = StakeReward
    { srEpoch     :: Integer
    -- ^ The Epoch the reward was paid.
    , srSlot      :: Integer
    -- ^ The 'Block' number of the reward.
    , srAmount    :: Lamports
    -- ^ The total number of 'Lamports' awarded.
    , srTimestamp :: POSIXTime
    }
    deriving (Show, Read, Eq, Generic)

instance FromJSON StakeReward where
    parseJSON = withObject "StakeReward" $ \o -> do
        srEpoch     <- o .: "epoch"
        srSlot      <- o .: "effectiveSlot"
        srAmount    <- o .: "amount"
        srTimestamp <- o .: "timestamp"
        return StakeReward { .. }

-- | Get information about a specific block number.
getBlock
    :: (MonadIO m, MonadReader Config m) => Integer -> m (APIResponse Block)
getBlock blockNum = do
    getReq (baseUrl /: "block" /~ blockNum) mempty

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
            (o .: "blocktime")
            >>= fmap (fromInteger . (truncate @Scientific))
            .   (.: "absolute")
        return Block { .. }


-- | Generic GET request to the Solana Beach API with up to 5 retries for
-- @ProcessingResponse@.
--
-- Note: Prints to 'stderr' when waiting for request to finish processing.
getReq
    :: forall m a
     . (MonadReader Config m, MonadIO m, FromJSON a)
    => Url 'Https
    -> Option 'Https
    -> m (APIResponse a)
getReq endpoint options = fetchWithRetries 0
  where
    maxRetries :: Integer
    maxRetries = 5
    fetchWithRetries :: Integer -> m (APIResponse a)
    fetchWithRetries retryCount = if retryCount >= maxRetries
        then return $ ErrorResponse $ RetriesExceeded $ renderUrl endpoint
        else do
            apikey <- asks cApiKey
            let authHeader =
                    header "Authorization" $ "Bearer: " <> encodeUtf8 apikey
            respBody <- responseBody <$> runReq
                defaultHttpConfig
                (req GET endpoint NoReqBody jsonResponse $ authHeader <> options
                )
            case respBody of
                ProcessingResponse -> do
                    liftIO $ hPutStrLn
                        stderr
                        "Waiting for API to finish processing request..."
                    liftIO $ threadDelay $ 10 * 1000000
                    fetchWithRetries (retryCount + 1)
                _ -> return respBody




-- | Wrapper around error & processing responses from the API.
data APIResponse a
    = SuccessfulReponse a
    | ProcessingResponse
    | ErrorResponse APIError
    deriving (Show, Read, Eq)

-- | Attempts to parse a processing response, then an error response,
-- & finally the inner @a@ response.
instance FromJSON a => FromJSON (APIResponse a) where
    parseJSON v = case v of
        Object o -> do
            o .:? "err" >>= \case
                Just errMsg -> o .:? "processing" >>= \case
                    Nothing          -> return $ ErrorResponse $ APIError errMsg
                    Just (_ :: Bool) -> return ProcessingResponse
                Nothing -> fmap SuccessfulReponse . parseJSON $ Object o
        _ -> SuccessfulReponse <$> parseJSON v

-- | Pull the inner value out of an 'APIResponse' or throw the respective
-- 'APIError'.
raiseAPIError :: MonadError APIError m => APIResponse a -> m a
raiseAPIError = \case
    SuccessfulReponse v -> return v
    ProcessingResponse ->
        throwError $ APIError "Request cancelled during processing wait."
    ErrorResponse err -> throwError err


-- | Potential error responses from the Solana Beach API.
data APIError
    = APIError T.Text
    -- ^ Generic API error with message.
    | RetriesExceeded T.Text
    -- ^ Exceeded maximum number of 'ProcessingResponse' retries.
    deriving (Show, Read, Eq, Generic)
