{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| Currently a very rough, incomplete Solana Beach API along with the
CLI's 'run' function for fetching rewards & printing out a CSV.

-}
module Lib where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( (<=<)
                                                , forM
                                                , forM_
                                                , unless
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                , runExceptT
                                                )
import           Control.Monad.Reader           ( MonadIO
                                                , MonadReader
                                                , ReaderT
                                                , asks
                                                , liftIO
                                                , runReaderT
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(parseJSON)
                                                , Value(Object)
                                                , withObject
                                                )
import           Data.Bifunctor                 ( second )
import           Data.List                      ( sortOn )
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
getAccountStakes
    :: (MonadIO m, MonadReader Config m) => m (APIResponse StakingAccounts)
getAccountStakes = do
    pubkey <- asks cAccountPubKey
    getReq
        (baseUrl /~ ("account" :: T.Text) /~ pubkey /~ ("stakes" :: T.Text))
        mempty

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
    getReq (baseUrl /~ ("block" :: T.Text) /~ blockNum) mempty

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




data APIResponse a
    = SuccessfulReponse a
    | ProcessingResponse
    | ErrorResponse APIError
    deriving (Show, Read, Eq)

instance FromJSON a => FromJSON (APIResponse a) where
    parseJSON v = case v of
        Object o -> do
            o .:? "err" >>= \case
                Just errMsg -> o .:? "processing" >>= \case
                    Nothing          -> return $ ErrorResponse $ APIError errMsg
                    Just (_ :: Bool) -> return ProcessingResponse
                Nothing -> fmap SuccessfulReponse . parseJSON $ Object o
        _ -> SuccessfulReponse <$> parseJSON v

raiseAPIError :: MonadError APIError m => APIResponse a -> m a
raiseAPIError = \case
    SuccessfulReponse v -> return v
    ProcessingResponse ->
        throwError $ APIError "Request cancelled during processing wait."
    ErrorResponse err -> throwError err


data APIError
    = APIError T.Text
    | RetriesExceeded T.Text
    deriving (Show, Read, Eq, Generic)
