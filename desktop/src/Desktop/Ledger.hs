{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Werror #-}

module Desktop.Ledger where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Bits ((.&.))
import Data.Word

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Binary.Builder as Binary
import qualified Data.Binary.Get as Binary
import qualified System.HIDAPI as HID

vendorId :: Word16
vendorId = 0x2c97

getLedgerDeviceInfo :: IO (Maybe HID.DeviceInfo)
getLedgerDeviceInfo = HID.withHIDAPI $ do
  deviceInfos <- HID.enumerate (Just vendorId) Nothing
  pure $ case deviceInfos of
    [] -> Nothing
    d:_ -> Just d

-- Ledger bits

data LedgerStatus
  = LedgerStatus_IncorrectLength
  | LedgerStatus_SecurityStatusNotSatisfied
  | LedgerStatus_InvalidData
  | LedgerStatus_FileNotFound
  | LedgerStatus_IncorrectParameter
  | LedgerStatus_TechnicalProblem Int
  | LedgerStatus_InvalidPin Int
  | LedgerStatus_Ok
  | LedgerStatus_Unknown Int
  deriving (Eq, Ord, Show)

-- Section 27 https://blog.ledger.com/btchip-doc/bitcoin-technical.html
ledgerStatusFromInt :: Int -> LedgerStatus
ledgerStatusFromInt = \case
  0x6700 -> LedgerStatus_IncorrectLength
  0x6982 -> LedgerStatus_SecurityStatusNotSatisfied
  0x6A80 -> LedgerStatus_InvalidData
  0x6A82 -> LedgerStatus_FileNotFound
  0x6B00 -> LedgerStatus_IncorrectParameter
  0x9000 -> LedgerStatus_Ok
  x | x >= 0x6F00 && x <= 0x6FFF -> LedgerStatus_TechnicalProblem $ x .&. 0xFF
    | x >= 0x63C0 && x <= 0x63CF -> LedgerStatus_InvalidPin $ x .&. 0xF
    | otherwise -> LedgerStatus_Unknown x

communicationChannelID :: Word16
communicationChannelID = 0x0101

data CommandTag
  = CommandTag_Ping
  | CommandTag_Apdu
  deriving (Eq, Ord, Show)

commandTagPing :: Word8
commandTagPing = 0x02

commandTagApdu :: Word8
commandTagApdu = 0x05

commandTagFromWord8 :: Word8 -> Maybe CommandTag
commandTagFromWord8 x
  | x == commandTagPing = Just CommandTag_Ping
  | x == commandTagApdu = Just CommandTag_Apdu
  | otherwise = Nothing

commandTagToWord8 :: CommandTag -> Word8
commandTagToWord8 = \case
  CommandTag_Ping -> commandTagPing
  CommandTag_Apdu -> commandTagApdu

-- | See section 26.1
data TransportHeader = TransportHeader
  { _transportHeader_commandTag :: CommandTag
  , _transportHeader_packetSequenceIndex :: Word16
  } deriving (Eq, Show)

data TransportError
  = TransportError_InvalidCommunicationChannelID Word16
  | TransportError_InvalidCommandTag Word8
  | TransportError_ParseError String
  | TransportError_HIDException String
  | TransportError_HIDWriteMissedBytes Int
  deriving (Eq, Show)

runGet :: Monad m => ExceptT TransportError Binary.Get a -> BSL.ByteString -> ExceptT TransportError m a
runGet get bs = case Binary.runGetOrFail (runExceptT get) bs of
  Left (_, _, err) -> throwError $ TransportError_ParseError err
  Right (_, _, a) -> ExceptT $ return a

parseTransport :: Monad m => BSL.ByteString -> ExceptT TransportError m (TransportHeader, BSL.ByteString)
parseTransport = runGet $ do
  rawChannelID <- lift Binary.getWord16be
  when (rawChannelID /= communicationChannelID) $ throwError $
    TransportError_InvalidCommunicationChannelID rawChannelID
  rawCommandTag <- lift Binary.getWord8
  commandTag <- case commandTagFromWord8 rawCommandTag of
    Nothing -> throwError $ TransportError_InvalidCommandTag rawCommandTag
    Just commandTag -> return commandTag
  packetSequenceIndex <- lift Binary.getWord16be
  rest <- lift Binary.getRemainingLazyByteString
  return (TransportHeader
    { _transportHeader_commandTag = commandTag
    , _transportHeader_packetSequenceIndex = packetSequenceIndex
    }, rest)

buildTransportHeader :: TransportHeader -> Binary.Builder
buildTransportHeader th = mconcat
  [ Binary.putWord16be communicationChannelID
  , Binary.singleton $ commandTagToWord8 $ _transportHeader_commandTag th
  , Binary.putWord16be $ _transportHeader_packetSequenceIndex th
  ]

-- | A 'safe' wrapper around HID device aquisition. HID exceptions are caught
-- here.
withDevice :: HID.DeviceInfo -> (HID.Device -> ExceptT TransportError IO a) -> ExceptT TransportError IO a
withDevice deviceInfo f = ExceptT $ handle h $
  bracket (HID.openDeviceInfo deviceInfo) HID.close $ \device ->
    runExceptT $ f device
  where
    h :: HID.HIDAPIException -> IO (Either TransportError a)
    h = return . Left . TransportError_HIDException . show

data HIDWriteResult
  = HIDWriteResult_Ok
  | HIDWriteResult_Missed Int
  deriving (Eq, Show)

-- | Write a bytestring to the device and check we managed to write it all.
deviceWriteBS :: HID.Device -> BSL.ByteString -> ExceptT TransportError IO ()
deviceWriteBS device bs = do
  written <- liftIO $ HID.write device $ BSL.toStrict bs
  let len = fromIntegral $ BSL.length bs
  when (written /= len) $ throwError $ TransportError_HIDWriteMissedBytes $ len - written

data PingResult
  = PingResult_Ok
  | PingResult_Failed
  deriving (Eq, Show)

-- | Ping a ledger device
pingLedger :: HID.DeviceInfo -> IO (Either TransportError PingResult)
pingLedger deviceInfo = runExceptT $ withDevice deviceInfo $ \device -> do
  let header = TransportHeader CommandTag_Ping 0
      payload = BSL.replicate 59 0
  deviceWriteBS device $ Binary.toLazyByteString $ mconcat
    [ buildTransportHeader header
    , Binary.fromLazyByteString payload
    ]
  bs <- liftIO $ HID.read device 64
  (readHeader, readPayload) <- parseTransport $ BSL.fromStrict bs
  return $ if readHeader == header && readPayload == payload
    then PingResult_Ok
    else PingResult_Failed
