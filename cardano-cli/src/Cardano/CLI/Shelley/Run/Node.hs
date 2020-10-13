module Cardano.CLI.Shelley.Run.Node
  ( ShelleyNodeCmdError
  , renderShelleyNodeCmdError
  , runNodeCmd
  ) where

import           Cardano.Api.TextView (TextViewDescription (..))
import           Cardano.Api.Typed
import           Cardano.CLI.Shelley.Commands
import           Cardano.CLI.Shelley.Key (InputDecodeError, OutputDirection (..), OutputFormat (..),
                     VerificationKeyOrFile, readSigningKeyFileAnyOf, readVerificationKeyOrFile,
                     serialiseInputAndWrite)
import           Cardano.CLI.Types (SigningKeyFile (..), VerificationKeyFile (..))
import           Cardano.Prelude
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither, newExceptT)
import           Prelude (id)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

{- HLINT ignore "Reduce duplication" -}

data ShelleyNodeCmdError
  = ShelleyNodeCmdReadFileError !(FileError TextEnvelopeError)
  | ShelleyNodeCmdReadKeyFileError !(FileError InputDecodeError)
  | ShelleyNodeCmdWriteFileError !(FileError ())
  | ShelleyNodeCmdOperationalCertificateIssueError !OperationalCertIssueError
  deriving Show

renderShelleyNodeCmdError :: ShelleyNodeCmdError -> Text
renderShelleyNodeCmdError err =
  case err of
    ShelleyNodeCmdReadFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdReadKeyFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdWriteFileError fileErr -> Text.pack (displayError fileErr)

    ShelleyNodeCmdOperationalCertificateIssueError issueErr ->
      Text.pack (displayError issueErr)


runNodeCmd :: NodeCmd -> ExceptT ShelleyNodeCmdError IO ()
runNodeCmd (NodeKeyGenCold ofo vk sk ctr) = runNodeKeyGenCold ofo vk sk ctr
runNodeCmd (NodeKeyGenKES  ofo vk sk)     = runNodeKeyGenKES  ofo vk sk
runNodeCmd (NodeKeyGenVRF  ofo vk sk)     = runNodeKeyGenVRF  ofo vk sk
runNodeCmd (NodeKeyHashVRF vk mOutFp) = runNodeKeyHashVRF vk mOutFp
runNodeCmd (NodeNewCounter vk ctr out) = runNodeNewCounter vk ctr out
runNodeCmd (NodeIssueOpCert vk sk ctr p out) =
  runNodeIssueOpCert vk sk ctr p out



--
-- Node command implementations
--

runNodeKeyGenCold :: OutputFormatOption
                  -> VerificationKeyFile
                  -> SigningKeyFile
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenCold outFmtOpt (VerificationKeyFile vkeyPath)
                  (SigningKeyFile skeyPath)
                  (OpCertCounterFile ocertCtrPath) = do
    skey <- liftIO $ generateSigningKey AsStakePoolKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ serialiseInputAndWrite
          (toTypedOutputFormat (AsSigningKey AsStakePoolKey) outFmtOpt)
          (OutputDirectionFile skeyPath)
          skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ serialiseInputAndWrite
          (toTypedOutputFormat (AsVerificationKey AsStakePoolKey) outFmtOpt)
          (OutputDirectionFile vkeyPath)
          vkey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope ocertCtrPath (Just ocertCtrDesc)
      $ OperationalCertificateIssueCounter initialCounter vkey
  where
    -- TODO @intricate: Don't forget to use these.
    _vkeyDesc, ocertCtrDesc :: TextViewDescription
    _skeyDesc = TextViewDescription "Stake Pool Operator Signing Key"
    _vkeyDesc = TextViewDescription "Stake Pool Operator Verification Key"
    ocertCtrDesc = TextViewDescription $ "Next certificate issue number: " <> BS.pack (show initialCounter)

    initialCounter :: Word64
    initialCounter = 0


runNodeKeyGenKES :: OutputFormatOption
                 -> VerificationKeyFile
                 -> SigningKeyFile
                 -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenKES outFmtOpt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsKesKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ serialiseInputAndWrite
          (toTypedOutputFormat (AsSigningKey AsKesKey) outFmtOpt)
          (OutputDirectionFile skeyPath)
          skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ serialiseInputAndWrite
          (toTypedOutputFormat (AsVerificationKey AsKesKey) outFmtOpt)
          (OutputDirectionFile vkeyPath)
          vkey
  where
    -- TODO @intricate: Don't forget to use these.
    _skeyDesc, _vkeyDesc :: TextViewDescription
    _skeyDesc = TextViewDescription "KES Signing Key"
    _vkeyDesc = TextViewDescription "KES Verification Key"


runNodeKeyGenVRF :: OutputFormatOption -> VerificationKeyFile
                 -> SigningKeyFile -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyGenVRF outFmtOpt (VerificationKeyFile vkeyPath) (SigningKeyFile skeyPath) = do
    skey <- liftIO $ generateSigningKey AsVrfKey
    let vkey = getVerificationKey skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ serialiseInputAndWrite
          (toTypedOutputFormat (AsSigningKey AsVrfKey) outFmtOpt)
          (OutputDirectionFile skeyPath)
          skey
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ serialiseInputAndWrite
          (toTypedOutputFormat (AsVerificationKey AsVrfKey) outFmtOpt)
          (OutputDirectionFile vkeyPath)
          vkey
  where
    -- TODO @intricate: Don't forget to use these.
    _skeyDesc, _vkeyDesc :: TextViewDescription
    _skeyDesc = TextViewDescription "VRF Signing Key"
    _vkeyDesc = TextViewDescription "VRF Verification Key"

runNodeKeyHashVRF :: VerificationKeyOrFile VrfKey
                  -> Maybe OutputFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeKeyHashVRF verKeyOrFile mOutputFp = do
  vkey <- firstExceptT ShelleyNodeCmdReadKeyFileError
    . newExceptT
    $ readVerificationKeyOrFile AsVrfKey verKeyOrFile

  let hexKeyHash = serialiseToRawBytesHex (verificationKeyHash vkey)

  case mOutputFp of
    Just (OutputFile fpath) -> liftIO $ BS.writeFile fpath hexKeyHash
    Nothing -> liftIO $ BS.putStrLn hexKeyHash


runNodeNewCounter :: ColdVerificationKeyOrFile
                  -> Word
                  -> OpCertCounterFile
                  -> ExceptT ShelleyNodeCmdError IO ()
runNodeNewCounter coldVerKeyOrFile counter
                  (OpCertCounterFile ocertCtrPath) = do

    vkey <- firstExceptT ShelleyNodeCmdReadFileError . newExceptT $
      readColdVerificationKeyOrFile coldVerKeyOrFile

    let ocertIssueCounter =
          OperationalCertificateIssueCounter (fromIntegral counter) vkey

    firstExceptT ShelleyNodeCmdWriteFileError . newExceptT $
      writeFileTextEnvelope ocertCtrPath Nothing ocertIssueCounter


runNodeIssueOpCert :: VerificationKeyOrFile KesKey
                   -- ^ This is the hot KES verification key.
                   -> SigningKeyFile
                   -- ^ This is the cold signing key.
                   -> OpCertCounterFile
                   -- ^ Counter that establishes the precedence
                   -- of the operational certificate.
                   -> KESPeriod
                   -- ^ Start of the validity period for this certificate.
                   -> OutputFile
                   -> ExceptT ShelleyNodeCmdError IO ()
runNodeIssueOpCert kesVerKeyOrFile
                   stakePoolSKeyFile
                   (OpCertCounterFile ocertCtrPath)
                   kesPeriod
                   (OutputFile certFile) = do

    ocertIssueCounter <- firstExceptT ShelleyNodeCmdReadFileError
      . newExceptT
      $ readFileTextEnvelope AsOperationalCertificateIssueCounter ocertCtrPath

    verKeyKes <- firstExceptT ShelleyNodeCmdReadKeyFileError
      . newExceptT
      $ readVerificationKeyOrFile AsKesKey kesVerKeyOrFile

    signKey <- firstExceptT ShelleyNodeCmdReadKeyFileError
      . newExceptT
      $ readSigningKeyFileAnyOf
          bech32PossibleBlockIssuers
          textEnvPossibleBlockIssuers
          stakePoolSKeyFile

    (ocert, nextOcertCtr) <-
      firstExceptT ShelleyNodeCmdOperationalCertificateIssueError
        . hoistEither
        $ issueOperationalCertificate
            verKeyKes
            signKey
            kesPeriod
            ocertIssueCounter

    -- Write the counter first, to reduce the chance of ending up with
    -- a new cert but without updating the counter.
    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope
        ocertCtrPath
        (Just $ ocertCtrDesc $ getCounter nextOcertCtr)
        nextOcertCtr

    firstExceptT ShelleyNodeCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope certFile Nothing ocert
  where
    getCounter :: OperationalCertificateIssueCounter -> Word64
    getCounter (OperationalCertificateIssueCounter n _) = n

    ocertCtrDesc :: Word64 -> TextViewDescription
    ocertCtrDesc n = TextViewDescription $ "Next certificate issue number: " <> BS.pack (show n)

    textEnvPossibleBlockIssuers
      :: [FromSomeType HasTextEnvelope
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    textEnvPossibleBlockIssuers =
      [ FromSomeType (AsSigningKey AsStakePoolKey)        Left
      , FromSomeType (AsSigningKey AsGenesisDelegateKey) (Left . castSigningKey)
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) Right
      ]

    bech32PossibleBlockIssuers
      :: [FromSomeType SerialiseAsBech32
                       (Either (SigningKey StakePoolKey)
                               (SigningKey GenesisDelegateExtendedKey))]
    bech32PossibleBlockIssuers =
      [FromSomeType (AsSigningKey AsStakePoolKey) Left]

-- | Read a cold verification key or file.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readColdVerificationKeyOrFile
  :: ColdVerificationKeyOrFile
  -> IO (Either (FileError TextEnvelopeError) (VerificationKey StakePoolKey))
readColdVerificationKeyOrFile coldVerKeyOrFile =
  case coldVerKeyOrFile of
    ColdStakePoolVerificationKey vk -> pure (Right vk)
    ColdGenesisDelegateVerificationKey vk ->
      pure $ Right (castVerificationKey vk)
    ColdVerificationKeyFile (VerificationKeyFile fp) ->
      readFileTextEnvelopeAnyOf
        [ FromSomeType (AsVerificationKey AsStakePoolKey) id
        , FromSomeType (AsVerificationKey AsGenesisDelegateKey) castVerificationKey
        ]
        fp

toTypedOutputFormat
  :: (SerialiseAsBech32 a, HasTextEnvelope a)
  => AsType a
  -> OutputFormatOption
  -> OutputFormat a
toTypedOutputFormat _asType outFmtOpt =
  case outFmtOpt of
    OutputFormatOptionBech32 -> OutputFormatBech32
    OutputFormatOptionHex -> OutputFormatHex
    OutputFormatOptionTextEnvelope -> OutputFormatTextEnvelope
