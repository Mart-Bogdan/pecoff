-- | Parses a ByteString into a Pecoff record. Parsing of section data currently
-- left as a todo.
module Data.Pecoff ( Pecoff(..)
                   , PecoffSection(..)
                   , IMAGE_SUBSYSTEM(..)
                   , IMAGE_SCN_CHARACTERISTICS(..)
                   , IMAGE_DLL_CHARACTERISTICS(..)
                   , IMAGE_FILE_CHARACTERISTICS(..)
                   , IMAGE_FILE_MACHINE(..)
                   , parsePecoff
                   ) where

import Data.Char
import Data.Bits
import Data.Word
import Data.Binary.Get
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
    
data IMAGE_SUBSYSTEM
    = IMAGE_SUBSYSTEM_UNKNOWN
    | IMAGE_SUBSYSTEM_NATIVE
    | IMAGE_SUBSYSTEM_WINDOWS_GUI
    | IMAGE_SUBSYSTEM_WINDOWS_CUI
    | IMAGE_SUBSYSTEM_OS2_CUI
    | IMAGE_SUBSYSTEM_POSIX_CUI
    | IMAGE_SUBSYSTEM_NATIVE_WINDOWS
    | IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
    | IMAGE_SUBSYSTEM_EFI_APPLICATION
    | IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
    | IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
    | IMAGE_SUBSYSTEM_EFI_ROM
    | IMAGE_SUBSYSTEM_XBOX
    deriving (Show, Eq)
imageSubsystem  0 = IMAGE_SUBSYSTEM_UNKNOWN
imageSubsystem  1 = IMAGE_SUBSYSTEM_NATIVE
imageSubsystem  2 = IMAGE_SUBSYSTEM_WINDOWS_GUI
imageSubsystem  3 = IMAGE_SUBSYSTEM_WINDOWS_CUI
imageSubsystem  5 = IMAGE_SUBSYSTEM_OS2_CUI
imageSubsystem  7 = IMAGE_SUBSYSTEM_POSIX_CUI
imageSubsystem  8 = IMAGE_SUBSYSTEM_NATIVE_WINDOWS
imageSubsystem  9 = IMAGE_SUBSYSTEM_WINDOWS_CE_GUI
imageSubsystem 10 = IMAGE_SUBSYSTEM_EFI_APPLICATION
imageSubsystem 11 = IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER
imageSubsystem 12 = IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
imageSubsystem 13 = IMAGE_SUBSYSTEM_EFI_ROM
imageSubsystem 14 = IMAGE_SUBSYSTEM_XBOX
imageSubsystem n = error ("hrm " ++ show n)

data IMAGE_SCN_CHARACTERISTICS
    = IMAGE_SCN_TYPE_NO_PAD
    | IMAGE_SCN_CNT_CODE
    | IMAGE_SCN_CNT_INITIALIZED_DATA
    | IMAGE_SCN_CNT_UNINITIALIZED_DATA
    | IMAGE_SCN_LNK_OTHER
    | IMAGE_SCN_LNK_INFO
    | IMAGE_SCN_LNK_REMOVE
    | IMAGE_SCN_LNK_COMDAT
    | IMAGE_SCN_GPREL
    | IMAGE_SCN_MEM_PURGEABLE
    | IMAGE_SCN_MEM_16BIT
    | IMAGE_SCN_MEM_LOCKED
    | IMAGE_SCN_MEM_PRELOAD
    | IMAGE_SCN_ALIGN_1BYTES
    | IMAGE_SCN_ALIGN_2BYTES
    | IMAGE_SCN_ALIGN_4BYTES
    | IMAGE_SCN_ALIGN_8BYTES
    | IMAGE_SCN_ALIGN_16BYTES
    | IMAGE_SCN_ALIGN_32BYTES
    | IMAGE_SCN_ALIGN_64BYTES
    | IMAGE_SCN_ALIGN_128BYTES
    | IMAGE_SCN_ALIGN_256BYTES
    | IMAGE_SCN_ALIGN_512BYTES
    | IMAGE_SCN_ALIGN_1024BYTES
    | IMAGE_SCN_ALIGN_2048BYTES
    | IMAGE_SCN_ALIGN_4096BYTES
    | IMAGE_SCN_ALIGN_8192BYTES
    | IMAGE_SCN_LNK_NRELOC_OVFL
    | IMAGE_SCN_MEM_DISCARDABLE
    | IMAGE_SCN_MEM_NOT_CACHED
    | IMAGE_SCN_MEM_NOT_PAGED
    | IMAGE_SCN_MEM_SHARED
    | IMAGE_SCN_MEM_EXECUTE
    | IMAGE_SCN_MEM_READ
    | IMAGE_SCN_MEM_WRITE
    deriving (Show, Eq)
imageScnCharacteristics n = imageScnAlign_ ((n .&. 0x00f00000) `shiftR` 20) ++ imageScnCharacteristics_ n 32
                          where imageScnAlign_ 0x0 = []
                                imageScnAlign_ 0x1 = [IMAGE_SCN_ALIGN_1BYTES]
                                imageScnAlign_ 0x2 = [IMAGE_SCN_ALIGN_2BYTES]
                                imageScnAlign_ 0x3 = [IMAGE_SCN_ALIGN_4BYTES]
                                imageScnAlign_ 0x4 = [IMAGE_SCN_ALIGN_8BYTES]
                                imageScnAlign_ 0x5 = [IMAGE_SCN_ALIGN_16BYTES]
                                imageScnAlign_ 0x6 = [IMAGE_SCN_ALIGN_32BYTES]
                                imageScnAlign_ 0x7 = [IMAGE_SCN_ALIGN_64BYTES]
                                imageScnAlign_ 0x8 = [IMAGE_SCN_ALIGN_128BYTES]
                                imageScnAlign_ 0x9 = [IMAGE_SCN_ALIGN_256BYTES]
                                imageScnAlign_ 0xa = [IMAGE_SCN_ALIGN_512BYTES]
                                imageScnAlign_ 0xb = [IMAGE_SCN_ALIGN_1024BYTES]
                                imageScnAlign_ 0xc = [IMAGE_SCN_ALIGN_2048BYTES]
                                imageScnAlign_ 0xd = [IMAGE_SCN_ALIGN_4096BYTES]
                                imageScnAlign_ 0xe = [IMAGE_SCN_ALIGN_8192BYTES]
                                imageScnCharacteristics_ n  0 = []
                                imageScnCharacteristics_ n  4 | testBit n  3 = IMAGE_SCN_TYPE_NO_PAD                         : imageScnCharacteristics_ n  3
                                imageScnCharacteristics_ n  6 | testBit n  5 = IMAGE_SCN_CNT_CODE                            : imageScnCharacteristics_ n  5
                                imageScnCharacteristics_ n  7 | testBit n  6 = IMAGE_SCN_CNT_INITIALIZED_DATA                : imageScnCharacteristics_ n  6
                                imageScnCharacteristics_ n  8 | testBit n  7 = IMAGE_SCN_CNT_UNINITIALIZED_DATA              : imageScnCharacteristics_ n  7
                                imageScnCharacteristics_ n  9 | testBit n  8 = IMAGE_SCN_LNK_OTHER                           : imageScnCharacteristics_ n  8
                                imageScnCharacteristics_ n 10 | testBit n  9 = IMAGE_SCN_LNK_INFO                            : imageScnCharacteristics_ n  9
                                imageScnCharacteristics_ n 12 | testBit n 11 = IMAGE_SCN_LNK_REMOVE                          : imageScnCharacteristics_ n 11
                                imageScnCharacteristics_ n 13 | testBit n 12 = IMAGE_SCN_LNK_COMDAT                          : imageScnCharacteristics_ n 12
                                imageScnCharacteristics_ n 16 | testBit n 15 = IMAGE_SCN_GPREL                               : imageScnCharacteristics_ n 15
                                imageScnCharacteristics_ n 18 | testBit n 17 = IMAGE_SCN_MEM_PURGEABLE : IMAGE_SCN_MEM_16BIT : imageScnCharacteristics_ n 17
                                imageScnCharacteristics_ n 19 | testBit n 18 = IMAGE_SCN_MEM_LOCKED                          : imageScnCharacteristics_ n 18
                                imageScnCharacteristics_ n 20 | testBit n 19 = IMAGE_SCN_MEM_PRELOAD                         : imageScnCharacteristics_ n 19
                                imageScnCharacteristics_ n 25 | testBit n 24 = IMAGE_SCN_LNK_NRELOC_OVFL                     : imageScnCharacteristics_ n 24
                                imageScnCharacteristics_ n 26 | testBit n 25 = IMAGE_SCN_MEM_DISCARDABLE                     : imageScnCharacteristics_ n 25
                                imageScnCharacteristics_ n 27 | testBit n 26 = IMAGE_SCN_MEM_NOT_CACHED                      : imageScnCharacteristics_ n 26
                                imageScnCharacteristics_ n 28 | testBit n 27 = IMAGE_SCN_MEM_NOT_PAGED                       : imageScnCharacteristics_ n 27
                                imageScnCharacteristics_ n 29 | testBit n 28 = IMAGE_SCN_MEM_SHARED                          : imageScnCharacteristics_ n 28
                                imageScnCharacteristics_ n 30 | testBit n 29 = IMAGE_SCN_MEM_EXECUTE                         : imageScnCharacteristics_ n 29
                                imageScnCharacteristics_ n 31 | testBit n 30 = IMAGE_SCN_MEM_READ                            : imageScnCharacteristics_ n 30
                                imageScnCharacteristics_ n 32 | testBit n 31 = IMAGE_SCN_MEM_WRITE                           : imageScnCharacteristics_ n 31
                                imageScnCharacteristics_ n i = imageScnCharacteristics_ n (i-1)

data IMAGE_DLL_CHARACTERISTICS
    = IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE
    | IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY
    | IMAGE_DLL_CHARACTERISTICS_NX_COMPAT
    | IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION
    | IMAGE_DLL_CHARACTERISTICS_NO_SEH
    | IMAGE_DLL_CHARACTERISTICS_NO_BIND
    | IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER
    | IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE
    deriving (Show, Eq)
imageDllCharacteristics n = imageDllCharacteristics_ n 16
                            where imageDllCharacteristics_ n  0 = []
                                  imageDllCharacteristics_ n  7 | testBit n  6 = IMAGE_DLL_CHARACTERISTICS_DYNAMIC_BASE          : imageDllCharacteristics_ n  6
                                  imageDllCharacteristics_ n  8 | testBit n  7 = IMAGE_DLL_CHARACTERISTICS_FORCE_INTEGRITY       : imageDllCharacteristics_ n  7
                                  imageDllCharacteristics_ n  9 | testBit n  8 = IMAGE_DLL_CHARACTERISTICS_NX_COMPAT             : imageDllCharacteristics_ n  8
                                  imageDllCharacteristics_ n 10 | testBit n  9 = IMAGE_DLL_CHARACTERISTICS_NO_ISOLATION          : imageDllCharacteristics_ n  9
                                  imageDllCharacteristics_ n 11 | testBit n 10 = IMAGE_DLL_CHARACTERISTICS_NO_SEH                : imageDllCharacteristics_ n 10
                                  imageDllCharacteristics_ n 12 | testBit n 11 = IMAGE_DLL_CHARACTERISTICS_NO_BIND               : imageDllCharacteristics_ n 11
                                  imageDllCharacteristics_ n 14 | testBit n 13 = IMAGE_DLL_CHARACTERISTICS_WDM_DRIVER            : imageDllCharacteristics_ n 13
                                  imageDllCharacteristics_ n 16 | testBit n 15 = IMAGE_DLL_CHARACTERISTICS_TERMINAL_SERVER_AWARE : imageDllCharacteristics_ n 15
                                  imageDllCharacteristics_ n i = imageDllCharacteristics_ n (i-1)

data IMAGE_FILE_CHARACTERISTICS
    = IMAGE_FILE_RELOCS_STRIPPED
    | IMAGE_FILE_EXECUTABLE_IMAGE
    | IMAGE_FILE_LINE_NUMS_STRIPPED
    | IMAGE_FILE_LOCAL_SYMS_STIRPPED
    | IMAGE_FILE_AGGRESSIVE_WS_TRIM
    | IMAGE_FILE_LARGE_ADDRESS_AWARE
    | IMAGE_FILE_BYTES_REVERSED_LO
    | IMAGE_FILE_32BIT_MACHINE
    | IMAGE_FILE_DEBUG_STRIPPED
    | IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP
    | IMAGE_FILE_NET_RUN_FROM_SWAP
    | IMAGE_FILE_SYSTEM
    | IMAGE_FILE_DLL
    | IMAGE_FILE_UP_SYSTEM_ONLY
    | IMAGE_FILE_BYTES_REVERSED_HI
    deriving (Show, Eq)
imageFileCharacteristics n = imageFileCharacteristics_ n 16
                            where imageFileCharacteristics_ n  0 = []
                                  imageFileCharacteristics_ n  1 | testBit n  0 = IMAGE_FILE_RELOCS_STRIPPED         : imageFileCharacteristics_ n 0
                                  imageFileCharacteristics_ n  2 | testBit n  1 = IMAGE_FILE_EXECUTABLE_IMAGE        : imageFileCharacteristics_ n 1
                                  imageFileCharacteristics_ n  3 | testBit n  2 = IMAGE_FILE_LINE_NUMS_STRIPPED      : imageFileCharacteristics_ n 2
                                  imageFileCharacteristics_ n  4 | testBit n  3 = IMAGE_FILE_LOCAL_SYMS_STIRPPED     : imageFileCharacteristics_ n 3
                                  imageFileCharacteristics_ n  5 | testBit n  4 = IMAGE_FILE_AGGRESSIVE_WS_TRIM      : imageFileCharacteristics_ n 4
                                  imageFileCharacteristics_ n  6 | testBit n  5 = IMAGE_FILE_LARGE_ADDRESS_AWARE     : imageFileCharacteristics_ n 5
                                  imageFileCharacteristics_ n  8 | testBit n  7 = IMAGE_FILE_BYTES_REVERSED_LO       : imageFileCharacteristics_ n 7
                                  imageFileCharacteristics_ n  9 | testBit n  8 = IMAGE_FILE_32BIT_MACHINE           : imageFileCharacteristics_ n 8
                                  imageFileCharacteristics_ n 10 | testBit n  9 = IMAGE_FILE_DEBUG_STRIPPED          : imageFileCharacteristics_ n 9
                                  imageFileCharacteristics_ n 11 | testBit n 10 = IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP : imageFileCharacteristics_ n 10
                                  imageFileCharacteristics_ n 12 | testBit n 11 = IMAGE_FILE_NET_RUN_FROM_SWAP       : imageFileCharacteristics_ n 11
                                  imageFileCharacteristics_ n 13 | testBit n 12 = IMAGE_FILE_SYSTEM                  : imageFileCharacteristics_ n 12
                                  imageFileCharacteristics_ n 14 | testBit n 13 = IMAGE_FILE_DLL                     : imageFileCharacteristics_ n 13
                                  imageFileCharacteristics_ n 15 | testBit n 14 = IMAGE_FILE_UP_SYSTEM_ONLY          : imageFileCharacteristics_ n 14
                                  imageFileCharacteristics_ n 16 | testBit n 15 = IMAGE_FILE_BYTES_REVERSED_HI       : imageFileCharacteristics_ n 15
                                  imageFileCharacteristics_ n i = imageFileCharacteristics_ n (i-1)
 
data IMAGE_FILE_MACHINE
    = IMAGE_FILE_MACHINE_UNKNOWN
    | IMAGE_FILE_MACHINE_AM33
    | IMAGE_FILE_MACHINE_AMD64
    | IMAGE_FILE_MACHINE_ARM
    | IMAGE_FILE_MACHINE_EBC
    | IMAGE_FILE_MACHINE_I386
    | IMAGE_FILE_MACHINE_IA64
    | IMAGE_FILE_MACHINE_M32R
    | IMAGE_FILE_MACHINE_MIPS16
    | IMAGE_FILE_MACHINE_MIPSFPU
    | IMAGE_FILE_MACHINE_MIPSFPU16
    | IMAGE_FILE_MACHINE_POWERPC
    | IMAGE_FILE_MACHINE_POWERPCFP
    | IMAGE_FILE_MACHINE_R4000
    | IMAGE_FILE_MACHINE_SH3
    | IMAGE_FILE_MACHINE_SH3DSP
    | IMAGE_FILE_MACHINE_SH4
    | IMAGE_FILE_MACHINE_SH5
    | IMAGE_FILE_MACHINE_THUMB
    | IMAGE_FILE_MACHINE_WCEMIPSV2
    deriving (Show, Eq)
imageFileMachine 0x0000 = IMAGE_FILE_MACHINE_UNKNOWN
imageFileMachine 0x01d3 = IMAGE_FILE_MACHINE_AM33
imageFileMachine 0x8664 = IMAGE_FILE_MACHINE_AMD64
imageFileMachine 0x01c0 = IMAGE_FILE_MACHINE_ARM
imageFileMachine 0x0ebc = IMAGE_FILE_MACHINE_EBC
imageFileMachine 0x014c = IMAGE_FILE_MACHINE_I386
imageFileMachine 0x0200 = IMAGE_FILE_MACHINE_IA64
imageFileMachine 0x9041 = IMAGE_FILE_MACHINE_M32R
imageFileMachine 0x0266 = IMAGE_FILE_MACHINE_MIPS16
imageFileMachine 0x0366 = IMAGE_FILE_MACHINE_MIPSFPU
imageFileMachine 0x0466 = IMAGE_FILE_MACHINE_MIPSFPU16
imageFileMachine 0x01f0 = IMAGE_FILE_MACHINE_POWERPC
imageFileMachine 0x01f1 = IMAGE_FILE_MACHINE_POWERPCFP
imageFileMachine 0x0166 = IMAGE_FILE_MACHINE_R4000
imageFileMachine 0x01a2 = IMAGE_FILE_MACHINE_SH3
imageFileMachine 0x01a3 = IMAGE_FILE_MACHINE_SH3DSP
imageFileMachine 0x01a6 = IMAGE_FILE_MACHINE_SH4
imageFileMachine 0x01a8 = IMAGE_FILE_MACHINE_SH5
imageFileMachine 0x01c2 = IMAGE_FILE_MACHINE_THUMB
imageFileMachine 0x0169 = IMAGE_FILE_MACHINE_WCEMIPSV2
           
data PecoffReader = PecoffReader
                  { isPE32Plus     :: Bool
                  , getAddress     :: Get Word64
                  }
pecoffReader 0x020b = PecoffReader  True getWord64le
pecoffReader 0x010b = PecoffReader False (liftM fromIntegral getWord32le)
pecoffReader _ = error "Invalid magic number for image file optional header."                         
                   
data Pecoff = Pecoff
            { pMachine             :: IMAGE_FILE_MACHINE           -- ^ Machine type.
            , pFileCharacteristics :: [IMAGE_FILE_CHARACTERISTICS] -- ^ File flags.
            , pEntryPointAddress   :: Word64                       -- ^ Entry point address.
            , pImageBase           :: Word64                       -- ^ Default load base for image.
            , pSubsystem           :: IMAGE_SUBSYSTEM              -- ^ Subsystem required to run this image.
            , pDllCharacteristics  :: [IMAGE_DLL_CHARACTERISTICS]  -- ^ DLL flags.
            , pSections            :: [PecoffSection]              -- ^ Sections contained in this PE/COFF object.
            } deriving (Show, Eq)

getImageFileHeader = do
  machine <- liftM imageFileMachine getWord16le
  numsect <- liftM fromIntegral getWord16le
  tmstamp <- getWord32le
  symtoff <- getWord32le
  symtcnt <- getWord32le
  szopthd <- getWord16le
  attribs <- liftM imageFileCharacteristics getWord16le
  return (machine, numsect, attribs)

getImageFileOptionalHeader = do
  magic                   <- getWord16le
  pr                      <- return $ pecoffReader magic
  majorLinkerVersion      <- getWord8
  minorLinkerVersion      <- getWord8
  sizeOfCode              <- getWord32le
  sizeOfInitializedData   <- getWord32le
  sizeOfUninitializedData <- getWord32le
  addressOfEntryPoint     <- liftM fromIntegral getWord32le
  baseOfCode              <- getWord32le
  baseOfData              <- if isPE32Plus pr then return 0 else getWord32le
  imageBase               <- getAddress pr
  sectionAlignment        <- getWord32le
  fileAlignment           <- getWord32le
  majorOSVersion          <- getWord16le
  minorOSVersion          <- getWord16le
  majorImageVersion       <- getWord16le
  minorImageVersion       <- getWord16le
  majorSubSystemVersion   <- getWord16le
  minorSubSystemVersion   <- getWord16le
  win32VersionValue       <- getWord32le
  sizeOfImage             <- getWord32le
  sizeOfHeaders           <- getWord32le
  checksum                <- getWord32le
  subsystem               <- liftM imageSubsystem $ getWord16le
  dllCharacteristics      <- liftM imageDllCharacteristics $ getWord16le
  sizeOfStackReserve      <- getAddress pr
  sizeOfStackCommit       <- getAddress pr
  sizeOfHeapReserve       <- getAddress pr
  sizeOfHeapCommit        <- getAddress pr
  loaderFlags             <- getWord32le
  numberOfRvaAndSizes     <- liftM fromIntegral $ getWord32le
  imageDataDirectory      <- sequence $ replicate numberOfRvaAndSizes (liftM2 (,) getWord32le getWord32le)
  skip (8 * (16 - numberOfRvaAndSizes))                           
       
  let exportTable           = if length imageDataDirectory >  0 then Just (imageDataDirectory !!  0) else Nothing
      importTable           = if length imageDataDirectory >  1 then Just (imageDataDirectory !!  1) else Nothing
      resourceTable         = if length imageDataDirectory >  2 then Just (imageDataDirectory !!  2) else Nothing
      exceptionTable        = if length imageDataDirectory >  3 then Just (imageDataDirectory !!  3) else Nothing
      certificateTable      = if length imageDataDirectory >  4 then Just (imageDataDirectory !!  4) else Nothing
      baseRelocationTable   = if length imageDataDirectory >  5 then Just (imageDataDirectory !!  5) else Nothing
      debug                 = if length imageDataDirectory >  6 then Just (imageDataDirectory !!  6) else Nothing
      architecture          = if length imageDataDirectory >  7 then Just (imageDataDirectory !!  7) else Nothing
      globalPtr             = if length imageDataDirectory >  8 then Just (imageDataDirectory !!  8) else Nothing
      tlsTable              = if length imageDataDirectory >  9 then Just (imageDataDirectory !!  9) else Nothing
      loadConfigTable       = if length imageDataDirectory > 10 then Just (imageDataDirectory !! 10) else Nothing
      boundImport           = if length imageDataDirectory > 11 then Just (imageDataDirectory !! 11) else Nothing
      iat                   = if length imageDataDirectory > 12 then Just (imageDataDirectory !! 12) else Nothing
      delayImportDescriptor = if length imageDataDirectory > 13 then Just (imageDataDirectory !! 13) else Nothing
      clrRuntimeHeader      = if length imageDataDirectory > 14 then Just (imageDataDirectory !! 14) else Nothing
  return (pr, addressOfEntryPoint, imageBase, subsystem, dllCharacteristics)

getCharUTF8 = do
  let getCharUTF82 b1 = do
        b2 <- liftM fromIntegral getWord8 :: Get Word32
        if b2 .&. 0xc0 == 0x80 then
          return $ ((b1 .&. 0x1f) `shiftL` 6) .|. (b2 .&. 0x3f)
         else
          fail "Invalid second byte in UTf8 string."
      getCharUTF83 b1 = do
        b2 <- liftM fromIntegral getWord8 :: Get Word32
        b3 <- liftM fromIntegral getWord8 :: Get Word32
        if b2 .&. 0xc0 == 0x80 && b3 .&. 0xc0 == 0x80 then
          return $ ((b1 .&. 0x0f) `shiftL` 12) .|. ((b2 .&. 0x3f) `shiftL` 6) .|. (b3 .&. 0x3f)
         else
          fail "Invalid second or third byte in UTf8 string."
      getCharUTF84 b1 = do
        b2 <- liftM fromIntegral getWord8 :: Get Word32
        b3 <- liftM fromIntegral getWord8 :: Get Word32
        b4 <- liftM fromIntegral getWord8 :: Get Word32
        if b2 .&. 0xc0 == 0x80 && b3 .&. 0xc0 == 0x80 && b4 .&. 0xc0 == 0x80 then
          return $ ((b1 .&. 0x07) `shiftL` 18) .|. ((b2 .&. 0x3f) `shiftL` 12) .|. ((b3 .&. 0x3f) `shiftL` 6) .|. (b4 .&. 0x3f)
         else
          fail "Invalid second or third byte in UTf8 string."
  b1 <- liftM fromIntegral getWord8 :: Get Word32
  case b1 of
    n | n .&. 0x80 == 0x00 -> return $ fromIntegral n
    n | n .&. 0xe0 == 0xc0 -> getCharUTF82 n
    n | n .&. 0xf0 == 0xe0 -> getCharUTF83 n
    n | n .&. 0xf8 == 0xf0 -> getCharUTF84 n
    _                      -> fail "Invalid first byte in UTF8 string."

getSectionName = liftM (map (chr . fromIntegral)) getSectionName_
    where getSectionName_ = do
            empty <- isEmpty
            if empty then
                return []
             else do
              char <- getCharUTF8
              if char == 0 then
                  return []
               else do
                rest <- getSectionName_
                return (char : rest)

data PecoffSection = PecoffSection
                   { psectName            :: String                      -- ^ Name of section.
                   , psectVirtualSize     :: Word64                      -- ^ Virtual memory size.
                   , psectVirtualAddress  :: Word64                      -- ^ Virtual memory address.
                   , psectCharacteristics :: [IMAGE_SCN_CHARACTERISTICS] -- ^ Flags.
                   , psectRelocations     :: B.ByteString                -- ^ Raw data for relocations.
                   , psectLinenumbers     :: B.ByteString                -- ^ Raw data for linenumbers.
                   , psectRawData         :: B.ByteString                -- ^ Raw data for section.
                   } deriving (Show, Eq)

getSectionHeader pr bs = do
  full_name            <- getByteString 8
  name                 <- return $ runGet getSectionName $ L.fromChunks[full_name]
  virtualSize          <- liftM fromIntegral getWord32le
  virtualAddress       <- liftM fromIntegral getWord32le
  sizeOfRawData        <- liftM fromIntegral getWord32le
  pointerToRawData     <- liftM fromIntegral getWord32le
  pointerToRelocations <- liftM fromIntegral getWord32le
  pointerToLinenumbers <- liftM fromIntegral getWord32le
  numberOfRelocations  <- liftM fromIntegral getWord16le
  numberOfLinenumbers  <- liftM fromIntegral getWord16le
  characteristics      <- liftM imageScnCharacteristics getWord32le
  return $ PecoffSection
         { psectName            = name
         , psectVirtualSize     = virtualSize
         , psectVirtualAddress  = virtualAddress
         , psectCharacteristics = characteristics
         , psectRelocations     = B.take (10 * numberOfRelocations) $ B.drop pointerToRelocations bs
         , psectLinenumbers     = B.take (6 * numberOfLinenumbers) $ B.drop pointerToLinenumbers bs
         , psectRawData         = B.take sizeOfRawData $ B.drop pointerToRawData bs
         }
                          
getPeOffset = do
  magic <- liftM (C.unpack . B.pack) $ sequence [getWord8, getWord8]
  if magic == "MZ" then do
     skip 0x3a
     coffOffset <- liftM fromIntegral getWord32le
     return coffOffset
   else
     fail "Invalid magic number in MSDOS header."
               
getPecoff bs = do
  magic <- liftM (C.unpack . B.pack) $ sequence [getWord8, getWord8, getWord8, getWord8]
  if magic /= "PE\0\0" then
      fail "Invalid magic number in PE header."
   else do
     (machine, numsects, fileCharacteristics)                            <- getImageFileHeader
     (pr, addressOfEntryPoint, imageBase, subsystem, dllCharacteristics) <- getImageFileOptionalHeader
     sections                                                            <- sequence $ replicate numsects (getSectionHeader pr bs)
     return $ Pecoff
                { pMachine             = machine
                , pFileCharacteristics = fileCharacteristics
                , pEntryPointAddress   = addressOfEntryPoint
                , pImageBase           = imageBase
                , pSubsystem           = subsystem
                , pDllCharacteristics  = dllCharacteristics
                , pSections            = sections
                }
     
-- | Parse the ByteString of a PE/COFF file into a Pecoff record.
parsePecoff :: B.ByteString -> Pecoff
parsePecoff bs =
  let coffOffset = runGet getPeOffset $ L.fromChunks [bs]
      coffObject = B.drop coffOffset bs
  in runGet (getPecoff coffObject) $ L.fromChunks [coffObject]
  
