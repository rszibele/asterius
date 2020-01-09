{-# LANGUAGE CPP, MagicHash, ScopedTypeVariables #-}

-- Get definitions for the structs, constants & config etc.
#include "Rts.h"

-- |
-- Run-time info table support.  This module provides support for
-- creating and reading info tables /in the running program/.
-- We use the RTS data structures directly via hsc2hs.
--
module GHCi.InfoTable
  (
#ifdef GHCI
    mkConInfoTable
#endif
  ) where

#ifdef GHCI
import Foreign
import Foreign.C
import GHC.Ptr
import GHC.Exts
import GHC.Exts.Heap
#endif

#ifdef GHCI /* To end */
-- NOTE: Must return a pointer acceptable for use in the header of a closure.
-- If tables_next_to_code is enabled, then it must point the the 'code' field.
-- Otherwise, it should point to the start of the StgInfoTable.
mkConInfoTable
   :: Bool    -- TABLES_NEXT_TO_CODE
   -> Int     -- ptr words
   -> Int     -- non-ptr words
   -> Int     -- constr tag
   -> Int     -- pointer tag
   -> [Word8]  -- con desc
   -> IO (Ptr StgInfoTable)
      -- resulting info table is allocated with allocateExec(), and
      -- should be freed with freeExec().

mkConInfoTable tables_next_to_code ptr_words nonptr_words tag ptrtag con_desc =
  castFunPtrToPtr <$> newExecConItbl tables_next_to_code itbl con_desc
  where
     entry_addr = interpConstrEntry !! ptrtag
     code' = mkJumpToAddr entry_addr
     itbl  = StgInfoTable {
                 entry = if tables_next_to_code
                         then Nothing
                         else Just entry_addr,
                 ptrs  = fromIntegral ptr_words,
                 nptrs = fromIntegral nonptr_words,
                 tipe  = CONSTR,
                 srtlen = fromIntegral tag,
                 code  = if tables_next_to_code
                         then Just code'
                         else Nothing
              }


-- -----------------------------------------------------------------------------
-- Building machine code fragments for a constructor's entry code

funPtrToInt :: FunPtr a -> Int
funPtrToInt (FunPtr a) = I## (addr2Int## a)

data Arch = ArchSPARC
          | ArchPPC
          | ArchX86
          | ArchX86_64
          | ArchAlpha
          | ArchARM
          | ArchARM64
          | ArchPPC64
          | ArchPPC64LE
          | ArchUnknown
 deriving Show

platform :: Arch
platform =
#if defined(sparc_HOST_ARCH)
       ArchSPARC
#elif defined(powerpc_HOST_ARCH)
       ArchPPC
#elif defined(i386_HOST_ARCH)
       ArchX86
#elif defined(x86_64_HOST_ARCH)
       ArchX86_64
#elif defined(alpha_HOST_ARCH)
       ArchAlpha
#elif defined(arm_HOST_ARCH)
       ArchARM
#elif defined(aarch64_HOST_ARCH)
       ArchARM64
#elif defined(powerpc64_HOST_ARCH)
       ArchPPC64
#elif defined(powerpc64le_HOST_ARCH)
       ArchPPC64LE
#else
#    if defined(TABLES_NEXT_TO_CODE)
#        error Unimplemented architecture
#    else
       ArchUnknown
#    endif
#endif

mkJumpToAddr :: EntryFunPtr -> ItblCodes
mkJumpToAddr a = case platform of
    ArchSPARC ->
        -- After some consideration, we'll try this, where
        -- 0x55555555 stands in for the address to jump to.
        -- According to includes/rts/MachRegs.h, %g3 is very
        -- likely indeed to be baggable.
        --
        --   0000 07155555              sethi   %hi(0x55555555), %g3
        --   0004 8610E155              or      %g3, %lo(0x55555555), %g3
        --   0008 81C0C000              jmp     %g3
        --   000c 01000000              nop

        let w32 = fromIntegral (funPtrToInt a)

            hi22, lo10 :: Word32 -> Word32
            lo10 x = x .&. 0x3FF
            hi22 x = (x `shiftR` 10) .&. 0x3FFFF

        in Right [ 0x07000000 .|. (hi22 w32),
                   0x8610E000 .|. (lo10 w32),
                   0x81C0C000,
                   0x01000000 ]

    ArchPPC ->
        -- We'll use r12, for no particular reason.
        -- 0xDEADBEEF stands for the address:
        -- 3D80DEAD lis r12,0xDEAD
        -- 618CBEEF ori r12,r12,0xBEEF
        -- 7D8903A6 mtctr r12
        -- 4E800420 bctr

        let w32 = fromIntegral (funPtrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
        in Right [ 0x3D800000 .|. hi16 w32,
                   0x618C0000 .|. lo16 w32,
                   0x7D8903A6, 0x4E800420 ]

    ArchX86 ->
        -- Let the address to jump to be 0xWWXXYYZZ.
        -- Generate   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
        -- which is
        -- B8 ZZ YY XX WW FF E0

        let w32 = fromIntegral (funPtrToInt a) :: Word32
            insnBytes :: [Word8]
            insnBytes
               = [0xB8, byte0 w32, byte1 w32,
                        byte2 w32, byte3 w32,
                  0xFF, 0xE0]
        in
            Left insnBytes

    ArchX86_64 ->
        -- Generates:
        --      jmpq *.L1(%rip)
        --      .align 8
        -- .L1:
        --      .quad <addr>
        --
        -- which looks like:
        --     8:   ff 25 02 00 00 00     jmpq   *0x2(%rip)      # 10 <f+0x10>
        -- with addr at 10.
        --
        -- We need a full 64-bit pointer (we can't assume the info table is
        -- allocated in low memory).  Assuming the info pointer is aligned to
        -- an 8-byte boundary, the addr will also be aligned.

        let w64 = fromIntegral (funPtrToInt a) :: Word64
            insnBytes :: [Word8]
            insnBytes
               = [0xff, 0x25, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
                  byte0 w64, byte1 w64, byte2 w64, byte3 w64,
                  byte4 w64, byte5 w64, byte6 w64, byte7 w64]
        in
            Left insnBytes

    ArchAlpha ->
        let w64 = fromIntegral (funPtrToInt a) :: Word64
        in Right [ 0xc3800000      -- br   at, .+4
                 , 0xa79c000c      -- ldq  at, 12(at)
                 , 0x6bfc0000      -- jmp  (at)    # with zero hint -- oh well
                 , 0x47ff041f      -- nop
                 , fromIntegral (w64 .&. 0x0000FFFF)
                 , fromIntegral ((w64 `shiftR` 32) .&. 0x0000FFFF) ]

    ArchARM { } ->
        -- Generates Arm sequence,
        --      ldr r1, [pc, #0]
        --      bx r1
        --
        -- which looks like:
        --     00000000 <.addr-0x8>:
        --     0:       00109fe5    ldr    r1, [pc]      ; 8 <.addr>
        --     4:       11ff2fe1    bx     r1
        let w32 = fromIntegral (funPtrToInt a) :: Word32
        in Left [ 0x00, 0x10, 0x9f, 0xe5
                , 0x11, 0xff, 0x2f, 0xe1
                , byte0 w32, byte1 w32, byte2 w32, byte3 w32]

    ArchARM64 { } ->
        -- Generates:
        --
        --      ldr     x1, label
        --      br      x1
        -- label:
        --      .quad <addr>
        --
        -- which looks like:
        --     0:       58000041        ldr     x1, <label>
        --     4:       d61f0020        br      x1
       let w64 = fromIntegral (funPtrToInt a) :: Word64
       in Right [ 0x58000041
                , 0xd61f0020
                , fromIntegral w64
                , fromIntegral (w64 `shiftR` 32) ]
    ArchPPC64 ->
        -- We use the compiler's register r12 to read the function
        -- descriptor and the linker's register r11 as a temporary
        -- register to hold the function entry point.
        -- In the medium code model the function descriptor
        -- is located in the first two gigabytes, i.e. the address
        -- of the function pointer is a non-negative 32 bit number.
        -- 0x0EADBEEF stands for the address of the function pointer:
        --    0:   3d 80 0e ad     lis     r12,0x0EAD
        --    4:   61 8c be ef     ori     r12,r12,0xBEEF
        --    8:   e9 6c 00 00     ld      r11,0(r12)
        --    c:   e8 4c 00 08     ld      r2,8(r12)
        --   10:   7d 69 03 a6     mtctr   r11
        --   14:   e9 6c 00 10     ld      r11,16(r12)
        --   18:   4e 80 04 20     bctr
       let  w32 = fromIntegral (funPtrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
       in Right [ 0x3D800000 .|. hi16 w32,
                  0x618C0000 .|. lo16 w32,
                  0xE96C0000,
                  0xE84C0008,
                  0x7D6903A6,
                  0xE96C0010,
                  0x4E800420]

    ArchPPC64LE ->
        -- The ABI requires r12 to point to the function's entry point.
        -- We use the medium code model where code resides in the first
        -- two gigabytes, so loading a non-negative32 bit address
        -- with lis followed by ori is fine.
        -- 0x0EADBEEF stands for the address:
        -- 3D800EAD lis r12,0x0EAD
        -- 618CBEEF ori r12,r12,0xBEEF
        -- 7D8903A6 mtctr r12
        -- 4E800420 bctr

        let w32 = fromIntegral (funPtrToInt a)
            hi16 x = (x `shiftR` 16) .&. 0xFFFF
            lo16 x = x .&. 0xFFFF
        in Right [ 0x3D800000 .|. hi16 w32,
                   0x618C0000 .|. lo16 w32,
                   0x7D8903A6, 0x4E800420 ]

    -- This code must not be called. You either need to
    -- add your architecture as a distinct case or
    -- use non-TABLES_NEXT_TO_CODE mode
    ArchUnknown -> error "mkJumpToAddr: ArchUnknown is unsupported"

byte0 :: (Integral w) => w -> Word8
byte0 w = fromIntegral w

byte1, byte2, byte3, byte4, byte5, byte6, byte7
       :: (Integral w, Bits w) => w -> Word8
byte1 w = fromIntegral (w `shiftR` 8)
byte2 w = fromIntegral (w `shiftR` 16)
byte3 w = fromIntegral (w `shiftR` 24)
byte4 w = fromIntegral (w `shiftR` 32)
byte5 w = fromIntegral (w `shiftR` 40)
byte6 w = fromIntegral (w `shiftR` 48)
byte7 w = fromIntegral (w `shiftR` 56)


-- -----------------------------------------------------------------------------
-- read & write intfo tables

-- entry point for direct returns for created constr itbls
foreign import ccall "&stg_interp_constr1_entry" stg_interp_constr1_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr2_entry" stg_interp_constr2_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr3_entry" stg_interp_constr3_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr4_entry" stg_interp_constr4_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr5_entry" stg_interp_constr5_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr6_entry" stg_interp_constr6_entry :: EntryFunPtr
foreign import ccall "&stg_interp_constr7_entry" stg_interp_constr7_entry :: EntryFunPtr

interpConstrEntry :: [EntryFunPtr]
interpConstrEntry = [ error "pointer tag 0"
                    , stg_interp_constr1_entry
                    , stg_interp_constr2_entry
                    , stg_interp_constr3_entry
                    , stg_interp_constr4_entry
                    , stg_interp_constr5_entry
                    , stg_interp_constr6_entry
                    , stg_interp_constr7_entry ]

data StgConInfoTable = StgConInfoTable {
   conDesc   :: Ptr Word8,
   infoTable :: StgInfoTable
}


pokeConItbl
  :: Bool -> Ptr StgConInfoTable -> Ptr StgConInfoTable -> StgConInfoTable
  -> IO ()
pokeConItbl tables_next_to_code wr_ptr _ex_ptr itbl = do
  if tables_next_to_code
  then do
      -- Write the offset to the con_desc from the end of the standard InfoTable
      -- at the first byte.
      let con_desc_offset = conDesc itbl `minusPtr` (_ex_ptr `plusPtr` conInfoTableSizeB)
      (#poke StgConInfoTable, con_desc) wr_ptr con_desc_offset
  else do
      -- Write the con_desc address after the end of the info table.
      -- Use itblSize because CPP will not pick up PROFILING when calculating
      -- the offset.
      pokeByteOff wr_ptr itblSize (conDesc itbl)
  pokeItbl (wr_ptr `plusPtr` (#offset StgConInfoTable, i)) (infoTable itbl)

sizeOfEntryCode :: Bool -> Int
sizeOfEntryCode tables_next_to_code
  | not tables_next_to_code = 0
  | otherwise =
     case mkJumpToAddr undefined of
       Left  xs -> sizeOf (head xs) * length xs
       Right xs -> sizeOf (head xs) * length xs

-- Note: Must return proper pointer for use in a closure
newExecConItbl :: Bool -> StgInfoTable -> [Word8] -> IO (FunPtr ())
newExecConItbl tables_next_to_code obj con_desc
   = alloca $ \pcode -> do
        let lcon_desc = length con_desc + 1{- null terminator -}
            -- SCARY
            -- This size represents the number of bytes in an StgConInfoTable.
            sz = fromIntegral (conInfoTableSizeB + sizeOfEntryCode tables_next_to_code)
               -- Note: we need to allocate the conDesc string next to the info
               -- table, because on a 64-bit platform we reference this string
               -- with a 32-bit offset relative to the info table, so if we
               -- allocated the string separately it might be out of range.
        wr_ptr <- _allocateExec (sz + fromIntegral lcon_desc) pcode
        ex_ptr <- peek pcode
        let cinfo = StgConInfoTable { conDesc = ex_ptr `plusPtr` fromIntegral sz
                                    , infoTable = obj }
        pokeConItbl tables_next_to_code wr_ptr ex_ptr cinfo
        pokeArray0 0 (castPtr wr_ptr `plusPtr` fromIntegral sz) con_desc
        _flushExec sz ex_ptr -- Cache flush (if needed)
        if tables_next_to_code
          then return (castPtrToFunPtr (ex_ptr `plusPtr` conInfoTableSizeB))
          else return (castPtrToFunPtr ex_ptr)

foreign import ccall unsafe "allocateExec"
  _allocateExec :: CUInt -> Ptr (Ptr a) -> IO (Ptr a)

foreign import ccall unsafe "flushExec"
  _flushExec :: CUInt -> Ptr a -> IO ()

-- -----------------------------------------------------------------------------
-- Constants and config

wORD_SIZE :: Int
wORD_SIZE = (#const SIZEOF_HSINT)

conInfoTableSizeB :: Int
conInfoTableSizeB = wORD_SIZE + itblSize
#endif /* GHCI */