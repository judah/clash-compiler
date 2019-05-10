module AES where

import Clash.Prelude
import Data.Bool

sBoxConsts :: Vec 256 (BitVector 8)
sBoxConsts = $(listToVecTH [
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16 :: BitVector 8
    ])

sBox :: BitVector 8 -> BitVector 8
sBox = asyncRomPow2 sBoxConsts . unpack

gfDouble :: Vec 8 Bool -> Vec 8 Bool
gfDouble (msb :> rest) = zipWith xor (rest :< False) $ unpack $ bool 0 0x1b msb

keyScheduleStep :: BitVector 8 -> Vec 4 (BitVector 32) -> Vec 4 (BitVector 32)
keyScheduleStep rcon state = postscanl xor nextKS state
    where
    nextKS :: BitVector 32
    nextKS = pack $ zipWith xor (rcon :> repeat 0) $ map sBox (rotateLeftS (unpack $ last state) (SNat @ 1))

type AESState = Vec 4 (Vec 4 (BitVector 8))

subBytes :: AESState -> AESState
subBytes = map (map sBox)

shiftRows :: AESState -> AESState
shiftRows = transpose . smap (flip rotateLeftS) . transpose

mixColumns :: AESState -> AESState
mixColumns = map mixColumn
    where
    mixColumn :: Vec 4 (BitVector 8) -> Vec 4 (BitVector 8)
    mixColumn col = r0 :> r1 :> r2 :> r3 :> Nil
        where
        doubled = map (pack . gfDouble . unpack) col

        r0 = (doubled !! 0) `xor` (col !! 3) `xor` (col !! 2) `xor` (doubled !! 1) `xor` (col !! 1);
        r1 = (doubled !! 1) `xor` (col !! 0) `xor` (col !! 3) `xor` (doubled !! 2) `xor` (col !! 2);
        r2 = (doubled !! 2) `xor` (col !! 1) `xor` (col !! 0) `xor` (doubled !! 3) `xor` (col !! 3);
        r3 = (doubled !! 3) `xor` (col !! 2) `xor` (col !! 1) `xor` (doubled !! 0) `xor` (col !! 0);

addRoundKey :: Vec 4 (BitVector 32) -> AESState -> AESState
addRoundKey key state = zipWith (zipWith xor) (map unpack key) state

round0 :: Vec 4 (BitVector 32) -> AESState -> AESState
round0 = addRoundKey

roundN :: Vec 4 (BitVector 32) -> AESState -> AESState
roundN roundKey = addRoundKey roundKey . mixColumns . shiftRows . subBytes

roundLast :: Vec 4 (BitVector 32) -> AESState -> AESState
roundLast roundKey = addRoundKey roundKey . shiftRows . subBytes

keyExpander
    :: forall dom enabled sync
    .  HiddenClockReset dom enabled sync
    => Signal tag Bool
    -> Signal tag (BitVector 128)
    -> Signal tag (Vec 4 (BitVector 32))
keyExpander start key = keyState
    where
    keyState :: Signal tag (Vec 4 (BitVector 32))
    keyState =  register (repeat 0) $ mux start (unpack <$> key) (keyScheduleStep <$> (pack <$> rc) <*> keyState)

    rc :: Signal tag (Vec 8 Bool)
    rc =  register (unpack 1) $ mux start (pure $ unpack 1) (gfDouble <$> rc)

aes
    :: forall dom enabled sync
    .  HiddenClockReset dom enabled sync
    => Signal tag Bool
    -> Signal tag (BitVector 128)
    -> Signal tag (BitVector 128)
    -> Signal tag (Unsigned 4, Vec 4 (BitVector 32), BitVector 128, Bool)
aes start key block = bundle (cnt, roundKey, pack <$> roundState, cnt .==. 11)
    where

    roundKey :: Signal tag (Vec 4 (BitVector 32))
    roundKey =  keyExpander start key

    cnt :: Signal tag (Unsigned 4)
    cnt =  register 0 $ mux start 0 (cnt + 1)

    roundState :: Signal tag AESState
    roundState = register (repeat (repeat 0)) $ step <$> cnt <*> roundState <*> roundKey <*> block
        where
        step :: Unsigned 4 -> AESState -> Vec 4 (BitVector 32) -> BitVector 128 -> AESState
        step cnt roundState roundKey block = addRoundKey roundKey $ bool preRoundKey (unpack block) (cnt == 0)
            where
            preRoundKey   = bool (mixColumns preMixColumns) preMixColumns (cnt == 10)
            preMixColumns = shiftRows $ subBytes roundState

topEntity clk rst = withClockReset @System @Regular @Synchronous clk rst aes
