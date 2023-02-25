module IntCode
  ( IntCode,
    fromString,
    withArgs,
    withResult,
    getArgs,
    run,
  )
where

import Data.Sequence

data Opcode = Add | Mul | Exit deriving (Eq, Show)

toOpcode :: Int -> Opcode
toOpcode 1 = Add
toOpcode 2 = Mul
toOpcode 99 = Exit
toOpcode x = error $ "Not an opcode: " ++ show x

data IntCode = IntCode
  { cursor :: Int,
    memory :: Seq Int
  }
  deriving (Show)

fromString :: String -> IntCode
fromString s = IntCode 0 $ fromList . map read . words . map replaceComma $ s
  where
    replaceComma c
      | c == ',' = ' '
      | otherwise = c

withArgs :: Int -> Int -> IntCode -> IntCode
withArgs noun verb ic = updateMemory (updateMemory ic 1 noun) 2 verb

getArgs :: IntCode -> (Int, Int)
getArgs ic = (indexAbs ic 1, indexAbs ic 2)

withResult :: Int -> IntCode -> IntCode
withResult expected ic = search 0 0
  where
    search a b
      | output == expected = result
      | a == 99 = search 0 (b + 1)
      | otherwise = search (a + 1) b
      where
        (output, result) = run $ withArgs a b ic

updateMemory :: IntCode -> Int -> Int -> IntCode
updateMemory ic idx value = ic {memory = replaced}
  where
    replaced = update idx value $ memory ic

advanceCursor :: Int -> IntCode -> IntCode
advanceCursor o ic = ic {cursor = cursor ic + o}

indexAbs :: IntCode -> Int -> Int
indexAbs ic idx = memory ic `index` idx

indexRel :: IntCode -> Int -> Int
indexRel ic o = indexAbs ic $ o + cursor ic

executeOpcode :: IntCode -> Opcode -> IntCode
executeOpcode ic Add = advanceCursor 4 $ u (v 1 + v 2)
  where
    v = indexAbs ic . indexRel ic
    u = updateMemory ic $ indexRel ic 3
executeOpcode ic Mul = advanceCursor 4 $ u (v 1 * v 2)
  where
    v = indexAbs ic . indexRel ic
    u = updateMemory ic $ indexRel ic 3
executeOpcode _ Exit = error "Cannot execute exit instruction!"

step :: IntCode -> Either IntCode (Int, IntCode)
step ic
  | opcode == Exit = Right (indexAbs ic 0, ic)
  | otherwise = Left $ executeOpcode ic opcode
  where
    opcode = toOpcode $ indexRel ic 0

run :: IntCode -> (Int, IntCode)
run ic = case step ic of
  Left nic -> run nic
  Right r -> r