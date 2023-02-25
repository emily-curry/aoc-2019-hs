import AocLib
  ( getPuzzleInput,
    getPuzzleInputFromFilename,
  )
import IntCode (fromString, getArgs, run, withArgs)
import Test.HUnit

testIntcode = fromString "1,9,10,3,2,3,11,0,99,30,40,50"

tests :: Test
tests =
  TestList
    [ TestLabel "getPuzzleInput" tGetPuzzleInput,
      TestLabel "getPuzzleInputFromFilename" tGetPuzzleInputFromFilename,
      TestLabel "IntCode withArgs" tIntCodeWithArgs,
      TestLabel "IntCode 1" tIntCode1
    ]

tIntCodeWithArgs =
  TestCase
    ( do
        let (inoun, iverb) = getArgs testIntcode
        assertEqual "first argument initial" 9 inoun
        assertEqual "second argument initial" 10 iverb
        let (noun, verb) = getArgs $ withArgs 12 2 testIntcode
        assertEqual "first argument" 12 noun
        assertEqual "second argument" 2 verb
    )

tIntCode1 =
  TestCase
    ( do
        let (result, _) = run testIntcode
        assertEqual "ic01 program results in value 3500" 3500 result
    )

tGetPuzzleInput =
  TestCase
    ( do
        input <- getPuzzleInput
        assertEqual "getPuzzleInput returns contents of lib.txt" "okay!\nline 2!" input
    )

tGetPuzzleInputFromFilename =
  TestCase
    ( do
        input <- getPuzzleInputFromFilename "lib.txt"
        assertEqual "getPuzzleInputFromFilename returns contents of lib.txt" "okay!\nline 2!" input
    )

main :: IO Counts
main = runTestTT tests