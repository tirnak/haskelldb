import Test.HUnit
import Data.Monoid
import Control.Monad
import MyDB

before :: IO [Table]
before = do
  createTable "test" [Column{cname="column1", ctype=DBString}]
  tabels <- getTables
  return tabels

test1 = TestCase (do
  tables <- before
  let theTable = tables!!0
  let columnName = cname $ (tstructure theTable)!!0
  assertEqual "column name is stored" columnName "column1")

test2 = TestCase (assertEqual "3 3" 3 3)

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main = do
  runTestTT tests