import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Algorithm/Search.hs"]
