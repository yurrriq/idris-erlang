module Main

import CoreErlang

public export
cerl_plus : Int -> Int -> Int
cerl_plus x y = x + y

main : IO ()
main = printLn $ cerl_plus 2 2
