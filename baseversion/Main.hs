module Main where
import TestHarness


main::IO()
main = do                                      --this is an example to show how to get user's input to configure the simulation.
       putStrLn "We are about to start testmeHetero simulation"
       putStrLn "Please configure the initial inventry of HFT agents.Such as [3000,0,3000] "
       input <- getLine
       
       let invcfg :: [Double]                 -- use 'read' to parse the input,  
           invcfg = (read input)
           
       putStrLn $"The number of Hft agents is " ++ (show $length invcfg)
       putStrLn "testmeHetero starts"
       hftwaveHD 2700 invcfg 1 1
       putStrLn "testmeHetero ends"






