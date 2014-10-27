module Stats where


import System.IO
import Data.List
import System.IO.Unsafe
--import Data.Tuple


converse :: (a->b->c)->(b->a->c)
converse f a b = f b a     

printmultiplesimstats inputname numberofruns columnnum
   = printcollectedstatoutput (collectstatfuncs inputname numberofruns columnnum) inputname

collectstatfuncs :: (Num a,Show a,Enum a)=>[Char] -> Int -> Int -> [[([Char], [(a,Double)])]]      --be careful of the types, potential bugs        
collectstatfuncs inputname numberofruns columnnum
   = transpose statistics
     where
     statistics = maparg True (maparg columnnum (map ((statfuncs).(converse (++) "-data.csv").((++) inputname).(show)) [0..numberofruns]))
     -- maparg :: * -> [*->**] -> [**]
     maparg arg [] = []
     maparg arg (x:remainingfuncs) = (x arg) : (maparg arg remainingfuncs)

--Pretty sure I just rewrote transpose but you know, saving it just in case.
--superzip inputlist
--     = [], if member inputlist []
--     = (tinyzip inputlist) : (superzip (map (drop 1) inputlist)), otherwise
--       where
--       || tinyzip :: [[*]] -> [*]
--       tinyzip [] = []
--       tinyzip (x:rest) = ((hd x) : (tinyzip rest))

printcollectedstatoutput ::(Num a,Show a)=>[[([Char], [(a,Double)])]] -> [Char] -> IO()
printcollectedstatoutput values prefix
    = do
      outh<- openFile (prefix++"-collectedstats.txt") WriteMode
      hSetBuffering outh (BlockBuffering Nothing)     -- set buffer mode 
      hPutStr outh resultstring                
      hClose outh
      where
      resultstring = concat (map printentry values)
      --At this point we're looking at [([char], [(num,num)])] Every element is going to have the same name bar some suffix.
      printentry entryvalues = concat (funczip (map buckettostr [1..(length bucketlist)]) bucketlist)
                               where
                               funczip [] y = []
                               funczip y [] = []
                               funczip (x:xs) (y:ys) = (x y) : funczip xs ys
                               buckettostr ind bucket = name ++ (show ind) ++ ": " ++ (((concat).(map ((converse (++) ", ").(show)))) bucket) ++ "\n"
                               name = fst (head entryvalues)
                               bucketlist = bucketise lists -- Get it? [[num]]
                               lists = (map snd entryvalues) --[[(num,num)]]
                               bucketise ([]:anything) = []
                               bucketise list = (map ((snd).(head)) list) : bucketise (map tail list)  -- [num] is result of first part, thus total result is [[num]]

printstatoutput :: (Num a,Show a)=>[([Char], [(a,a)])] -> [Char] -> IO()
printstatoutput values prefix
     = do
       outh<- openFile (prefix++"-stats.txt") WriteMode
       hSetBuffering outh (BlockBuffering Nothing)     -- set buffer mode 
       hPutStr outh resultstring                
       hClose outh
       where
       resultstring = concat (map (makestr False) values)
       makestr label val |and [(length(snd val) == 1), label == True]   = (show (snd ((snd val)!!0))) ++ "\n"
                         |length(snd val) == 1                         = (fst val) ++ (show (fst ((snd val)!!0))) ++ ": " ++ (show (snd ((snd val)!!0))) ++ "\n"
                         |label == True                           = ((show (snd ((snd val)!!0))) ++ ", ") ++ (makestr True ((fst val), (drop 1 (snd val))))
                         |otherwise    = ((fst val) ++ (show (fst ((snd val)!!0))) ++ ": " ++ (show (snd ((snd val)!!0))) ++ ", ") ++ (makestr True ((fst val), (drop 1 (snd val))))
                        
statfuncs :: Num a=>[Char] -> Int -> Bool -> [([Char], [(a,Double)])]
statfuncs inputname columnnum trimheaders  -- If you get a numval error then set trimheaders to true.
    = results
      where
      trimmed = if trimheaders == False  
                then input 
                else drop ((findfirst input '\n')+1) input
      input = unsafePerformIO $ getInput inputname
      csv = str2rows trimmed
      vallist = map ((converse (!!)) columnnum) csv
      functions = [("mean",uniformat.mean), ("var",uniformat.var), ("stdev",uniformat.stdev), ("skew", uniformat.skew), ("kurt",uniformat.kurt)] --uniformat turns num to [(1,num)], uniformatlist does the same for list of nums.
      results = map (arg2func vallist) functions
      getInput dir = do 
                      s<- readFile dir
                      return s

arg2func argument function = ((fst function),((snd function) argument))


str2rows :: [Char] -> [[Double]]
str2rows str = if not(null rest)  
               then vals : str2rows rest
               else [vals]
               where
               (vals, rest) = (str2row str)

str2row :: [Char] -> ([Double],[Char])
str2row str = if and [not(null str), endofrow == False]
              then (val : vallist, remstr) 
              else ([val], rest)
              where
              (vallist, remstr) = str2row rest
              endofrow = if and [ci /= -1, commabeforenewline]  
                         then False  -- cbnl is true and ci = 2
                         else True
              commabeforenewline | nli == -1  = True
                                 | ci < nli   = True 
                                 | otherwise  = False
              rest | and [ci /= -1, commabeforenewline]  = drop (ci+1) str
                   | nli /= -1                           = drop (nli+1) str
                   | otherwise                           = []
              val = read strval :: Double    -- not sure it's Double, need to be confirmed
              strval | and [ci /= -1, commabeforenewline] = take ci str 
                     | nli /= -1                          = take nli str
                     | otherwise                          = str
              nli = findfirst str '\n'
              ci = findfirst str ','


findfirst :: (Num num,Eq a)=>[a] -> a -> num
findfirst list value 
      = realfindfirst list value 0
        where
        realfindfirst [] value i = -1                  -- Could not find the value in the list.
        realfindfirst (h:t) value i  = if h == value 
                                       then i
                                       else realfindfirst t value (i+1)


uniformatlist [] = []
uniformatlist vals = realformatlist vals 1
                         where
                         realformatlist [] num = []
                         realformatlist (x:rest) num = (num,x):(realformatlist rest (num+1))

uniformat x = [(1,x)]

logs xs = map log xs

mean xs = (foldr (+) 0 xs) / (fromIntegral $length xs)

var xs = (foldr (+) 0 devs) / (fromIntegral $length xs)
               where
               devs = map ((^2).((converse (-)) mu)) xs
               mu = mean xs

stdev xs = sqrt (var xs)

skew xs = (foldr (+) 0 cubes) / (fromIntegral $length xs)
                  where
                  cubes = map ((^3).(/sigma).((converse (-))mu)) xs
                  mu = mean xs
                  sigma = stdev xs

kurt xs = ((foldr (+) 0 quads) / (fromIntegral $length xs)) - 3
                where
                quads = (map ((^4).(/sigma).((converse (-))mu)) xs)
                mu = mean xs
                sigma = stdev xs

cov xs ys = (foldr (+) 0 prods) / (len - 1)
                    where
                    len = fromIntegral $ minimum [length xs, length ys] 
                    mux = mean xs
                    muy = mean ys
                    xdevs = map ((converse (-))mux) xs
                    ydevs = map ((converse (-))muy) ys
                    prods = f xdevs ydevs
                    f [] bs = []
                    f as [] = []
                    f (a:as) (b:bs) = (a*b) : (f as bs)

--autocov gives the autocovariance of a series against itself (lagged)
-- it takes two args - the sequence and the maximum lag to be tried (30 is a typical value)
-- it returns a list of two-tuples (of length maxlag), each of which gives the lag and the covariance at that lag
-- it assumes (i) the series is (weak) stationary so that the mean is the same for all subseries; (ii) series length >> maxlag
-- it uses other stats functions: mean

autocov xs maxlag = map f [1..maxlag]
                    where
                    f lag = (lag, (mean prods) - (mu^2))
                            where
                            mu = mean xs
                            prods = zipwith (*) xs laggedxs
                            laggedxs = drop lag xs
                            zipwith f []         bs = []
                            zipwith f as         [] = []
                            zipwith f (a:as) (b:bs) = (f a b):(zipwith f as bs)

-- autocorr gives the autocorrelation of a series against itself (lagged)
-- it takes two args - the sequence and the maximum lag to be tried (30 is a typical value)
-- it returns a list of two-tuples (of length maxlag), each of which gives the lag and the corellation at that lag
-- it assumes (i) the series is (weak) stationary so that the mean is the same for all subseries; (ii) series length >> maxlag
-- it uses other stats functions: autocov, stdev
autocorr xs maxlag = map mydiv (autocov xs maxlag)
                     where
                     sigma = stdev xs
                     mydiv (lag, value) = (lag, (value / (sigma ^ 2)))    