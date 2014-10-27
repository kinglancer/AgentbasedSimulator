module TestHarness where


import Comm
import Sim
import Stats
import Agent
import Messages


import System.IO
import System.Directory


import Nicemime
import Noiseagent
import Hft
import LaggedHft
import Fundamentals

-- ==============================================================
-- Notes on sim use
-- -----------------------
-- 
-- Presence of (Arg (Str "Randomise", y)) indicates that simulator should randomise order of messages passed.
-- Presense of (Arg (Str "StubsEnabled", y)) indicates that HFTs should use stub orders.
-- Presense of (Arg (Str "Stubsize", y)) indicates the size to be used for stub orders (if stubs enabled).
-- Presense of (Arg (Str "ProbeSize", y)) indicates the size to be used for each market order issued by the probe agent
-- Presense of (Arg (Str "Delay", y)) indicates messages inbund to HFTs ONLY be delayed by y timesteps where y <- [0 .. ]. Y is 1 by default.
-- Presense of (Arg (Str "SoftLimit", y)) indicates HFTs soft limit should be Y. Y is defsoftlimit (defined in Hft.m) by default.
-- Presense of (Arg (Str "AbsLimit", y)) indicates HFTs hard limit should be Y. Y is defabslimit (defined in Hft.m) by default.
-- Presense of (Arg (Str "ProbeStop", y)) indicates that the test agent adding liquidity should stop at timestep y.
-- Presense of (Arg (Str prefix, 9989793425)) indicates files should be written to the messyoutputfolder folder in the files prefixed by the specified prefix.
-- Presense of (Arg (Str "PeriodicLiquidity", y)) has the test agent etc. etc.
-- Presense of (Arg (Str "MinimumRestingTime", y)) enforces a minimum resting time of y at the exchange, 0 is the default.
-- Presense of (Arg (Str "autostats", y)) causes automatic generation of statistics file.
-- Presense of (Arg (Str "randseed", y)) causes sim harness to use y as a seed for randomisation.
-- Presense of (Arg (Str "<X>HFTiseed", y)) causes sim harness to use y as initial inventory for the HFT with id <X> should that HFT exist, default 0.
-- Presense of (Arg (Str "amxcoef", y)) causes sim harness to use y as initial inventory for the HFT with id <X> should that HFT exist, default 0.
-- 
-- 
-- 
-- 
-- 
-- ==============================================================


--Producing data for the publication:
--
--Figure 17 - D[01]SiFalseLltFalseTse40NoFalseStuSz1ProSz15Ags5          - runTest (0,False,False,40,False,1,15,5)    and    runTest (1,False,False,40,False,1,15,5)
--Figure 18 - D0Si[False,True]LltFalseTse140NoFalseStuSz1ProSz15Ags5     - runTest (0,False,False,140,False,1,15,5)   and    runTest (0,True,False,140,False,1,15,5)   || could also include noise traders
--Figure 19 - D0SiTrueLltFalseTse40NoFalseStuSz[1,10,100]ProSz15Ags5     - runTest (0,True,False,140,False,10,15,5)   and    runTest (0,True,False,140,False,100,15,5)
--Figure 20 - D0SiTrueLltFalseTse40NoFalseStuSz[1,10,100]ProSz15Ags5     - runTest (0,True,False,140,False,10,15,5)   and    runTest (0,True,False,140,False,100,15,5)
--
--
--increasing delay
--increasing coefficient

--Stable conditions 6 FSellers, 4 FBuyers, 3 Noise agents.
testforsentiments ::IO()
testforsentiments
  = mapM_ testsent [ ("Calm", True), ("Ramp", True), ("Choppy", True) , ("Ramp", False), ("Choppy", False), ("Calm",False)]          --using monad
    where
    testsent (sent, noise) = do 
                             createDirectory ("messyoutputfolder/"++thefilename)
                             sim 300 myarguments myagents
                             
                             where
                             dir = "messyoutputfolder/" ++ thefilename ++ "/" ++ thefilename
                             thefilename = "SentTest" ++ sent ++ "Noise" ++ (show noise) --was 4 hfts
                             myagents = [(nice_mime_wrapper, [0])] ++ (rep 10 (fundwrapper, [0])) ++ (rep 4 (hftwrapper, [0])) ++ nag ++ (rep 0 (testagent1,[0])) -- (System ("mkdir messyoutputfolder/"++thefilename))
                             nag = if noise == True
                                   then rep 3 (noisywrapper,[0])  --Noisy agents
                                   else []
                             delayarg = [(Arg (Str "Delay", 0))]
                             stubsarg = rep 1 (Arg (Str "StubsEnabled", 1))
                             stubsizearg = rep 1 (Arg (Str "Stubsize", 1))
                             plarg = [(Arg (Str "PeriodicLiquidity", 4))] --Periodic liquidity arg.
                             psarg = [(Arg (Str "ProbeStop", 300))] --probestop arg
                             myarguments = [(Arg (Str sent, 1))] ++ (rep 6 (Arg (Str "FundSeller", 16))) ++ (rep 4 (Arg (Str "FundBuyer", 4))) ++ [(Arg (Str dir, 9989793425))] ++ [(Arg (Str "Randomise", 1))] ++ delayarg ++ stubsarg ++ plarg ++ psarg ++ stubsizearg ++ [(Arg (Str "autostats", 0))]


                             
    
    
    




jalt = do
       stabilitytest 5 0 0 0 0 1 40 100 300 True False True 0 3000 
       stabilitytest 5 0 0 0 1 1 40 100 300 True False True 0 3000
       stabilitytest 5 0 0 0 0 1 500 500 1000 True False True 0 3000 
       stabilitytest 5 0 0 0 0 1 500 500 1000 True True True 1000 3000

--stabilitytest 1 2 10 10 1 0 0 0 300 False False True 0 2700 --Looking for single HFT swing.
--stabilitytest 1 2 10 10 3 0 0 0 300 False False True 0 2700



--stabilitytest :: num -> num -> num -> num -> num -> num -> num -> num -> num -> bool -> bool -> bool -> num -> num -> [sys_message]
stabilitytest hfts noises sellers buyers delay probe probestop probesize steps splitting stubs randomise stubsize softlimit
  = mapM_ testsent [ ("Calm", True)]
    where
    testsent (sent, noise) = do                                                  
                             createDirectory ("messyoutputfolder/"++thefilename) 
                             sim steps myarguments myagents 
                             stats                       
    
                             where
                             dir = "messyoutputfolder/" ++ thefilename ++ "/" ++ thefilename
                             stats = printstatoutput (statfuncs (dir ++ "-data.csv") 27 True) dir
                             thefilename = "StabTest" ++ "-" ++ (show hfts) ++ "-" ++ (show noises) ++ "-" ++ (show sellers) ++ "-" ++ (show buyers) ++ "-" ++ (show delay) ++ "-" ++ (show probe) ++ "-" ++ (show probestop) ++ "-" ++ (show probesize) ++ "-" ++ (show steps) ++ "-" ++ (show splitting) ++ "-" ++ (show stubs) ++ "-" ++ (show randomise) ++ "-" ++ (show stubsize) ++ "-" ++ (show softlimit) --was 4 hfts
                             myagents = [(nice_mime_wrapper, [0])] ++ (rep (sellers + buyers) (fundwrapper, [0])) ++ (rep hfts (hftwrapper, [0])) ++ nag ++ (rep probe (testagent1,[0])) -- (System ("mkdir messyoutputfolder/"++thefilename))
                             nag = if noise == True
                                   then rep noises (noisywrapper,[0])  --Noisy agents
                                   else []
                             delayarg = [(Arg (Str "Delay", delay))]
                             stubsarg = if stubs == False 
                                        then []
                                        else rep 1 (Arg (Str "StubsEnabled", 0))
                             stubsizearg = rep 1 (Arg (Str "Stubsize", stubsize)) --used 1000 to get our results
                             plarg = [(Arg (Str "PeriodicLiquidity", 4))] --Periodic liquidity arg.
                             psarg = [(Arg (Str "ProbeStop", probestop))] --probestop arg
                             pszarg = [(Arg (Str "ProbeSize", probesize))] -- was 500 for stubs panic
                             ospl = if splitting == False
                                    then []
                                    else [(Arg (Str "UseGaussians", 1))]
                             randomisea = if randomise == True  
                                          then [(Arg (Str "Randomise", 1))]
                                          else []
                             softlimitarg = [(Arg (Str "SoftLimit", softlimit))]
                             myarguments = [(Arg (Str sent, 1))] ++ (rep sellers (Arg (Str "FundSeller", 16))) ++ (rep buyers (Arg (Str "FundBuyer", 4))) ++ [(Arg (Str dir, 9989793425))] ++ randomisea ++ ospl ++ delayarg ++ stubsarg ++ plarg ++ psarg ++ stubsizearg ++ pszarg ++ softlimitarg ++ [(Arg (Str "autostats", 0))]

superlongtest = mapM_ runTest mytuples
                where
                mytuples = map list2tuple mylist
                list2tuple [a,b,c,d,e,f,g,h,i,j] = (a,nb,nc,d,ne,f,g,h,i,jj)    -- CDC 17/08/12
                                                   where
                                                   jj | j == (-5)   = True 
                                                      | j == (-6)   = False
                                                   nb | b == (-5)   = True  
                                                      | b == (-6)   = False 
                                                   nc | c == (-5)   = True  
                                                      | c == (-6)   = False 
                                                   ne | e == (-5)   = True 
                                                      | e == (-6)   = False
                mylist = (map (++ [2700,-6]) list8)
                list8 = (map (++ [5]) list7)                                                     -- CDC 17/08/12 number of agents always 5
                list7 = (map (++ [15]) list6)                                                    -- CDC 17/08/12 probe size only at 15
                list6 = (map (++ [1]) list5) ++ (map (++ [10]) list5) ++ (map (++ [100]) list5)  -- CDC 17/08/12 try three different stub sizes - 1, 10, 100
                list5 = (map (++ [-5]) list4) ++ (map (++ [-6]) list4)
                list4 = (map (++ [140]) list3) ++ (map (++ [40]) list3)
                list3 = (map (++ [-5]) list2) ++ (map (++ [-6]) list2)
                list2 = (map (++ [-5]) list1) ++ (map (++ [-6]) list1)
                list1 = [[1],[0],[2]]

mylongtest = mapM_ runTest mytuples
             where
             mytuples = map list2tuple mylist
             list2tuple [a,b,c,d,e,f,g,h,i,j] = (a,nb,nc,d,ne,f,g,h,i,jj)    -- CDC 17/08/12
                                                where
                                                jj| j == -5    = True  
                                                  | j == -6    = False 
                                                nb| b == -5    = True  
                                                  | b == -6    = False 
                                                nc| c == -5    = True  
                                                  | c == -6    = False 
                                                ne| e == -5    = True  
                                                  | e == -6    = False 
             mylist = (map (++ [2700,-6]) list8)
             list8 = (map (++ [5]) list7) -- ++ (map (++ [10]) list7) ++ (map (++ [20]) list7)  -- CDC 17/08/12 number of agents = 5
             list7 = (map (++ [100]) list6) -- ++ (map (++ [1000]) list6)                       -- CDC 17/08/12 probe size 100 
             list6 = (map (++ [1]) list5) ++ (map (++ [10]) list5) ++ (map (++ [100]) list5)    -- CDC 17/08/12 try three different stub sizes - 1, 10, 100
             list5 = (map (++ [-6]) list4)  -- ++ (map (++ [-5]) list4) -- CDC noise agents off
             list4 = (map (++ [140]) list3) -- ++ (map (++ [40]) list3) -- CDC probe stops at 140
             list3 = (map (++ [-6]) list2)  -- CDC no low limit test
             list2 = (map (++ [-5]) list1)  -- CDC only stubs in
             list1 = [[0]]                  -- CDC only try delays off

--runTest :: (num, bool, bool, num, bool, num, num, num, num, bool) -> [sys_message]
runTest (delay, stubs, lowlimittest, tastopearly, noiseon, stubsize, probesize, numagents,softlimit,usegauss) --num, bool, bool, num, bool, num, num num (tastopearly = num of stop timestep)
  = do 
    createDirectory ("messyoutputfolder/"++thefilename)   
    sim 300 meinargs meinagents                                                                       
  
  
    where
    meinargs = [(Arg (Str "Calm", 1)), (Arg (Str "Randomise", 1)), (Arg (Str fthefilename, 9989793425))]
               ++ delayarg ++ stubarg ++ limitarg ++ tastoparg ++ stubsizearg ++ probesizearg ++ softlimitarg ++ usegaussarg ++ [(Arg (Str "autostats", 0))]
    fthefilename = "messyoutputfolder/" ++ thefilename ++ "/" ++ thefilename
    thefilename = "D"++(show delay)++"Si"++(show stubs)++"Llt"++(show lowlimittest)++"Tse"++(show tastopearly)
                     ++"No"++(show noiseon)++"StuSz"++(show stubsize)++"ProSz"++(show probesize)++"Ags"++(show numagents)
                     ++"SLim"++(show softlimit)++"Gauss"++(show usegauss) -- CDC 17/08/12
    delayarg = [(Arg (Str "Delay", delay))]
    stubarg = if stubs == True
              then [(Arg (Str "StubsEnabled", 1))] 
              else []
    stubsizearg = [(Arg (Str "StubSize", stubsize))]      -- CDC 17/08/12  added stubsize argument - see Hft.m
    probesizearg = [(Arg (Str "ProbeSize", probesize))]   -- CDC 17/08/12  added probesize argument - see Agents.m
    usegaussarg = if usegauss==True
                  then [(Arg (Str "UseGaussians", 1))]    -- CDC 18/08/12  
                  else []
    softlimitarg = if lowlimittest == True
                   then [] 
                   else [(Arg (Str "SoftLimit", softlimit))]
    (limitarg, limitag) = if lowlimittest == True
                          then ([(Arg (Str "SoftLimit", 10))], (rep 19 (hftwrapper, [0])))
                          else ([], (rep (round numagents) (hftwrapper, [0])))
    tastoparg = [(Arg (Str "ProbeStop", tastopearly))]
    noisyag = if noiseon == True
              then [(noisywrapper, [0])] 
              else []
    meinagents = [(nice_mime_wrapper, [0]), (testagent1,[0])] ++ limitag ++ noisyag

--runTest (1, False, False, 0, False, 0, 0, 2, 2700, False)
--
--(delay, stubs, lowlimittest, tastopearly, noiseon, stubsize, probesize, numagents,softlimit,usegauss)
--runTest (1, False, False, 140, True, 0, 50, 5, 2700, False)
--runTestMRT (1, False, False, 50, True, 0, 50, 5, 2700, False)
--runTestMRT (0, False, False, 140, True, 0, 50, 5, 2700, False)
--runTestMRT (0, True, False, 140, True, 3, 50, 5, 2700, False)
--runTestMRT (1, False, False, 80, True, 0, 50, 5, 2700, False)
--runTestMRTmult 3 (1, False, False, 80, True, 0, 50, 5, 2700, False)
--rtMRTmwrapper 15 (1, False, False, 80, True, 0, 50, 5, 2700, False, 5) ||Run test in diagram 5.3.2 with resting times. (this is with a 5 step mrt)
--rtMRTmwrapper 15 (1, False, False, 80, True, 0, 50, 5, 2700, False, 0)
--rtMRTmwrapper 40 (1, False, False, 80, True, 0, 50, 5, 2700, False, 25) ||15 step MRT
--
--rtMRTmwrapper 1 (1, False, False, 80, True, 0, 50, 5, 2700, False, 25) ||15 step MRT
--
--
--
--
--rtMRTmwrapper 15 (1, False, False, 80, True, 0, 50, 5, 2700, False, 100)
--
--
--rtMRTmwrapper 0 (1, False, False, 80, True, 0, 50, 5, 2700, False, 0)
--
--
--rtMRTmwrapper 3 (1, False, False, 80, True, 0, 50, 5, 2700, False, 5)

arcn = 27 -- Column number for asset returns in data files.

rtMRTmwrapper times (delay, stubs, lowlimittest, tastopearly, noiseon, stubsize, probesize, numagents,softlimit,usegauss, mrt) --Because wrappers are cool.
  = do
    (runTestMRTmult times (delay, stubs, lowlimittest, tastopearly, noiseon, stubsize, probesize, numagents,softlimit,usegauss, mrt)  (systemTime 1))
    (printmultiplesimstats fthefilename times arcn)
    where
    fthefilename = "messyoutputfolder/MRTs/" ++ thefilename ++ "/" ++thefilename
    thefilename = "MRT" ++ (show mrt) ++ "D"++(show delay)++"Si"++(show stubs)++"Llt"++(show lowlimittest)++"Tse"++(show tastopearly)
                     ++"No"++(show noiseon)++"StuSz"++(show stubsize)++"ProSz"++(show probesize)++"Ags"++(show numagents)
                     ++"SLim"++(show softlimit)++"Gauss"++(show usegauss) -- CDC 17/08/12

-- This is used to run multiple MRT (Minimum Resting Time) simulations with the same parameters.
-- There are multiple runs because the simulator is non-deterministic, so the results may vary slightly on each run.
-- The stats files for each run (that are created automatcially) are brought together into a single file, which can be used later for statistical testing (eg T-test)
-- An MRT simulation is just a simulation with MRT-aware HFTs instead of normal HFTs.

--runTestMRTmult :: num -> (num, bool, bool, num, bool, num, num, num, num, bool, num) -> num -> [sys_message]
runTestMRTmult times (delay, stubs, lowlimittest, tastopearly, noiseon, stubsize, probesize, numagents,softlimit,usegauss,mrt) seed --num, bool, bool, num, bool, num, num num (tastopearly = num of stop timestep)
  = if times == 0
    then do
         createDirectory ("messyoutputfolder/MRTs/"++thefilename)
         sim 1000 meinargs meinagents                             
    else do
         createDirectory ("messyoutputfolder/MRTs/"++thefilename)
         sim 1000 meinargs meinagents
         (runTestMRTmult (times - 1) (delay, stubs, lowlimittest, tastopearly, noiseon, stubsize, probesize, numagents,softlimit,usegauss, mrt)) seed1                           
    
    where
    
    seed1 = fromIntegral (randoms!!(round seed))
    meinargs = [(Arg (Str "Calm", 1)), (Arg (Str "Randomise", 1)), (Arg (Str "randseed", seed)), (Arg (Str fthefilename, 9989793425))] ++ [(Arg (Str "autostats", 0))]
               ++ delayarg ++ stubarg ++ limitarg ++ tastoparg ++ stubsizearg ++ probesizearg ++ softlimitarg ++ usegaussarg ++ mrtarg
    fthefilename = "messyoutputfolder/MRTs/" ++ thefilename ++ "/" ++thefilename ++ (show times)
    thefilename = "MRT" ++ (show mrt) ++ "D"++(show delay)++"Si"++(show stubs)++"Llt"++(show lowlimittest)++"Tse"++(show tastopearly)
                     ++"No"++(show noiseon)++"StuSz"++(show stubsize)++"ProSz"++(show probesize)++"Ags"++(show numagents)
                     ++"SLim"++(show softlimit)++"Gauss"++(show usegauss) -- CDC 17/08/12
    delayarg = [(Arg (Str "Delay", delay))]
    stubarg = if stubs == True
              then [(Arg (Str "StubsEnabled", 1))] 
              else []
    stubsizearg = [(Arg (Str "StubSize", stubsize))]      -- CDC 17/08/12  added stubsize argument - see Hft.m
    probesizearg = [(Arg (Str "ProbeSize", probesize))]   -- CDC 17/08/12  added probesize argument - see Agents.m
    usegaussarg = if usegauss==True 
                  then [(Arg (Str "UseGaussians", 1))]    -- CDC 18/08/12
                  else []
    softlimitarg = if lowlimittest == True
                   then []
                   else [(Arg (Str "SoftLimit", softlimit))]
    (limitarg, limitag) = if lowlimittest == True
                          then ([(Arg (Str "SoftLimit", 10))], (rep 19 (laggedhftwrapper, [0]))) 
                          else ([], (rep numagents (laggedhftwrapper, [0])))
    tastoparg = [(Arg (Str "ProbeStop", tastopearly))]
    noisyag = if noiseon == True
              then [(noisywrapper, [0])] 
              else []
    meinagents = [(nice_mime_wrapper, [0]), (testagent1,[0])] ++ limitag ++ noisyag   -- ++ [(spikeagent, [0,1])]
    mrtarg = if mrt > 0
             then [(Arg (Str "MinimumRestingTime", mrt))] 
             else []

--runTestMRT :: (num, bool, bool, num, bool, num, num, num, num, bool, num) -> [sys_message]
runTestMRT (delay, stubs, lowlimittest, tastopearly, noiseon, stubsize, probesize, numagents,softlimit,usegauss,mrt) --num, bool, bool, num, bool, num, num num (tastopearly = num of stop timestep)
  = do
    createDirectory ("messyoutputfolder/MRTs/"++thefilename)  
    sim 1000 meinargs meinagents                          
    
    where
    meinargs = [(Arg (Str "Calm", 1)), (Arg (Str "Randomise", 1)), (Arg (Str fthefilename, 9989793425))]
               ++ delayarg ++ stubarg ++ limitarg ++ tastoparg ++ stubsizearg ++ probesizearg ++ softlimitarg ++ usegaussarg ++ mrtarg ++ [(Arg (Str "autostats", 0))]
    fthefilename = "messyoutputfolder/MRTs/" ++ thefilename ++ "/" ++ thefilename
    thefilename = "D"++(show delay)++"Si"++(show stubs)++"Llt"++(show lowlimittest)++"Tse"++(show tastopearly)
                     ++"No"++(show noiseon)++"StuSz"++(show stubsize)++"ProSz"++(show probesize)++"Ags"++(show numagents)
                     ++"SLim"++(show softlimit)++"Gauss"++(show usegauss) -- CDC 17/08/12
    delayarg = [(Arg (Str "Delay", delay))]
    stubarg = if stubs == True
              then [(Arg (Str "StubsEnabled", 1))]
              else []
    stubsizearg = [(Arg (Str "StubSize", stubsize))]   -- CDC 17/08/12  added stubsize argument - see Hft.m
    probesizearg = [(Arg (Str "ProbeSize", probesize))]   -- CDC 17/08/12  added probesize argument - see Agents.m
    usegaussarg = if usegauss==True
                  then [(Arg (Str "UseGaussians", 1))]   -- CDC 18/08/12
                  else []
    softlimitarg = if lowlimittest == True
                   then []
                   else [(Arg (Str "SoftLimit", softlimit))]
    (limitarg, limitag) = if lowlimittest == True
                          then ([(Arg (Str "SoftLimit", 10))], (rep 19 (laggedhftwrapper, [0])))
                          else ([], (rep numagents (laggedhftwrapper, [0])))
    tastoparg = [(Arg (Str "ProbeStop", tastopearly))]
    noisyag = if noiseon == True 
              then [(noisywrapper, [0])]
              else []
    meinagents = [(nice_mime_wrapper, [0]), (testagent1,[0])] ++ limitag ++ noisyag
    mrtarg = if mrt > 0 
             then [(Arg (Str "MinimumRestingTime", mrt))] 
             else []


---------------------------------------------------------  
--not sure about this test, because of the (show (system "echo...........)
-- generatevalues maxlength
--  = do
--    outh<-openFile ("surface") WriteMode   
--    hSetBuffering outh (BlockBuffering Nothing) 
--       
--    hPutStr outh  (show (system "echo \"set terminal svg; set output 'surface.svg'; set xrange [0:1]; set yrange [0:10]; set zrange [0:200]; set xlabel 'coef'; set ylabel 'delay'; set zlabel 'timetopanic'; set hidden3d; splot 'surface' with lines\" | gnuplot"))
--    mapM_ (print outh) (map myfunc x)
--    
--    hClose outh
--  
--    where
--    x = [(i, j) | i <- [0.01, 0.02 .. 1], j <- [1, 2 .. 10]] 
--    myfunc (i, j) = (i,j,(howlongtopanic 2 j 0 True 0 i maxlength))
--    print outh (i, j, k) =hPutStr outh ((show i) ++ " " ++ (show j) ++ " " ++ (show k) ++ "\n")
-- 
-- hptest steps hfts tas sent
--  = sim steps specargs specagents
--    where
--    specargs = [(Arg (Str sent, 1)), (Arg (Str "Randomise", 1))] ++ [(Arg (Str "Delay", 0))] ++ [(Arg (Str "autostats", 0))]
--    specagents = [(nice_mime_wrapper, [0])] ++ (rep hfts (hftwrapper, [0])) ++ (rep tas (testagent1,[0]))
-- 
-- --fhowlongtopanic xth delay delta flag neutfrac coef --How close to 2700?
-- -- = take 20 (recurwrap xth delay flag neutfrac coef)
-- 
-- howlongtopanic xth delay delta flag neutfrac coef limit --How close to 2700?
--  = if value >= 0
--    then value 
--    else -1
--    where
--    value = (findindexoffirst (recurwrap xth delay flag neutfrac coef limit) ((<=) (2700 - delta))) - delay
-- 
-- recurwrap xth delay flag fractionoffull coef limit --is the xth of soft limit you want neutral size to be. E.g. 10th of soft limit would be xth = 10.
--  = valuelist
--    where
--    divisor = xth * 0.5
--    valuelist = map smartfunc [0..limit]
--    smartfunc x = if x >= (1+delay)
--                  then (valuelist!(x-1)) + (recurrence (valuelist!(x-(delay + 1)))) 
--                  else -2700
--    recurrence x | flag == True   = ((2700/divisor) * (((-x)/(2700 * 2)) + 0.5) * coef)              
--                 | x >= 0        = (2700/divisor) * (((-x)/(2700/fractionoffull)) + fractionoffull) 
--                 | otherwise    = (2700/divisor) * (((-x)/(2700/(1 - fractionoffull))) + fractionoffull)
-- 
-- 
-- 
-- ----------------------------------------------


--hftwrapper (Emptyagentstate) [(Arg (Str "Choppy", 0))] [((-1),[],[])] 4

nmhn steps hfts noisies sent = nmhnbs steps hfts noisies sent 0 0 --NiceMimeHftsNoisies

nmhnbs steps hfts noisies sent num_fundbuyer num_fundseller --NiceMimeHftsNoisiesBuyersSellers
   = sim steps specargs specagents
     where
     specargs = if or [(sent == "Choppy"), (sent == "Calm"), (sent == "Toxic"), (sent == "Ramp")] 
                then [(Arg (Str sent, 1))] ++ (rep (hfts + noisies) (Arg (Str "HFT", 4))) ++ (rep num_fundseller (Arg (Str "FundSeller", 16))) ++ (rep num_fundbuyer (Arg (Str "FundBuyer", 4))) ++ (rep num_interm (Arg (Str "Intermediary", 4))) ++ (rep num_opp (Arg (Str "Opportunistic", 4))) ++ (rep num_small (Arg (Str "Small", 4))) ++ [(Arg (Str "autostats", 0))] 
                else error"nmvshft - sentiment not recognised."
     specagents = [(nice_mime_wrapper, [0])] ++ (rep noisies (noisywrapper,[0])) ++ (rep hfts (hftwrapper, [0])) ++ (rep (total) (traderagent, [0]))
     total = num_interm + num_hft + num_fundbuyer + num_fundseller + num_small + num_opp + num_sst + num_bst + num_bsm + num_ssm

     num_interm = 0      -- old value was 3
     num_hft = 0         -- old 3
     num_small = 0       -- old 4
     num_opp = 0         -- old 4
     num_sst = 0         -- old 0
     num_bst = 0         -- old 0
     num_bsm = 0         -- old 0
     num_ssm = 0         -- old 0


testmesimple = sim 2 myargs newtestagents
               where
               myargs = [(Arg (Str "Choppy", 0))] --0 means emptylob, 1 means primed_emptylob.

testnms = sim 2 myargs newtestagentswm
          where
          myargs = [(Arg (Str "Choppy", 0))] --0 means emptylob, 1 means primed_emptylob.


testme = sim 100 myargs testagents
         where
         myargs = [(Arg (Str "Calm", 1))] ++ (rep num_hft (Arg (Str "HFT", 4))) ++ (rep num_fundseller (Arg (Str "FundSeller", 4))) ++ (rep num_fundbuyer (Arg (Str "FundBuyer", 4))) ++ (rep num_interm (Arg (Str "Intermediary", 4))) ++ (rep num_opp (Arg (Str "Opportunistic", 4))) ++ (rep num_small (Arg (Str "Small", 4))) ++ [(Arg (Str "autostats", 0))]


testnm = sim 100 myargs testagentswmime
         where
         myargs = [(Arg (Str "Calm", 1))] ++ (rep num_hft (Arg (Str "HFT", 4))) ++ (rep num_fundseller (Arg (Str "FundSeller", 4))) ++ (rep num_fundbuyer (Arg (Str "FundBuyer", 4))) ++ (rep num_interm (Arg (Str "Intermediary", 4))) ++ (rep num_opp (Arg (Str "Opportunistic", 4))) ++ (rep num_small (Arg (Str "Small", 4))) ++ [(Arg (Str "autostats", 0))]


testagents :: [(Agent_t, [Int])] --Each agent also contains a list of the broadcast id's they are subscribed to. Agent ID is inferred from its index in this list.
testagents = [(exchagent, [0])]  ++ (rep (total) (traderagent, [0]))
testagentswmime :: [(Agent_t, [Int])] --Each agent also contains a list of the broadcast id's they are subscribed to. Agent ID is inferred from its index in this list.
testagentswmime = [(nice_mime_wrapper, [0])]  ++ (rep (total) (traderagent, [0]))
total = num_interm + num_hft + num_fundbuyer + num_fundseller + num_small + num_opp + num_sst + num_bst + num_bsm + num_ssm

num_interm = 3      -- old value was 3
num_hft = 3         -- old 3
num_fundbuyer = 2   -- old 2
num_fundseller = 2  -- old 2
num_small = 4       -- old 4
num_opp = 4         -- old 4
num_sst = 0         -- old 0
num_bst = 0         -- old 0
num_bsm = 0         -- old 0
num_ssm = 0         -- old 0

newtestagents :: [(Agent_t, [Int])]
newtestagents = [(exchagent, [0]), (testagent1, [0]), (testagent2, [0])]

newtestagentswm :: [(Agent_t, [Int])]
newtestagentswm = [(nice_mime_wrapper, [0]), (testagent1, [0]), (testagent2, [0])]

howniceisnicemime = sim 2 someargs someagents
                    where
                    someargs = [(Arg (Str "Calm", 1)), (Arg (Str "FundSeller", 4)), (Arg (Str "FundBuyer", 4))] ++ [(Arg (Str "autostats", 0))]
                    someagents = [(nice_mime_wrapper, [0]), (testagent1, [0]), (testagent2, [0])]



--funTest = [datecall (f 3), datecall (f 5)]
--          where
--          f x = 23, if x <4
--          f x = 23, otherwise

--anotherfuntest x = valssss
--                   where
--                   valssss = (systemTime (# x))

--(Arg (Str "HFT<X>iseed", y))
--
--hftwave 2700 [2000, 2000, 2000, 2000, 5800] 1
--hftwave 10 [10, -9] 0 0 1

--hftwave :: num -> [num] -> num -> num -> num -> [sys_message]
hftwave softlimit seedvents stubsize pstop amxcoef
  = do 
    createDirectory ("messyoutputfolder/"++thefilename)    
    sim 500 meinargs meinagents                                     
  
  
    where
    meinargs = [(Arg (Str "Calm", 1)), (Arg (Str "Randomise", 1)), (Arg (Str fthefilename, 9989793425))]
               ++ delayarg ++ softlimitarg ++ fakeinventargs ++ stubsarg ++ stubsizearg ++ rsarg ++ psarg ++ amxarg ++ [(Arg (Str "PeriodicLiquidity", 6))]
    fthefilename = "messyoutputfolder/" ++ thefilename ++ "/" ++ thefilename
    thefilename = "seededHFTwave" ++ (foldl ((++).(++ "-")) "" (map show seedvents)) ++ "-stsz" ++ (show stubsize) ++ "-sflm" ++ (show softlimit) ++ "-pstp" ++ (show pstop) ++ "-amxcoef" ++ (show (amxcoef * 1000))
    delayarg = [(Arg (Str "Delay", 0))]
    fakeinventargs = makeargs (take (length seedvents) (map ((++ "HFTiseed").(show)) [2..])) seedvents
    makeargs [] [] = []
    makeargs (mystr:sr) (myinv:ir) = (Arg (Str mystr, myinv)) : makeargs sr ir
    meinagents = [(nice_mime_wrapper, [0])] ++ (rep (length seedvents) (hftwrapper, [0])) ++ thetestagent
    stubsarg = rep 1 (Arg (Str "StubsEnabled", 1))
    stubsizearg = rep 1 (Arg (Str "Stubsize", stubsize))
    softlimitarg = [(Arg (Str "SoftLimit", softlimit))]
    rsarg = [(Arg (Str "randseed", (systemTime  (length seedvents))))]
    psarg = [(Arg (Str "ProbeStop", pstop))] --probestop arg
    amxarg = [(Arg (Str "amxcoef", amxcoef))]
    thetestagent = if pstop /= 0
                   then (rep 1 (testagent1,[0]))
                   else []

--hftwaveD :: num -> [num] -> num -> num -> num -> [sys_message]
hftwaveD softlimit seedvents delay pstop amxcoef
  = do                                                  
    createDirectory ("messyoutputfolder/"++thefilename) 
    sim 500 meinargs meinagents                         
 
    where
    meinargs = [(Arg (Str "Calm", 1)), (Arg (Str "Randomise", 1)), (Arg (Str fthefilename, 9989793425))]
               ++ delayarg ++ softlimitarg ++ fakeinventargs ++ stubsarg ++ stubsizearg ++ rsarg ++ psarg ++ amxarg ++ [(Arg (Str "PeriodicLiquidity", 6))]
    fthefilename = "messyoutputfolder/" ++ thefilename ++ "/" ++ thefilename
    thefilename = "seededHFTwaveD" ++ (foldl ((++).(++ "-")) "" (map show seedvents)) ++ "-D" ++ (show delay) ++ "-sflm" ++ (show softlimit) ++ "-pstp" ++ (show pstop) ++ "-amxcoef" ++ (show (amxcoef * 1000))
    delayarg = [(Arg (Str "Delay", delay))]
    fakeinventargs = makeargs (take (length seedvents) (map ((++ "HFTiseed").(show)) [2..])) seedvents
    makeargs [] [] = []
    makeargs (mystr:sr) (myinv:ir) = (Arg (Str mystr, myinv)) : makeargs sr ir
    meinagents = [(nice_mime_wrapper, [0])] ++ (rep (length seedvents) (hftwrapper, [0])) ++ thetestagent
    stubsarg = []
    stubsizearg = rep 1 (Arg (Str "Stubsize", 0))
    softlimitarg = [(Arg (Str "SoftLimit", softlimit))]
    rsarg = [(Arg (Str "randseed", (systemTime (length seedvents))))]
    psarg = [(Arg (Str "ProbeStop", pstop))] --probestop arg
    amxarg = [(Arg (Str "amxcoef", amxcoef))]
    thetestagent = if pstop /= 0
                   then (rep 1 (testagent1,[0]))
                   else []

testmeApril2014 = hftwaveD 2700 [0,3000,0,-3000] 3 0 1


--hftwaveHD :: num -> [num] -> num -> num -> [sys_message]
hftwaveHD softlimit seedvents delay hetero
  = do                                                   
    createDirectory ("messyoutputfolder/"++thefilename)  
    sim 200 meinargs meinagents                          
    --sim 400 meinargs meinagents   --longer test
  
    where
    meinargs = [(Arg (Str "Calm", 1)), (Arg (Str "Randomise", 1)), (Arg (Str fthefilename, 9989793425))]
               ++ delayarg ++ softlimitarg ++ fakeinventargs ++ stubsarg ++ stubsizearg ++ rsarg ++ psarg ++ amxarg ++ heteroarg ++ [(Arg (Str "PeriodicLiquidity", 6))]
    fthefilename = "messyoutputfolder/" ++ thefilename ++ "/" ++ thefilename
    thefilename = "seededHFTwaveHD" ++ (foldl ((++).(++ "-")) "" (map show seedvents)) ++ "-D" ++ (show delay) ++ "-sflm" ++ (show softlimit) ++ "-hetero" ++ (show hetero)
    delayarg = [(Arg (Str "Delay", delay))]
    fakeinventargs = makeargs (take (length seedvents) (map ((++ "HFTiseed").(show)) [2..])) seedvents
    makeargs [] [] = []
    makeargs (mystr:sr) (myinv:ir) = (Arg (Str mystr, myinv)) : makeargs sr ir
    meinagents = [(nice_mime_wrapper, [0])] ++ (rep (length seedvents) (hftwrapper, [0])) ++ thetestagent
    stubsarg = []
    stubsizearg = rep 1 (Arg (Str "Stubsize", 0))
    softlimitarg = [(Arg (Str "SoftLimit", softlimit))]
    rsarg = [(Arg (Str "randseed", (systemTime (length seedvents))))]
    psarg = [(Arg (Str "ProbeStop", 0))] --probestop arg
    amxarg = [(Arg (Str "amxcoef", 1))]
    heteroarg = [(Arg (Str "hetero", hetero))] -- 1 if heterogeneity is enabled, 0 otherwise
    thetestagent = []  
    
--testmeHetero = hftwaveHD 2700 [0, 3000, 0, 3000, 0,3000,0,3000] 1 1  --one more agent
testmeHetero = hftwaveHD 2700 [0, 3000, 0, 3000, 0] 1 1      
--testmeHetero = hftwaveHD 2700 [0, 3000] 1 1
testmeHeteroPairs = hftwaveHD 2700 [0,5000,10000,(-6000),(-6000)] 1 1
                                                                      