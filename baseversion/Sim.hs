module Sim where

import Comm
import Stats
import Messages
import System.IO
import Data.List
import Data.Char




assetreturnscoln = 27  
--The main simulator
----------------------------------
--Args are (string,value) pairs with numeric values - these are passed on to simstep, sim_updatestate and agents
--The "agents" parameter to sim is a list of functions, which are applied to their arguments inside sim
--Once applied to their full set of arguments, agents consume the potentially-infinite stream of simulator system states ("allstates")
--and each produces a potentially-infinite stream of lists of messages to be sent to other agents (according to the target id in the message).
--Notice that at each timestep an agent can produce a list of messages - because one agent might want to simultaneously send messages to several different 
--other agents.
--They do this in a recursive loop - it is important that (i) there is a precomputed first state for agents to view, (ii) the
--agents only ever look at the head of the incoming list of states each time around their own loop, and (iii) the "simstep" function
--only ever looks at the head of the message lists from the agents each time around its own loop.  Any attempt to lookahead will cause the simulation
--to deadlock.
--Any message sent to target id "0" is a message for the simulator harness and is used to update the simulator state - which is
--printed to the trace file and is also visible to all other agents.
--Each agent has its own private state. 
--The last arg to sim is the required highest group number for broadcast messages

sim :: Int -> [Arg_t] -> [(Agent_t, [Int])] -> IO()
sim steps args agents = do
                        trace_outh<-openFile (prefix ++ "trace") WriteMode
                        data_outh<- openFile (prefix ++ "data.csv") WriteMode
                        hSetBuffering trace_outh (BlockBuffering Nothing)     -- set buffer mode
                        hSetBuffering data_outh  (BlockBuffering Nothing)     -- set buffer mode
                        
                        --start output
                        hPutStr trace_outh ("START OF SIMULATION - seed is " ++ seedmsg ++ "\nNB System Time 2 aligns with Exchange Time 1\n\n")   --need to confirm the recursive behaviour
                        tracer allstates trace_outh data_outh                        
                        statstuff
                        
                        where 
                        statstuff = if (arg_findval "autostats" args) /= -1
                                    then (printstatoutput (statfuncs (prefix ++ "data.csv") assetreturnscoln True) (prefix ++ "-asset_returns-")) 
                                    else  return()   --do nothing
                        highestbrg = maximum ((concat (map snd agents)) ++ [0]) --The zero is incase there are no agents subscribed to any broadcast groups.
                        myrands = if (arg_findval "randseed" args) /= -1
                                  then drop (pmh + (length agents)) randoms 
                                  else drop ((round (arg_findval "randseed" args)) * (pmh + (length agents))) randoms
                        seedmsg = if (arg_findval "randseed" args) == (-1)
                                  then "default"
                                  else show (arg_findval "randseed" args)
                        pmh = (sum (map ord prefix)) --Poor Man's Hash (function)'
                        prefix = if (arg_findstr 9989793425 args) /= ""
                                 then (arg_findstr 9989793425 args) ++ "-"
                                 else  ""
                        startsimstate = sim_emptystate agents highestbrg myrands
                        allstates = startsimstate : simstates
                        simstates = simstep steps 1 args startsimstate allmessages myrands
                        allmessages = map f (zip [1..] agents)
                                        where
                                        -- f is a function that fetches the messages and broadcasts for a given agent (using sim_getmymessages and sim_getmybroadcasts)
                                        -- and then applies the agent to (i) the empty state; (ii) the args (that are sent to the simulator at the start);
                                        -- (iii) an infinite list of lists of messages and broadcasts, and (iv) the agent id.
                                        -- It fetches the messages trivially by using the agent's ID to index into the message list
                                        -- (which is sorted by destination ID for each message).
                                        -- It fetches the broadcasts for an agent as follows:
                                        -- - each agent is subscribed to a list of broadcast groups (this is the data called brcs)
                                        -- - for each subscribed broadcast group ID, that ID is used to index into the broadcast list (which is
                                        --   held in the state called st, and which is sorted into broadcast groups).  The indexing is done by sim_getmybroadcasts.
                                        -- - all of the returned broadcast lists (for different broadcast group IDs) are concatenated into a single list.
                                        f :: (Int,(Agent_t, [Int]))  -> [[Msg_t]]
                                        f (id,(a, brcs)) = a emptyagentstate args (map (g id) allstates)  id
                                                           where
                                                           g x st = ((fromIntegral $sim_gettime st), sim_getmymessages st x, concat (map (sim_getmybroadcasts st) brcs)) 
                                                                     
                        simstep n t args st []   myrnds   = []                                                  -- for completeness - should never happen
                        simstep 0 t args st msgs myrnds   = []                                                  -- end of simulation steps
                        simstep n t args st msgs myrnds   = if (elem msgs [])
                                                            then []                              -- end simulation if any agent ends
                                                            else newstate : (simstep (n-1) (t+1) args newstate (map tail msgs) (drop t myrnds))
                                                            where
                                                            newstate = sim_updatestate t args cleanst (map (cpsafehd "simstep") msgs) myrnds  -- t is the "time" - the simulation step
                                                            cleanst = sim_clean_msg_br st 
                        
                        



--Tracer can take an argument to send its output to specified files. The argument is detailed below.
--Presence of (Arg (String prefix, 9989793425)) indicates files should be written to the messyoutputfolder folder in the files prefixed by the specified prefix.
--E.g. if prefix is "MyLatestTest" files will be written to folder "MyLatestTest" and the files will be called "MyLatestTest-data.csv" and "MyLatestTest-trace".
--The number 9989793425 acts as a key so that the HFT agent can find the value (the prefix).

tracer :: [Simstate_t] -> Handle->Handle -> IO()
tracer [] traceh datah  = do
                          hPutStr traceh ("END OF SIMULATION\n")
                          
                          hClose traceh
                          hClose datah
                        
tracer (x:xs) traceh datah = do
                             hPutStr traceh ((showsimstate_t x) ++ "\n==============\n")    --showsimstate_t ?
                             datatraces
                             tracer xs traceh datah
                             where
                             datatraces = hPutStr datah dtracepostformatting                                                                                              
                             dtracepostformatting = if dtracepreformatting /= ""
                                                    then (show (sim_gettime x)) ++ ", " ++ dtracepreformatting ++ "\n"    -- time added for debugging CDC 8/4/2014 
                                                    else (show (sim_gettime x)) ++ ", " ++ dtracepreformatting                                                                     
                             dtracepreformatting = ((concat (map msg_disptrace (filter msg_isdata (sim_getmymessages x 0)))))   
                             
                                                                                           


--The abstract type definition for simulator state
------------------------------------------------------------------

--abstype simstate_t
--with
--    sim_emptystate :: [(agent_t,[num])] -> num -> [num] -> simstate_t                             || creates an empty message stream for each agent
--    sim_updatestate :: num -> [arg_t] -> simstate_t -> [[msg_t]] -> [num] -> simstate_t   || time, args, oldst, one msg_t per agent
--    showsimstate_t :: simstate_t -> [char]
--    sim_gettime :: simstate_t -> num
--    sim_getmymessages :: simstate_t -> num -> [msg_t] ||num is id
--    sim_getmybroadcasts :: simstate_t -> num -> [msg_t] ||num is groupid
--    sim_clean_msg_br :: simstate_t -> simstate_t
--    || and so on - we will need access methods
--

--The implementation of simulator state
--It must implement methods sim_emptystate, sim_updatestate and showsimstate_t

type Simstate_t = ([[Msg_t]], Int, [Msg_t], [[Msg_t]])     
                                        -- list of messages for each agent - a snapshot at time t - plus time and other info

sim_emptystate :: [(Agent_t,[Int])] -> Int -> [Int] -> Simstate_t
sim_emptystate []     hbrg rnds = ([[]], 0, [(debugmessage (0,0) (show (take 30 rnds)))], (map f [0..hbrg]))            -- this base case gives the extra list of messages for agent id which is the sim harness
                                  where
                                  f x = []
sim_emptystate (a:as) hbrg rnds = (([]:x), b, c, br)
                                  where
                                  (x,b,c,br) = sim_emptystate as hbrg rnds

sim_clean_msg_br :: Simstate_t -> Simstate_t 
sim_clean_msg_br (m, b, c, br) = (emptym, b, c, emptybr)
                                 where
                                 emptym = map f m 
                                 emptybr = map f br
                                 f x = [] 

sim_updatestate :: Int -> [Arg_t] -> Simstate_t -> [[Msg_t]] -> [Int] -> Simstate_t                                 
sim_updatestate t args (m, b, c, br) [] myrands     = (newm, t, safehd newm "sim_updatestate", reverse br2) --just copy over broadcast??
                                                      where
                                                      newm = reverse (map reverse m2)
                                                      m2 = if randomise == True 
                                                           then ((map (randomWrap myrands) (take ((length m) - 1) m)) ++ (drop ((length m) - 1) m))
                                                           else m
                                                      br2 = if randomise == True
                                                            then (map (randomWrap myrands) br)
                                                            else br
                                                      randomise = if (arg_findval "Randomise" args) /= (-1)
                                                                  then True
                                                                  else False
sim_updatestate t args (m, b, c, br) (x:xs) myrands
                                             = sim_updatestate t args (fm, b, c, fcasts) xs (drop 72 myrands)
                                               where
                                               --updatemsgs takes messages from agents and puts them in msgqueues for agents (id 0 = for sim)
                                               updatemsgs 0 (y:ys) = [(filter ((==0).msg_getid) (filter ((not).msg_isbroadcast) x)) ++ y]
                                               updatemsgs n (y:ys) = ((filter ((==n).msg_getid) (filter ((not).msg_isbroadcast) x)) ++ y): (updatemsgs (n-1) ys)
                                               updatecasts 0 (y:ys) = [(filter ((==0).msg_getid) (filter msg_isbroadcast x)) ++ y]
                                               updatecasts n (y:ys) = ((filter ((==n).msg_getid) (filter msg_isbroadcast x)) ++ y): (updatecasts (n-1) ys)
                                               newm =  (updatemsgs ((length m) - 1) ( m))
                                               newcasts = (updatecasts ((length br) - 1) ( br)) --These guys need to know the number of broadcast groups...
                                               --     (fm, fcasts) = (((map (randomWrap myrands) (take newml newm)) ++ (drop newml newm)),(map (randomWrap myrands) newcasts)), if randomise = True
                                               (fm, fcasts) = (newm, newcasts)
                                               -- newml = (# newm) - 1
                                               -- randomise = True, if (arg_findval "Randomise" args) ~= (-1)
                                               --           = False, otherwise
                                               --
                                                                                                      
showsimstate_t :: Simstate_t -> [Char]

--showsimstate_t (m,b,c,br)  = ", \n\nSystem Time: "++t++"\nMessages to sim: "++m2s++", \n\nHarness messages: " ++ h ++ "\n\nBroadcasts: "++bm++"\n\n\nEND OF STATE\n"  --for profiling
--                             where
--                             t = show b
--                             bm = (concat (map (concat.(map showmsg_t)) (drop 1 br)))   
--                             h  = (concat (map showmsg_t c))                        
--                             m2s = concat msg2Sim
--                             msg2Sim = [f] ++ g
--                                       where
--                                       f = (concat.(map showmsg_t)) (head (drop 1 m))
--                                       g = map (concat.(map showmsg_t)) (tail (drop 1 m))
                             
                             
                             
                              
showsimstate_t (m,b,c,br) = ", \n\nSystem Time: "++(show b)++"\nMessages to sim: "++(concat (map (concat.(map showmsg_t)) (drop 1 m)))++", \n\nHarness messages: " ++ (concat (map showmsg_t c)) ++ "\n\nBroadcasts: "++(concat (map (concat.(map showmsg_t)) (drop 1 br)))++"\n\n\nEND OF STATE\n"  --original

--Dropping 1 from m to avoid printing harness messages twice.
--showsimstate_t (m,b,c,br) = ", \n\nTime: "++(show b)++"\nMessages to sim: "++(concat (map (concat.(map show)) m))++", \n\nUnknown: "++(show c)++", \n\nBroadcasts: "++(show br)++"\n\n\nEND OF STATE\n" OLD UNKNOWN FIX ME
--Only index 2 of messages and br is populated and for some reason only one message is retained...
                           
sim_gettime :: Simstate_t -> Int                           
sim_gettime (m, b, c, br) = b
      
sim_getmymessages :: Simstate_t -> Int -> [Msg_t]       
sim_getmymessages (m,b,c,br) idnum = si "sim:1" m idnum

sim_getmybroadcasts :: Simstate_t -> Int -> [Msg_t]
sim_getmybroadcasts (m,b,c,br) groupnum = si "sim:2" br groupnum


--Helper functions
-------------------------------------------

randomWrap myrands list 
                = randomize (map ((* coef).(converse (-) 500)) myrands) list
                  where
                  coef = if len < 500
                         then 1
                         else (floor (len / 500)) + 1    --entier== floor
                  len = fromIntegral $length list
                                  
randomize rands [] = []
randomize (r:rs) msgs
 =  a : (randomize rs b)
    where
    a = msgs !! ( mod r len)
    len = length msgs
    b = listsub msgs  [a]
    
    
listsub :: Eq a=>[a]->[a]->[a]

listsub x [] = x
listsub x (b:y) =  listsub (delete b x) y
