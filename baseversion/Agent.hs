
module Agent where

--System includes                     
----------------------------------
                                    
import Messages  
import Sim_lob
import Traders(generic_trader) 
import Order
import Comm      
                                    
--User includes                       
----------------------------------
                                    
  
        
        
                                    
--  %insert "./nicemime.m"             
--  %insert "./Hft.m"                  
--  %insert "./LaggedHft.m"            
--  %insert "./Noiseagent.m"           
--  %insert "./Fundamentals.m"    



--The agent and agentstate types
---------------------------------------


--The parameters for agents                                                                                          
---------------------------                                                                                          
                                                                                                                   
maxorder = 2000

timestep = 0.04 --This might need to be changed to 0.00425
stepstoseconds s = s * timestep
secondstosteps s = s/timestep



  










--Helper functions (for this particular case)
-------------------------------------------------------

gs = gaussians
--bfilter :: [msg_t] -> [broadcast_t]
--bfilter (() : ys)
find x list = realfind x list 0
              where
              realfind x [] n = error "Couldn't find the item in the list..."
              realfind x (f : r) n = if f == x 
                                     then n
                                     else realfind x r (n+1)


traderagent_applyuids uids orders = if (null orders)
                                    then (uids, [])
                                    else(remaininguids, fixedorder : rest)
                                    where
                                    fixedorder = (safehd uids "fixedorder1") (safehd orders "fixedorder2")
                                    (remaininguids, rest) = traderagent_applyuids (tail uids) (tail orders)


isOtype typestr or = result
                     where
                     result = check (order_gettype or)
                              where
                              check (Offer x) | typestr == "Offer"   = True
                              check (Bid x)   | typestr == "Bid"     = True
                              check (Sell x)  | typestr ==  "Sell"   = True
                              check (Buy x)   | typestr == "Buy"     = True
                              check x = False

isBuySide order = if (or [isOtype "Bid" order, isOtype "Buy" order])  
                  then True 
                  else False
isSellSide order = if (or [isOtype "Offer" order, isOtype "Sell" order]) 
                   then True
                   else False

                  
--systemTime x
--      = (numval timeinseconds) mod 887
--        where
--        (time, st, ste) = datecall x
--        timeinseconds = time -- "\n"
--
--datecall x = system ("date '+%s'" ++ (concat (rep (x mod 249) " ")))
              


delta onbook wantedonbook = if wantedonbook >= onbook 
                            then wantedonbook - onbook
                            else 0

tan x = (sin x) / (cos x)
--mymax a b = max [a,b]
--mymin a b = min [a,b]

--round x = entier x, if (x - (entier x)) < 0.5
--        = (entier x) + 1, otherwise

applyuids uids orders = if (null orders) 
                        then (uids, []) 
                        else (remaininguids, fixedorder : rest)
                        where
                        fixedorder = (safehd uids "fixedorder1") (safehd orders "fixedorder2")
                        (remaininguids, rest) = applyuids (tail uids) (tail orders)
                        
                        
                        
between x low high = if (x >= low) && (x <= high)
                     then True 
                     else False                            
-------------------------------------------------------                                          


-- The collection of agents
------------------------------------

--exchagent's ID will be ID 1.

--price 2000 size 100 bid (price, size, time, traderid, ordertype)
testagent1 :: Agent_t
testagent1 mystate args []     myid                = []
testagent1 mystate args ((time, messages, broadcasts) : simstateinfos) myid
   = (probeorder ++ liquid) : (testagent1 mystate args simstateinfos myid)
     where
     liquid | (arg_findval "PeriodicLiquidity" args) == -1     = []
            | (mod time 70) == 44   = [(ordermessage (myid, 1) (order_setuid "ta1" 1 (order_create (ltp - 1) 1 time (FundSeller myid) (Bid (Goodtilldate time))))), (ordermessage (myid, 1) (order_setuid "ta1" 1 (order_create (ltp + 1) 1 time (FundSeller myid) (Offer (Goodtilldate time)))))] 
--             = rep 1 (ordermessage (myid, 1) (order_setuid "ta1" 1 (order_create (ltp - 12) 2000 time (FundSeller myid) (Bid (Goodtilldate time))))), if time mod 70 = 44
            | otherwise = [] --[(cancelmessage (myid, 1) (myid, 1))], otherwise
     probeorder = if time <= teststop
                  then [(ordermessage (myid, 1) (order_setuid "ta1" 1 (order_create 2000 probesize time (FundSeller myid) (Sell Andkill))))] 
                  else []
     probesize = if (arg_findval "ProbeSize" args) /= (-1) 
                 then arg_findval "ProbeSize" args  -- CDC 17/08/12
                 else 100
     teststop = if (arg_findval "ProbeStop" args) /= (-1)
                then round $arg_findval "ProbeStop" args
                else 140                              
     [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or, ltp] = if time <= 0
                                                           then [1993, 2007, 500, 500, 1, 1, 500, 500, 2, 2000]  --[bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or, ltp]
                                                           else msg_getnumlistfrombcast (findG0bcast broadcasts)
                                                           where
                                                           findG0bcast [] = error ("No broadcast found?" ++ (show time))
                                                           findG0bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 0)
                                                                               then f 
                                                                               else findG0bcast r

testagent2 :: Agent_t
testagent2 mystate args []                     myid = []
testagent2 mystate args ((time, messages, broadcasts) : simstateinfos) myid = [(ordermessage (myid, 1) (order_setuid "ta2" 3 (order_create 0 100 time (FundBuyer myid) (Buy Andkill)))), (ordermessage (myid, 1) (order_setuid "ta22" 4 (order_create 1850 100 time (FundBuyer myid) (Bid Goodtillcancelled))))] : (testagent2 mystate args simstateinfos myid)
       
 
spikeagent :: Agent_t
spikeagent mystate args ((time, messages, broadcasts) : simstateinfos) myid
  = (liquidity ++ curativeliquidity):(spikeagent mystate args simstateinfos myid)
    where
    liquidity = if time == 0 
                then rep 20 (ordermessage (myid, 1) (order_setuid "sp1" 1 (order_create (ltp + 12) maxorder time (FundSeller myid) (Offer (Goodtillcancelled)))))
                else []
    curativeliquidity = if findG1bcast broadcasts
                        then rep 10 (ordermessage (myid, 1) (order_setuid "sp1" 1 (order_create (ltp + 1) maxorder time (FundSeller myid) (Offer (Goodtillcancelled))))) ++ rep 20 (ordermessage (myid, 1) (order_setuid "sp1" 1 (order_create (ltp - 1) maxorder time (FundSeller myid) (Bid (Goodtillcancelled))))) 
                        else []
    
    findG1bcast [] = False
    findG1bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 1) 
                        then True 
                        else findG1bcast r                  
    [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, ors, ltp] = if time <= 0
                                                           then [1993, 2007, 500, 500, 1, 1, 500, 500, 2, 2000]  -- [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or, ltp]
                                                           else msg_getnumlistfrombcast (findG0bcast broadcasts)
                                                           where
                                                           findG0bcast [] = error ("No broadcast found?" ++ (show time))
                                                           findG0bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 0) 
                                                                               then f 
                                                                               else findG0bcast r  
                                                                               

-- THE EXCHANGE AGENT
exchagent (Emptyagentstate) args ((time, messages, broadcasts) : simstateinfos) myid = [message (myid,0) [Arg (Str (showlob initiallob), 0)], broadcastmessage (myid,0) (broadcast_numlist (getlobsummary initiallob))] : (exchagent (Exchstate initiallob) args simstateinfos myid)
                                                                                        where
                                                                                        initiallob = lob_setsentiment getsent mylob --primed_emptylob
                                                                                                     where
                                                                                                     mylob | (arg_getnum (args!!0)) == 0   = emptylob
                                                                                                           | (arg_getnum (args!!0)) == 1   = primed_emptylob 
                                                                                                           | otherwise                     = error "exchagent - primary lob not recognised"
                                                                                                     getsent|(arg_getstr (args!!0)) == "Calm"       = Calm 
                                                                                                            |(arg_getstr (args!!0)) == "Choppy"     = Choppy
                                                                                                            |(arg_getstr (args!!0)) == "Ramp"       = Ramp
                                                                                                            |(arg_getstr (args!!0)) == "Toxic"      = Toxic
                                                                                                            | otherwise                             = error "Unrecognised sentiment."
                                                                              
exchagent (Exchstate curlob) args ((time, messages, broadcasts) : simstateinfos) myid 
          = ([datamessage (myid, 0) trdata, message (myid,0) [Arg (Str (showlob nextlob), 0)], broadcastmessage (myid,0) (broadcast_numlist (getlobsummary nextlob))] ++ (tdtomsg trades)) : (exchagent (Exchstate nextlob) args simstateinfos myid)
            where
            tdtomsg [] = []
            tdtomsg ((o1, o2) : rest) = [(trademessage (myid, order_gettraderidno o1) o1 o2), (trademessage (myid, order_gettraderidno o2) o2 o1)] ++ tdtomsg rest
            (nextlob, trades, trdata) = (a,lob_gettrades a,lob_gettrace a) -- (a,b,lob_gettrace a)
                                        where
                                        a = foldr match ((lob_increment_time.lob_clear_trades) curlob) orders

                                     --   (a,b) = foldr setret (((lob_increment_time.lob_clear_trades) curlob), []) orders
                                     --   setret ordxr (loba, tradesxa) = (lobb, (lob_gettrades lobb) ++ tradesxa)
                                     --                                           where
                                     --                                           lobb = (match ordxr loba)
                                        orders = (filterorders messages)
                                                 where
                                                 filterorders qx = msg_getorders qx
                                                 
                                                 
                                                 
traderagent (Emptyagentstate) args any myid = traderagent (Traderstate (0, 0, 0, getsent, 0, (map (order_setuid "traderagent") [0..]))) args any myid
                                              where
                                              getsent |(arg_getstr (args!!0)) == "Calm"       = Calm 
                                                      |(arg_getstr (args!!0)) == "Choppy"     = Choppy
                                                      |(arg_getstr (args!!0)) == "Ramp"       = Ramp 
                                                      |(arg_getstr (args!!0)) == "Toxic"      = Toxic
                                                      | otherwise                             = error "Unrecognised =sentiment."
                                              
traderagent (Traderstate (obb, obo, oon, sent, invent, uids)) args ((time, messages, broadcasts) : simstateinfos) myid 
     = if bequiet == True 
       then [] : (traderagent (Traderstate ((xsum!!0), (xsum!!1), (xsum!!8), sent, newinvent, remaininguids)) args simstateinfos myid)  -- shall I cast the ordernum to Int?
       else (ordstomsg (map fixorder fordxr)) : (traderagent (Traderstate ((xsum!!0), (xsum!!1), (xsum!!8), sent, newinvent, remaininguids)) args simstateinfos myid)
       where
       bequiet | quiettime == (-1)                = False
               | quiettime <= time                = True 
               | otherwise                        = False

       quiettime = round (arg_getnum (args!!(myid-1)))
       (remaininguids, fordxr) = traderagent_applyuids uids [ordxr]
       newinvent = foldr (+) invent (map getmovement mytrades)
                   where
                   mytrades = map (cpsafehd "traderagent") (filter (not.null) (map msg_gettrade messages))
                   getmovement o |(or [otype == (Bid Goodtillcancelled), otype == (Buy Andkill)])          = osize    
                                 |(or [otype == (Sell Andkill), otype == (Offer Goodtillcancelled)])       = (- osize) 
                                 | otherwise                                                               = 0
                                     where
                                     osize = order_getsize o
                                     otype = order_gettype o


       ordstomsg o = map (ordermessage (myid, 1)) o
       ordxr = generic_trader mytraderid randoms gs xsum (obb, obo, oon, sent, time, newinvent)
               where
               mytraderid|(arg_getstr (args!!(myid-1))) == "HFT"             = (HFT myid)  -- -2 assuming first trader is at index 2 in agentlist
                         |(arg_getstr (args!!(myid-1))) == "FundBuyer"       = (FundBuyer myid)
                         |(arg_getstr (args!!(myid-1))) == "FundSeller"      = (FundSeller myid)
                         |(arg_getstr (args!!(myid-1))) == "Intermediary"    = (Intermediary myid)
                         |(arg_getstr (args!!(myid-1))) == "Opportunistic"   = (Opportunistic myid)
                         |(arg_getstr (args!!(myid-1))) == "Small"           = (Small myid) 
                         | otherwise                                         = error "traderagent - unrecognised trader category"
       fixorder x | (pr >= (ltp - 12)) && (pr <= (ltp + 12))   = x
                  | pr < (ltp - 12)                            = order_setprice (ltp - 12) x
                  | pr > (ltp + 12)                            = order_setprice (ltp + 12) x
                    where
                    pr = order_getprice x
                    ltp = last xsum
       xsum = if time <= 0
              then [1993, 2007, 500, 500, 1, 1, 500, 500, 2, 2000]  -- rep 9 0  [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or]
              else  msg_getnumlistfrombcast (findG0bcast broadcasts)
              where
              findG0bcast [] = error ("No broadcast found?" ++ (show time))
              findG0bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 0)
                                  then f
                                  else findG0bcast r                                                
                                                               
                                                               
                                                               

                                                               
                                                      
                                                                                                
                                                                                                






-- Here is the start of HFT  
-----------------------------------------------------------------------------------------






-- here is the start of LaggedHft
-----------------------------------------------



-- Here is the start of Noiseagent
--------------------------------------------------


--Here is the start of Fundamentals
----------------------------------------------



                                                                            