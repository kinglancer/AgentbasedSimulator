module Traders(generic_trader) where

import Comm
import Order


----------------------------------
--TRADERS
--
--Traders are characterised by 
--(i)  their target inventory level; 
--(ii) their increased demand for or supply of securities at each simulated time step; and
--(iii) four functions that determine their trading behaviour �C 
--	(a) according to the gap between current price and current underlying value; 
--	(b) according to the gap between current inventory and target inventory; 
--	(c) in response to order imbalance; and 
--	(d) in response to sudden changes in price:
--
--Thus we use the tuple (target, demand, supply, pricefunction, inventoryfunction, orderfunction, volatilityfunction).  
--The trader categories are:
--	Fundamental Buyer          - target inventory=20,000; demand=200; supply=0; pf; invfun; oflow;  vfhigh
--	Fundamental Seller         - target inventory=20,000; demand=0;   supply=0; pf; invfun; oflow;  vfhigh
--	Intermediary (incudes HFT) - target inventory=2,000;  demand=0;   supply=0; pf; invfun; ofhigh; vflow
--	Opportunistic              - target inventory=1,000;  demand=0;   supply=0; pf; invfun; ofmed;  vfmed
--	Small                      - target inventory=500;    demand=0;   supply=0; pf; invfun; ofmed;  vfmed                        


target_inventory (FundBuyer     id) = 20000 
target_inventory (FundSeller    id) = 100 
target_inventory (Intermediary  id) = 1200
target_inventory (HFT           id) = 800
target_inventory (Opportunistic id) = 800
target_inventory (Small         id) = 400

demand (FundBuyer     id) = 200
demand (FundSeller    id) = 0
demand (Intermediary  id) = 0
demand (HFT           id) = 0
demand (Opportunistic id) = 0
demand (Small         id) = 0

supply (FundBuyer     id) = 0
supply (FundSeller    id) = 200
supply (Intermediary  id) = 0
supply (HFT           id) = 0
supply (Opportunistic id) = 0
supply (Small         id) = 0

tradersensitivity (FundBuyer     id) = (oflow, vfhigh)
tradersensitivity (FundSeller    id) = (oflow, vfhigh)
tradersensitivity (Intermediary  id) = (ofhigh, vflow)
tradersensitivity (HFT           id) = (ofhigh, vflow)
tradersensitivity (Opportunistic id) = (ofmed, vfhigh)
tradersensitivity (Small         id) = (ofmed, vfhigh)

-- Support functions
myexp x = if x < 700
          then exp x 
          else 1000            -- error "myexp: arg > 700", otherwise
tangent x = (sin y) / (cos y)
            where
            y = (sigmoid x) * pi
sigmoid x = (1 / (1 + myexp (-abs(x)))) - 0.5        -- gives output between +0.5 and -0.5

-- oflow, ofmed and ofhigh provide low, medium and high sensitivity to order imbalance
-- They take in a number which is the amount of order flow imbalance and they return a multiplier for order size
oflow x = 1
ofmed x =  1-(sigmoid x)
ofhigh x = minimum [myexp x, 40]

-- vflow, vfmed and vfhigh provide low, medium and high sensitivity to volatility (sudden price change)
-- They take in a number which is the change in traded prices since the last timestep and they return a multiplier for order size
vflow x = 1
vfmed x = 1-(sigmoid x)
vfhigh x = minimum [myexp x, 40]

-- pf is the pricing function that ensures traders place orders just ahead of top of book if there is high TOB depth, and behind
-- top of book if there is low TOB depth.
-- pf takes the depth at the top of the book and returns a fraction (must be applied to spread to give offset from TOB price)
-- We assume depth >= 0 at all times and anything over depth=2000 is saturated
-- The output is bounded by -0.25 and +0.25
pf depth = maximum [ minimum [0.25,  curve], -0.25 ]
           where
           depth1 = minimum [depth, 2000]
           curve = 0.25*(tangent (((2*depth1)-2000)/2000))/1.56

-- INVFUN
-- ======
-- The function invfun does two things - it calculates a revised target (based on fundamental target and current price and current value); 
-- and it then calculates an amount to buy or sell based on the revised target and the current inventory.
-- The inputs are the catid, the curent value, the current midprice, and the current inventory.  The function might also take into account
-- the spread (but doesn't do so yet!).  This response function is different for each category of trader.
-- In general terms, if inv < target then buy (+ve amount) else sell (-ve amount)
--
-- Worked example for FundBuyer:
-- x = (value - price)                ------  Thus, if x is positive we are encouraged to buy because the underlying value is higher than the midprice
-- (sigmoid x)                        ------  gives an output between +0.5 (if x is positive) and -0.5 (if x is negative)
-- 1 + (sigmoid x)                    ------  creates a multiplier to give increased buying (if x positive) or selling (if x negative) pressure
-- target_inventory * (1 + sigmoid x) ------  changes the target - an increased target (positive x) means greater pressure to buy
-- y = inv - changed target           ------  the difference between current inventory and target inventory; buy if y is negative
-- targetsize                         ------  the target size of a single order
-- amount = targetsize - (exp(y/150)) ------  if y negative, (exp(y/150)) is very small so amount is positive and close to target size
--                                            if y positive and (exp(y/150))>targetsize then amount is negative - need to sell
--                                            the number 150 is empirically determined
-- min [amount, 2000]                 ------  upperbound of buy size is 2,000
-- max [amount, -2000]                ------  lowerbound of buy size is -2,000
--
invfun (FundBuyer id) value price inv     = if  amount > 0 
                                            then minimum [amount, 2000]
                                            else maximum [amount, -2000]
                                            where
                                            amount = targetsize - (exp(y/150))
                                            targetsize = 200
                                            y = inv-((target_inventory (FundBuyer id))*(1+sigmoid x))
                                            x = (value - price)
invfun (FundSeller id) value price inv    = if amount > 0 
                                            then minimum [amount, 2000]
                                            else maximum [amount, -2000]
                                            where
                                            amount = (exp(-y/150)) - targetsize  
                                            targetsize = 200
                                            y = inv-((target_inventory (FundSeller id))*(1+sigmoid x))
                                            x = (value - price)
invfun (Intermediary id) value price inv  = if amount > 0 
                                            then minimum [amount, 2000]
                                            else maximum [amount, -2000]
                                            where
                                            amount = 0.2 * (-(tangent (y/700)))*targetsize    -- titrate the parameters for Intermediary!
                                            targetsize = 100
                                            y = inv-((target_inventory (Intermediary id))*(1+sigmoid x))
                                            x = (value - price)
invfun (HFT          id) value price inv  = if amount > 0  
                                            then minimum [amount, 2000] 
                                            else maximum [amount, -2000]
                                            where
                                            amount = 0.2 * (-(tangent (y/700)))*targetsize    -- titrate the parameters for HFT!
                                            targetsize = 100
                                            y = inv-((target_inventory (HFT id))*(1+sigmoid x))
                                            x = (value - price)
invfun (Opportunistic id) value price inv = if amount > 0   
                                            then minimum [amount, 250] 
                                            else maximum [amount, -250]
                                            where
                                            amount = f ((-(tangent (y/700)))*targetsize)    -- titrate the parameters for Opportunistic!
                                            targetsize = 50
                                            y = inv-((target_inventory (Opportunistic id))*(1+sigmoid x))
                                            x = (value - price)
                                            f x = x
                                         --  f x = abs x, if (value > price)
                                         --      = - (abs x), otherwise
invfun (Small        id) value price inv  = if amount > 0
                                            then minimum [amount, 250] 
                                            else maximum [amount, -250]
                                            where
                                            amount = f (((-1)^(mod id  2)) * 0.9 * (-(tangent (y/700)))*targetsize)    -- titrate the parameters for Small!
                                            targetsize = 100
                                            y = inv-((target_inventory (Small id))*(1+sigmoid x))
                                            x = (value - price)
                                            f x = x
--                                          f x = abs x, if (value > price)
--                                              = - (abs x), otherwise
--invfun a b c d = error ("oops: " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d)) 

foo = [(0.2 * (-(tangent (y/700)))*100) | y <- [-1000,-800,-600,-400,-200,0,200,400,600,800,1000]]
--unit testing of invfun
invfun_ut catid = map (invfun catid 5 10) [t - 1000, t - 900 .. t+1000]
                   where
                   t = target_inventory catid

type Traderstate = (Double, Double, Int)
emptytraderstate = (0, 0, 0)

-- A generic trader takes as arguments:
--    - a catid comprising a constructor giving the category of the trader and an ID number
--    - a list of random numbers
--    - a list of numbers with gaussian distribution 
--    - a list of time-ordered snapshots of the limit order book - only look at the head of this list!
--    - a traderstate holding state information only of interest to this instance of the trader
-- And the returned result is a list of orders to be sent to the exchange
--

--old_ordernum is Double because it comes from Xsum which is [Double] 
generic_trader :: Traderid -> [Int] -> [Double] -> [Double] -> (Price,Price,Double, Sentiment, Time, Inventory) -> Order
generic_trader catid (r:rs) (g:gs) [] state = emptyorder
generic_trader catid []     []     summary state = error "generic trader - run out of both randoms and gaussians"
generic_trader catid []     (g:gs) summary state = error "generic trader: run out of random numbers"
generic_trader catid (r:rs) []     summary state = error "generic trader: run out of gaussian numbers"
generic_trader catid (r:rs) (g:gs) [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, ordernum, ltp] (old_bestbid, old_bestoffer, old_ordernum, sent, time, my_inventory)
     = (d2 catid sent)
       where
       newstate = (bestbid, bestoffer, ordernum) 
       
       ordernum_move = if (old_ordernum /= 0)
                       then (ordernum - old_ordernum) /  old_ordernum          -- big problem Double or Int
                       else 0.05
       bestbid = bb
       bestoffer = bo
       midprice = (bestbid + bestoffer)/2
       spread = bestoffer - bestbid
       bidpricemove = if (old_bestbid /= 0)  
                      then (bestbid - old_bestbid) / old_bestbid
                      else 0.05
       offerpricemove = if (old_bestoffer /= 0)
                        then (bestoffer - old_bestoffer) / old_bestoffer
                        else 0.05
--      my_inventory = foldr (+) 0 (map getmovement mytrades)
--                     where
--                     mytrades = filter is_mine flattenedtrades
--                     expand [] = []
--                     expand ((a,b):xs) = a:b:(expand xs)
--                     is_mine o = ((order_gettraderid o) = catid)
--                     getmovement o = osize,     if (or [otype = (Bid Goodtillcancelled), otype = (Buy Andkill)])
--                                   = (- osize), if (or [otype = (Sell Andkill), otype = (Offer Goodtillcancelled)])
--                                   = 0, otherwise
--                                     where
--                                     osize = order_getsize o
--                                     otype = order_gettype o
       bs_liquidity_alert = bsld < 10
       ss_liquidity_alert = ssld < 10
       crossed_book_alert = bb > bo
       bs_levels_alert = bsls < 3
       ss_levels_alert = ssls < 3
       current_value = if v == 0 
                       then error (show v) 
                       else v
                       where
                       v = value sent time
       offerpremium = current_value*0.05   --if premium set as percentage of value, ramp is stable
       bidpremium = current_value*0.05
       newbidprice = maximum[(current_value - bidpremium),0]  --entier (bestbid + (g*20))
       newofferprice = maximum [(current_value + offerpremium),0] -- entier (bestoffer - (g*20))
       newbuysize  = 100 -- entier (abs (( mod r 500) + (g*10)))
       newsellsize = 100 -- entier (abs (( mod r 500) + (g*1000)))
       fillsperorder = 1.5
       (ordertype, orderprice, ordersize) = (ot, (abs op), fromIntegral $floor(abs os))                       -- (ot, (entier (abs op)), (entier (abs os)))   is it important to give an integral price?
                                            where
                                            newinv = my_inventory + (((supply catid) - (demand catid))*(fromIntegral time))
                                            os = (vf pricechange) * ((of1 imbalance) * (invfun catid current_value midprice newinv)) 
                                            pricechange = 2*(bidpricemove + offerpricemove)
                                            imbalance = ssd - bsd
                                            ot |  or [and [(os < 0), os_is_large], and[(os < 0), is_smalltrader catid]]          = (Sell Andkill)
                                               |  (os < 0)                                                                       = (Offer Goodtillcancelled)
                                               |  or [and [(os >= 0), os_is_large], and [(os >= 0), is_smalltrader catid]]       = (Buy Andkill) 
                                               | otherwise                                                                       = (Bid Goodtillcancelled) 
                                            -- Price Logic - compare size (os), bestbid (b), bestoffer (bo) and current value (cv)
                                            -- Assume large size implies greater need to trade
                                            -- If (bb > bo > cv), crossed book + falling; Smallbid at cv,  Largebid at bo;  Smalloffer at bb,  Largeoffer at bo
                                            -- If (bb > cv > bo), crossed book + stable;  Smallbid at bo,  Largebid at bo;  Smalloffer at bb,  Largeoffer at bb
                                            -- If (cv > bb > bo), crossed book + rising;  Smallbid at bo,  Largebid at bb;  Smalloffer at cv,  Largeoffer at cv
                                            -- If (bo > bb > cv),                falling; Smallbid at cv,  Largebid at bo; Smalloffer at bb,  Largeoffer at cv+
                                            -- If (bo > cv > bb),                stable;  Smallbid at bb+, Largebid at cv-; Smalloffer at bo-, Largeoffer at bb
                                            -- If (cv > bo > bb),                rising;  Smallbid at bo,  Largebid at bb+;  Smalloffer at cv,  Largeoffer at bo-
                                            op | and[ot==(Bid Goodtillcancelled), os_is_large]           = opBb
                                               | (ot==(Bid Goodtillcancelled))                           = opBs
                                               | and[ot==(Offer Goodtillcancelled), os_is_large]         = opOb
                                               | otherwise                                              = opOs
                                            opBs| and [cv >= bestoffer, bestoffer /=0]          = bestoffer                          
                                                | and [bestoffer >= cv, cv >= bestbid]          = bestbid + (a1 * (cv-bestbid))     
                                                | and [bestbid >= bestoffer,bestoffer >= cv]    = cv                                 
                                                | and [bestoffer >= bestbid ,bestbid >= cv]     = cv                               
                                                | (bestoffer==0)                                = cv                                
                                                  --    = cv - (abs (a1 * (bestbid - cv))),    otherwise   || not sure if we need this
                                                | otherwise = error ("opBs: bb="++(show bestbid)++" bo="++(show bestoffer)++" cv="++(show cv)++" os="++(show os)
                                                            ++" a2="++(show a2)++"\n") 
                                            opBb| and [bestbid >= bestoffer, bestoffer >= cv, bestoffer /= 0]  = bestoffer                         
                                                | and [bestbid >= cv, cv >= bestoffer, bestoffer /= 0]         = bestoffer                         
                                                | and [cv >= bestbid, bestbid >= bestoffer, bestbid /= 0]      = bestbid                            
                                                | and [cv >= bestoffer, bestoffer>= bestbid, bestoffer /= 0]   = bestoffer                          
                                                | and [bestoffer >= bestbid, bestbid >= cv]                    = cv - (abs (a1 * (bestbid - cv)))  
                                                | and [bestoffer >= cv,cv >= bestbid]                          = bestbid + (a1 * (cv-bestbid))       
                                                | otherwise                                                    = cv - (abs (a1 * (bestbid - cv)))   
                                            opOs| and [bestbid >= bestoffer, bestoffer >= cv]                  = bestbid                            
                                                | and [bestbid >= cv, cv >= bestoffer]                         = bestbid                           
                                                | and [cv >= bestbid, cv >= bestoffer, bestbid /= 0]           = cv                                 
                                                | and [cv >= bestoffer, bestoffer >= bestbid, bestoffer /= 0]  = cv                                 
                                                | and [bestoffer >= bestbid, bestbid >= cv]                    = bestbid                            
                                                | and [bestoffer >= cv,cv >= bestbid]                          = bestoffer - (a2 * spread)          
                                                | otherwise                                                    = cv + (abs (a2 * (cv-bestoffer)))   
 --                                                = error ("opOs: bb="++(show bestbid)++" bo="++(show bestoffer)++" cv="++(show cv)++" os="++(show os)
 --                                                       ++" a2="++(show a2)++"\n"), otherwise 
                                            opOb| and [bestbid >= bestoffer, bestoffer >= cv]                  = bestoffer                          
                                                | and [bestbid >= cv, cv >= bestoffer]                         = bestbid                           
                                                | and [cv >= bestbid, bestbid >= bestoffer]                    = cv                                
                                                | and [cv >= bestoffer, bestoffer >= bestbid]                  = cv + (abs (a2 * (cv-bestbid)))    
                                                | and [bestoffer >= bestbid, bestbid >= cv]                    = bestbid                           
                                                | and [bestoffer >= cv, cv >= bestbid]                         = bestoffer - (a2 * spread)         
                                                | otherwise = error ("opOb: bb="++(show bestbid)++" bo="++(show bestoffer)++" cv="++(show cv)++" os="++(show os)++"\n")
--                                                = cv + (abs (a2 * (cv-bestoffer))),    otherwise   || not sure if we need this
                                            os_is_large = (abs(os) > 3000)
                                            is_smalltrader (Small x) = True
                                            is_smalltrader any       = False
                                            a1 = 0.5 * (pf bsd)
                                            a2 = 0.5 * (pf ssd)
                                            cv = current_value
                                            (of1, vf) = tradersensitivity catid
 -- New version replaces d1
       d2 identifier sent = order_create orderprice ordersize time catid ordertype
 
 --
 -- ************************************************************************************************
 -- VERSION 3 has feedback loops
 -- ************************************************************************************************
 --
 -- Fundamental buyers are informed by the underlying value of the security. They monitor quote volumes (ordernum_move)
 -- and withdraw liquidity when they are high
       d1 (FundBuyer id) sent      | (bestoffer == 0)                          = order_create newbidprice z time catid (Bid Goodtillcancelled) 
                                   | and[(r < 500), (ordernum_move < 0.1)]     = order_create (minimum[bestoffer,bestbid,newbidprice]) z time catid (Bid Goodtillcancelled) 
                                   | and[(r < 500), (ordernum_move < 0.3)]     = order_create (minimum[bestoffer,bestbid,newbidprice]) (z*(1-ordernum_move)) time catid (Bid Goodtillcancelled)
                                   | otherwise  = emptyorder 
                                      where
                                      --target = 5000
                                      z = (fillsperorder * newsellsize) -- max [target-my_inventory,50]
 -- Fundamental sellers are informed by the underlying value of the security. They monitor quote volumes (ordernum_move)
 -- and withdraw liquidity when they are high
       d1 (FundSeller id) sent     | (bestbid == 0)                                           = order_create newofferprice z time catid (Offer Goodtillcancelled) 
                                   | and[(or[(r < 700),(time < 100)]),(ordernum_move < 0.1)]  = order_create (maximum[bestbid, bestoffer,newofferprice]) z time catid (Offer Goodtillcancelled)
                                   | and[(or[(r < 700),(time < 100)]),(ordernum_move < 0.3)]  = order_create (maximum[bestbid, bestoffer,newofferprice]) (z*(1-ordernum_move)) time catid (Offer Goodtillcancelled) 
                                   | otherwise  = emptyorder 
                                      where
                                      --target = 5000
                                      z = fillsperorder * newbuysize -- min [abs (my_inventory - target),50]
 -- Intermediaries issue both Bids and Offers at the current BBO or just behind (since they can place bigger orders)
       d1 (Intermediary id) sent   | (bestbid == 0)                                                        = order_create newbidprice (fillsperorder*newsellsize) time catid (Bid Goodtillcancelled) 
                                   | (bestoffer == 0)                                                      = order_create newofferprice (fillsperorder*newbuysize) time catid (Offer Goodtillcancelled) 
                                   | or[ss_liquidity_alert,ss_levels_alert, crossed_book_alert, r<500]     = order_create newofferprice newbuysize time catid (Offer Goodtillcancelled) 
                                   | or[(my_inventory < 50),bs_liquidity_alert,bs_levels_alert, r>=500]    = order_create newbidprice   newsellsize  time catid (Bid Goodtillcancelled) 
                                   | otherwise  = emptyorder 
                                      --where
                                      --target = 50000
                                      --temp = target - my_inventory
                                      --z = newsellsize || min [(abs temp),500]
 -- HFTs place bids and offers at the current BBO or (Sell Andkill) aggressively if they exceed their target inventory
       d1 (HFT id) sent            | (bestbid == 0)              = order_create newbidprice (fillsperorder*newsellsize) time catid (Bid Goodtillcancelled)
                                   | (bestoffer == 0)            = order_create newofferprice (fillsperorder*newbuysize) time catid (Offer Goodtillcancelled)
                                   | (my_inventory < target)     = order_create newbidprice   bsize time catid (Bid Goodtillcancelled)  
                                   | (my_inventory > osize)      = order_create newofferprice osize time catid (Offer Goodtillcancelled)
                                   | (my_inventory > target)     = order_create 0         my_inventory time catid (Sell Andkill)  
                                      where
                                      osize = fillsperorder * newbuysize
                                      bsize = fillsperorder * newsellsize
                                      target = 1000
                                      --z = newsellsize || min [abs (target-my_inventory),50]

 -- Small traders mostly do not trade at all, and when they do trade it is essentially random
 -- BSTakers/SSTaker traders (buyers/sellers) change order size if price movements exceed certain limits.  
 -- Tiny movements are ignored, small movements increase size, bigger movements decrease size down to 0 for fear of buying too expensively
 -- or selling too cheaply
 --
       d1 (Small id) sent          | (r < 900)   = emptyorder
                                   | (r<800)     = order_create 0 mysellsize time catid (Sell Andkill)     --same as SSTaker
                                   | otherwise   = order_create 0 mybuysize time catid (Buy Andkill)        --same as BSTaker
                                     where
                                     mysellsize | ((abs bidpricemove) < 0.05)                   = newbuysize 
                                                | ((bidpricemove > 0) && (bidpricemove < 0.1))  = newbuysize * (1+(abs bidpricemove))
                                                | ((bidpricemove > 0) && (bidpricemove < 0.4))  = newbuysize * (1-(abs bidpricemove)) 
                                                | (bidpricemove > 0)                            = 0 
                                                | otherwise                                     = 0
                                     mybuysize  | ((abs offerpricemove) < 0.05)                          = newbuysize
                                                | ((offerpricemove < 0) && (offerpricemove > (-0.1)))    = newbuysize * (1+(abs offerpricemove))
                                                | ((offerpricemove < 0) && (offerpricemove > (-0.4)))    = newbuysize * (1-(abs offerpricemove))
                                                | (offerpricemove < 0)                                   = 0 
                                                | otherwise                                              = 0 
 -- Opportunistic traders make small orders where they can see value.  they all are subject to the same feedback loops as Small traders
       d1 (Opportunistic id) sent  |(bestbid > current_value)    = order_create 0 mysellsize time catid (Sell Andkill)
                                   |(bestoffer < current_value)  = order_create 0 mybuysize time catid (Buy Andkill)
                                   |(r<320)                      = order_create 0 mysellsize  time catid (Sell Andkill)
                                   | otherwise                   = emptyorder
                                      where
                                      mysellsize| ((abs bidpricemove) < 0.05)                  = newbuysize 
                                                | ((bidpricemove > 0) && (bidpricemove < 0.1))  = newbuysize * (1+(abs bidpricemove))
                                                | ((bidpricemove > 0) && (bidpricemove < 0.4))  = newbuysize * (1-(abs bidpricemove))
                                                | (bidpricemove > 0)                           = 0
                                                | otherwise                                    = 0
                                      mybuysize | ((abs offerpricemove) < 0.05)                       = newbuysize 
                                                | ((offerpricemove < 0) && (offerpricemove > (-0.1)))  = newbuysize * (1+(abs offerpricemove))
                                                | ((offerpricemove < 0) && (offerpricemove > (-0.4)))  = newbuysize * (1-(abs offerpricemove))
                                                | (offerpricemove < 0)                                = 0 
                                                | otherwise                                           = 0 
 
 -- ************************************************************************************************
 --VERSIONS 1 and 2 only use makers and takers
 -- ************************************************************************************************
 --
 -- MAKERS
 --
 -- In a Calm market BSMaker traders make bids 
       d1 (BSMaker id) sent         = order_create newbidprice (fillsperorder*newsellsize) time catid (Bid Goodtillcancelled)
 -- In a Calm market SSMaker traders make offers
       d1 (SSMaker id) sent         = order_create newofferprice (fillsperorder*newsellsize) time catid (Offer Goodtillcancelled)
 -- In a Calm market Maker traders make bids and offers
       d1 (Maker id) sent           = if (r<500) 
                                      then order_create newbidprice (fillsperorder*newsellsize) time catid (Bid Goodtillcancelled)
                                      else order_create newofferprice (fillsperorder*newbuysize) time catid (Offer Goodtillcancelled)
 -- 
 -- TAKERS
 -- 
 -- In a Calm market BSTaker traders make buys 
       d1 (BSTaker id) sent         = order_create 0 newbuysize  time catid (Buy Andkill)
 -- In a Calm market SSTaker traders make sells
       d1 (SSTaker id) sent         = order_create 0 newsellsize time catid (Sell Andkill)
 -- In a Calm market Taker traders make buys and sells
       d1 (Taker id) sent           =  if (r < 500)  
                                       then order_create 0 newsellsize time catid (Sell Andkill)
                                       else order_create 0 newbuysize  time catid (Buy Andkill)
generic_trader catid (r:rs) (g:gs) (x:xs) state = error ("Generic trader can't recognize something: " ++ (show catid)++" "++(show (x:xs))++" "++(show state))