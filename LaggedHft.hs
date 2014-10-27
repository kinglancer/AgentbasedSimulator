module LaggedHft(laggedhftwrapper,laggedhftwrapper1) where


import Comm
import Agent


--parameters
----------------------------------------
defabslimit = 20000 -- 30 (
defsoftlimit = 2700 --20
--maxorder = 2000 -- cmegroup.com E-mini FAQ
ltpbound = 12
foam = 3 -- 0.5 The spread will be no more extreme than 1.01 or 0.99 times the base price.
ordersperstepperside = 10
xth = 3

defstubsize = 1 -- CDC 17/08/12 - added in anticipation of experimenting with stub sizes
defmrt = 0 -- Minimum resting time.
-------------------------------------------------

--laggedhftlogic 671 2000 2002 2000 2000 12 gaussians True 2700 defstubsize   || CDC 17/08/12 - added stubsize argument
--laggedhftlogic 2700 2000 2002 2000 2000 12 gaussians True 2700 defstubsize  || CDC 17/08/12 - added stubsize argument
--laggedhftlogic 2699 2000 2002 2000 2000 12 gaussians True 2700 defstubsize  || CDC 17/08/12 - added stubsize argument


--Return two lists of tuples with (Size,Price)
laggedhftlogic inventory bb bo ltp uv myid ingaus stubs softlimit abslimit stubsize usegauss bstock sstock  -- CDC 17/08/12 - added stubsize argument - 18/08/12 added usegauuss
-- = error ((show inventory) ++ " " ++ (show bb) ++ " " ++ (show bo) ++ " " ++ (show ltp) ++ " " ++ (show uv) ++ " " ++ (show myid))
   = (bids, offers)
-- = "Bids: " ++ (show bids) ++ "\nOffers:" ++ (show offers)
   where



--Step 1.
--First and foremost, if inventory exceeds either limit then just put in the respective market order.
--E.g. if invent >2700 then SELL and vice-versa.
--Also, if the bidsize is 0 we don't bother with all the order calculations. (This is both because there won't be any orders and doing so would trigger math related errors.)

   bids   | (inventory >= abslimit)     = []                                                                                    -- stubs are only active below the hard limit (the abslimit)
          | inventory <= (-softlimit)   = bstep ++ (markettuples  softlimit (-1))               -- CDC 17/08/12 - changed size from 2*softlimit to softlimit to fit with analytic model
          | bidsize < stubsize          = bstep                                                -- (1, bbp)
          | usegauss == 1                = error "DON'T USE GAUSSES."                             --zipfunc bidprep bidgaussians, if usegauss=1
          | otherwise                      = markettuples totbidsize bbp                                           -- EMC 18/09/12 - corrected from bidsize to totbidsize
   offers | (inventory <= (-abslimit))  = []                                                                                             -- stubs are only active below the hard limit (the abslimit)
          | inventory >= softlimit      = sstep ++ (markettuples  softlimit (-1))                     -- CDC 17/08/12 - changed size from 2*softlimit to softlimit to fit with analytic model
          | offersize < stubsize        = sstep                                                       -- (1, nbop)
          | usegauss == 1                = error "DON'T USE GAUSSES."                                --zipfunc offerprep offergaussians, if usegauss=1
          | otherwise                      = markettuples totoffersize nbop                                         -- EMC 18/09/12 - corrected from offersize to totoffersize
   markettuples size price = if size > maxorder
                             then (maxorder, price) : (markettuples (size - maxorder) price)   -- CDC 17/08/12 now uses maxorder instead of hard-coded 1,000. NB price -1 is code for market order
                             else  [(size, price)]                                                     
   (bstep, sstep) = if stubs == True 
                    then ([(stubsize, bbp)], [(stubsize, nbop)])                                 -- CDC 17/08/12 - changed stub sizes from 1 to stubsize
                    else ([], [])

--Step 2.
--Get a base price for the buys and offers.
--As an laggedhft gets closer and closer to its panic zones it has to make its corrective orders more and more attractive.
--However, at the same time the laggedhft wants to maximise profits and mimimise losses.
--Thus a version of the tangent function would seem to suit our needs.
--The version proposed is shown below.
--As an laggedhft's inventory approaches the panic zone on either side it initially slowly makes the opposite side's orders more attractive and the offending side's less attractive.
--As it progresses closer and closer it does this more violently until eventually using market orders instead (See step 1).

   nbop = if bop /= bbp
          then bop 
          else bbp + 1
   bbp = if nbb > lowerbound
         then maximum [0, minimum [(maximum[((nbb) + baseoffset), lowerbound]), upperbound]]     -- CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
         else maximum [0, minimum [(maximum[(lowerbound + baseoffset), lowerbound]), upperbound]]         -- CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
   bop = if nbo < upperbound
         then maximum [0, maximum [(minimum[((nbo) + baseoffset), upperbound]), lowerbound] ]   -- CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
         else maximum [0, maximum [(minimum[(upperbound + baseoffset), upperbound]), lowerbound] ]       -- CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
   nbb = if bb == 0
         then ltp
         else bb
   nbo = if bo == 0 
         then ltp 
         else bo
   baseoffset = -(((nbo - nbb) - 1)*(inventory / softlimit))      --(vertimultiplier * (-(tan (inventory * horimultiplier)))) ||Chris's pricing formula injected here.
   -- vertimultiplier = 3/(tan (softlimit * (pi/(2 * abslimit)))) || 4 or 3 used to be ltpbound
   -- horimultiplier = pi/(2 * abslimit)
   upperbound = (ltp + ltpbound)
   lowerbound = (ltp - ltpbound)

--Step 3.
--Then the laggedhft needs to figure out the total size of offers and bids it will make.
--They do this by augmenting the total inventory an HFC may have by a "size multiplier" to get both the total bid and offer sizes.
--The graphs and functions for the buy and sell size multipliers are below.
--The Y offset in both is 0.5 as if the laggedhft has an equal position (0 inventory) then it doesn't mind whether it buys or sells
--and ideally it would wish not to exceed half of its maximum inventory in either direction. (e.g. + or - 1500)
--There are hard limits of 1 on either side, this is so that as the inventory approaches the panic zone it becomes one-sided
--concentrating on bringing the inventory back to a middle position.

-- +x                Bid size multiplier               +
-- x                      +>1.0
-- +     x                   |                         +
-- x                |
-- +           x             |                         +       FMLA
-- x          |                                 Y = (-x/(softlimit * 2)) + 0.5
-- +                 x       |                         +
-- x    |
-- +                       x |                         +
-- +>0.5
-- +                         |   x                     +
-- |       x
-- +                         |          x              +
-- |            x
-- +                         |               x         +
-- |                 x
-- +                         |                    x    +
-- |                        x
-- +-------------------------+-------------------------+  Inventory
-- (Short -> Long)
-- +                                                   +
-- 
-- +                                                   +
-- Panic                                               Panic
-- (soft limit)                                        (soft limit)
-- 
-- 
-- 
-- 
-- 
-- +                Offer size multiplier              +
-- +>1.0                  x
-- +                         |                   x     +
-- |                x
-- +                         |              x          +       FMLA
-- |           x                     Y = (x/(softlimit * 2)) + 0.5
-- +                         |        x                +
-- |     x
-- +                         |  x                      +
-- +>0.5
-- +                      x  |                         +
-- x     |
-- +                x        |                         +
-- x           |
-- +          x              |                         +
-- x                 |
-- +    x                    |                         +
-- x                       |
-- +-------------------------+-------------------------+  Inventory
-- (Short -> Long)
-- +                                                   +
-- 
-- +                                                   +
-- Panic                                               Panic
-- (soft limit)                                        (soft limit)

   totbidsize = delta bstock (maximum [0, softlimit - 1 - inventory])      --  -((inventory / softlimit) - 1) * (softlimit - 1)] || CDC 17/08/12 use max(0,UL-1-inventory) to fit with analytic model
   totoffersize = delta sstock (maximum [0, inventory - (-softlimit) - 1]) -- -((-inventory / softlimit) - 1) * (softlimit - 1)] || CDC 17/08/12 use max(0,inventory-LL-1) to fit with analytic model, where LL is negative
   -- neutralsize = (softlimit/(xth * 0.5))             || no longer used

--Step 3.
--Given the price and size we now split up the orders based upon a number of orders each laggedhft wishes to place each turn (this will be a constant).
--If the total size is less than the number of orders then we just place total size many orders of 1.
--
   bidsize ::Double
   bidsize =  fromIntegral $floor(totbidsize / (fromIntegral ordersperstepperside))
   offersize::Double
   offersize =  fromIntegral $floor(totoffersize / (fromIntegral ordersperstepperside))
   bidprep = if bidsize >=1
             then rep ordersperstepperside (maketuple bidsize )
             else rep (round totbidsize) (maketuple 1 )
   offerprep =if offersize >=1 
              then rep ordersperstepperside (maketuple offersize ) 
              else rep (round totbidsize) (maketuple 1 )
   maketuple sz gs = (sz, gs) --This takes a size and a price (made from a base price and gaussian number) to make a (size,price) tuple.

--laggedhftlogic 1256 2000 2002 2000 2000 12 gaussians True 2700 defstubsize  || CDC 17/08/12 - added stubsize argument
--
--Step 4.
--We generate as many gaussians numbers as orders between +- the maximum desired spread (e.g. 3) and generate an order at baseprice + gaussiannumber.
--This is so that we have many small orders within and around the spread rather than few large orders by each laggedhft.
--If any one of the prices is outside the price band we reflect it about the price band. E.g. price band 1.7 and order of 1.78 would then become 1.62.

   bidgaussians = map f gaussiansforbidsandoffers                                 -- CDC 17/08/12 replaced (take ordersperstepperside mygaussians)
                  -- map ((fixgs).(+bbp).(* checkbb)) gaussiansforbidsandoffers   -- CDC 17/08/12 replaced (take ordersperstepperside mygaussians)
                  where
                  f x = if bb /= 0
                        then fixgs (bbp + (x * foam * uv / (bb*6000)))
                        else fixgs (bbp + (x * foam / 6000))
                  --checkbb = (foam*uv)/(bb*6000), if bb ~= 0
                  --        = (foam/6000), otherwise
   offergaussians = map f gaussiansforbidsandoffers -- CDC 17/08/12 replaced (take ordersperstepperside (drop ordersperstepperside mygaussians))
                    --   map ((fixgs).(+nbop).(* checkbo)) gaussiansforbidsandoffers || CDC 17/08/12 replaced (take ordersperstepperside (drop ordersperstepperside mygaussians))
                    where
                    f x = if bo /= 0 
                          then fixgs (nbop + (x * foam * uv / (bo*6000)))
                          else fixgs (nbop + (x * foam / 6000))
                    --checkbo = (foam*uv)/(bo*6000), if bo ~= 0
                    --        = (foam/6000), otherwise
   gaussiansforbidsandoffers = take ordersperstepperside ingaus                                      -- CDC 17/08/12
   -- gaussiansforoffers = take ordersperstepperside (drop ordersperstepperside foamedgaussians)     -- CDC 17/08/12
   -- foamedgaussians = map (*(foam/6000)) ingaus                                                    -- CDC 17/08/12
   -- mygaussians = take (ordersperstepperside * 3) (map (*(foam/6000)) ingaus) ||This is to bring the gaussian bounds to +- 3 instead of +- 6
   fixgs gs | (gs < lowerbound)  = lowerbound + (lowerbound - gs)  -- CDC 17/08/12 replaced (ltp-ltpbound) with lowerbound
            | (gs > upperbound)  = upperbound - (gs - upperbound)  -- CDC 17/08/12 replaced (ltp+ltpbound) with upperbound
            | otherwise   = gs 

laggedhftwrapper :: Agent_t
laggedhftwrapper (Emptyagentstate) args any myid
     = laggedhftwrapper1 (Newagstate (0:0:0:iinvent, [], getsent, (map (order_setuid "new laggedhft") [0..]), gses, [mymrt, softlimit, abslimit, stubsin, stubsize, delay, usegauss])) args any myid
                                              where
                                              mymrt = if (arg_findval "MinimumRestingTime" args) /= (-1)
                                                      then arg_findval "MinimumRestingTime" args
                                                      else fromIntegral defmrt
                                              iinvent = if delay /= 0
                                                        then rep (round delay) 0 
                                                        else [0]
                                                      -- = -softlimit, if myid = 2
                                                      -- = -(softlimit - 1), otherwise
                                                      -- = (-2500) + (abs ((randoms!(myid^2)) * 5)) || very old
                                              gses = gaussians
                                              getsent| sentiment == "Calm"     = Calm   
                                                     | sentiment == "Choppy"   = Choppy 
                                                     | sentiment == "Ramp"     = Ramp  
                                                     | sentiment == "Toxic"    = Toxic  
                                                     | otherwise  = error "Unrecognised sentiment." 
                                                        where
                                                        sentiment = arg_getstr (si "eerc:1" args 0)  
                                              softlimit :: Double         
                                              softlimit = if (arg_findval "SoftLimit" args) /= (-1) 
                                                          then arg_findval "SoftLimit" args 
                                                          else defsoftlimit
                                              abslimit = if (arg_findval "AbsLimit" args) /= (-1) 
                                                         then arg_findval "AbsLimit" args
                                                         else defabslimit
                                              stubsin = if (arg_findval "StubsEnabled" args) /= (-1) 
                                                        then 1 
                                                        else 0
                                              stubsize = if (arg_findval "Stubsize" args) /= (-1) 
                                                         then arg_findval "Stubsize" args 
                                                         else defstubsize
                                              delay = if (arg_findval "Delay" args) /= (-1)
                                                      then arg_findval "Delay" args 
                                                      else 1
                                              usegauss = 0 --(arg_findval "UseGaussians" args) decided not to have order splitting
laggedhftwrapper anything args any myid = laggedhftwrapper1 anything args any myid


laggedhftwrapper1 (Newagstate (oldprofits:bstock:sstock:invent:irest, oldorders, sent, uids, gses, [mrt, softlimit, abslimit, stubsin, stubsize, delay, usegauss])) args ((time, messages, broadcasts) : simstateinfos) myid
     = (tracemsg ++ cancellations ++ [dbmsg] ++ (ordstomsg idtagdfinalorders)) : (laggedhftwrapper1 (Newagstate (newprofits:newbstock:newsstock:(irest ++ [newinvent]), idtagdfinalorders, sent, remaininguids, (drop (ordersperstepperside * 3) gses), [mrt, softlimit, abslimit, stubsin, stubsize, delay,usegauss])) args simstateinfos myid)
       where
       (newbstock, newsstock, dbmsg) = ((bstock + bdelta),(sstock + sdelta), (debugmessage (myid,0) dbtext))
                                       where
                                       dbtext = "bstock_t-1: " ++ (show bstock) ++ "\nbdelta: " ++ (show bdelta) ++ "\nsuccessfulbids: " ++ (show successfulbids) ++ "\nbidconfs: " ++ (show bidconfs) ++ "\nbidrejs: " ++ (show bidrejs) ++ "\nsstock_t-1: " ++ (show sstock) ++ "\nsdelta: " ++ (show sdelta) ++ "\nsuccessfuloffers: " ++ (show successfuloffers) ++ "\nofferconfs: " ++ (show offerconfs) ++ "\nofferrejs: " ++ (show offerrejs) ++ "\ntrvals: " ++ (show trvals)
                                       bdelta = bidconfs - bidrejs - successfulbids
                                       sdelta = offerconfs - offerrejs - successfuloffers
                                       ackmsgs = (filter msg_isack messages) -- Collection of all acks.
                                       confs = map msg_getackdorder (filter ((== 0).msg_getackcode) ackmsgs) -- Looking for 0 codes, these indicate successful addition to the exchange.
                                       rejects = map msg_getackdorder (filter ((== 5).msg_getackcode) ackmsgs) -- Looking for cancellations.
                                       trvals = (map getmovement (filter (mysuperor (isOtype "Bid") (isOtype "Offer")) mytrades))
                                       successfulbids = foldr (+) 0 (filter ((not).((>) 0)) trvals)
                                       successfuloffers = foldr (+) 0 (map abs (filter (((>) 0)) trvals))
                                       bidconfs = foldr (+) 0 (map order_getsize (filter (isOtype "Bid") confs))
                                       offerconfs = foldr (+) 0 (map order_getsize (filter (isOtype "Offer") confs))
                                       bidrejs = (foldr (+) 0 (map order_getsize (filter (isOtype "Bid") rejects)))
                                       offerrejs = (foldr (+) 0 (map order_getsize (filter (isOtype "Offer") rejects)))
       newprofitsincassetvalue =  newprofits + (newinvent * (si "third" xsum 9))
       newprofits = (foldr (+) oldprofits (map getcost mytrades))
                    where
                    getcost t | or [(isOtype "Bid" t), (isOtype "Buy" t)]     = -((order_getsize t) * (order_getprice t))  
                              | or [(isOtype "Offer" t), (isOtype "Sell" t)]  = (order_getsize t) * (order_getprice t)     

       tracemsg = if time == 0
                  then [(datamessage (myid,0) ("laggedhft" ++ (show myid) ++ "i, laggedhft" ++ (show myid) ++ " Panicking?, Profits/Losses" ++ (show myid) ++","))] 
                  else [(datamessage (myid,0) ((show newinvent) ++ "," ++ (show panicking) ++ "," ++ (show newprofitsincassetvalue) ++ ","))]
                  where
                  panicking =if (abs newinvent) >= softlimit 
                             then 1 
                             else  0
       cancellations = [] -- map (cancelmessage (myid, 1)) (map canceltuple oldorders)
                       -- where
                       -- canceltuple or = (myid, order_getuid or)
       (remaininguids, idtagdfinalorders) = if or [(delay /= 0), ((mod time 2) == 0)]
                                            then applyuids uids finalorders 
                                            else (uids, [])
       mytrades = map (cpsafehd "laggedhftwrapper1") (filter (not.null) (map msg_gettrade messages))
       newinvent = foldr (+) invent (map getmovement mytrades)
       getmovement o | (isBuySide o)   = (order_getsize o)     
                     | (isSellSide o)  = (- (order_getsize o)) 
                     | otherwise       = error "laggedhftwrapper - getmovement - order type not recognised."

       finalorders = bsorders ++ ssorders
       bsorders = (map (tupletoorder False time myid (round mrt)) mybids)
       ssorders = (map (tupletoorder True time myid (round mrt)) myoffers)
       ordstomsg o = map (ordermessage (myid, 1)) o
       (mybids, myoffers) = laggedhftlogic newinvent (si "first" xsum 0) (si "second" xsum 1) (si "third" xsum 9) underlyingvalue myid gses stubs softlimit abslimit stubsize usegauss newbstock newsstock  -- CDC 17/08/12 added stubsize  18/08/12 added usegauss
       stubs = if stubsin == 1
               then True 
               else False
       underlyingvalue = if time < 0
                         then (value sent 0) 
                         else (value sent time)
       xsum = if time <= 0
              then [1993, 2007, 500, 500, 1, 1, 500, 500, 2, 2000]  -- rep 9 0  [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or]
              else msg_getnumlistfrombcast (findG0bcast broadcasts)
              where
              findG0bcast [] = error ("No broadcast found?" ++ (show time))
              findG0bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 0)
                                  then f 
                                  else findG0bcast r
       tupletoorder tf ti mid mrt (size, price) = order_create price safesize ti (HFT mid) ty
                                                  where
                                                  safesize = if size < 0 
                                                             then error "laggedhftwrapper - tupletoorder - safesize" 
                                                             else size
                                                  ty| (tf == True) && price == (-1)   = (Sell Andkill)                                               --Sell
                                                    | (tf == True) && price /= (-1)  = (Offer (Goodtilldate (ti + 1 + mrt))) --Offer
                                                    | (tf == False) && price == (-1)  = (Buy Andkill)                                                --Buy
                                                    | (tf == False) && price /= (-1) = (Bid (Goodtilldate (ti + 1 + mrt)))     --Bid
 
--end of LaggedHft
----------------------------------------------
