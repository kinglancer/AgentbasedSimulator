module Hft(hftwrapper,hftwrapper1)  where

import Comm
import Agent


--parameters
-------------------------------------------------
--Hft
defabslimit = 20000 -- 30 (
defsoftlimit = 2700 --20
--maxorder = 2000 -- cmegroup.com E-mini FAQ
ltpbound = 12
foam = 3 -- 0.5 The spread will be no more extreme than 1.01 or 0.99 times the base price.
ordersperstepperside = 10
xth = 3

defstubsize = 1 -- CDC 17/08/12 - added in anticipation of experimenting with stub sizes




-----------------------------------------------------------
--hftlogic 671 2000 2002 2000 2000 12 gaussians True 2700 defstubsize   || CDC 17/08/12 - added stubsize argument
--hftlogic 2700 2000 2002 2000 2000 12 gaussians True 2700 defstubsize  || CDC 17/08/12 - added stubsize argument
--hftlogic 2699 2000 2002 2000 2000 12 gaussians True 2700 defstubsize  || CDC 17/08/12 - added stubsize argument
--
--hftlogic 2699 2000 2002 2000 2000 12 gaussians True 2700 defstubsize usegauss amx || EMC 07/06/2013 - added coefficient for market order size (amx)




--Return two lists of tuples with (Size,Price)
hftlogic inventory bb bo ltp uv myid ingaus stubs softlimit abslimit stubsize usegauss amx hetero -- CDC 17/08/12 - added stubsize argument - 18/08/12 added usegauuss
-- = error ((show inventory) ++ " " ++ (show bb) ++ " " ++ (show bo) ++ " " ++ (show ltp) ++ " " ++ (show uv) ++ " " ++ (show myid))
   = (bids, offers)
-- = "Bids: " ++ (show bids) ++ "\nOffers:" ++ (show offers)
   where



--Step 1.
--First and foremost, if inventory exceeds either limit then just put in the respective market order.
--If it exceeds abslimit STOP completely.
--E.g. if invent >2700 then SELL and vice-versa.
--Also, if the bidsize is 0 we don't bother with all the order calculations. (This is both because there won't be any orders and doing so would trigger math related errors.)

   bids   | (inventory >= abslimit)    = []                                                    -- stubs are only active below the hard limit (the abslimit)
          | inventory <= (-softlimit)  = bstep ++ (markettuples (amx * softlimit) (-1))                   -- CDC 17/08/12 - changed size from 2*softlimit to softlimit to fit with analytic model
          | bidsize < stubsize         = bstep  -- (1, nbbp)
          | usegauss==1                = zipfunc bidprep bidgaussians
          | otherwise      = markettuples totbidsize nbbp                                                      -- EMC 18/09/12 - corrected from bidsize to totbidsize
   offers | (inventory <= (-abslimit))   = []                                                          -- CDC stubs are INactive below the hard limit (the abslimit)
          | inventory >= softlimit     = sstep ++ (markettuples (amx * softlimit) (-1))                 -- CDC 17/08/12 - changed size from 2*softlimit to softlimit to fit with analytic model
          | offersize < stubsize      = sstep -- (1, nbop)
          | usegauss==1              = zipfunc offerprep offergaussians
          | otherwise              = markettuples totoffersize nbop                                                  -- EMC 18/09/12 - corrected from offersize to totoffersize
   markettuples size price = if size==0
                             then []                                                             -- CDC 11/06/14 - trying to avoid orders with size 0
                             else f size price
                             where
                             f size price 
                               = if size > maxorder
                                 then (maxorder, price) : (f (size - maxorder) price)  -- CDC 17/08/12 now uses maxorder instead of hard-coded 1,000. NB price -1 is code for market order
                                 else [(fromIntegral $floor(size), price)]
   (bstep, sstep) = if stubs == True
                    then ([(stubsize, nbbp)], [(stubsize, nbop)])                    -- CDC 17/08/12 - changed stub sizes from 1 to stubsize
                    else ([], [])

--Step 2.
--Get a base price for the buys and offers.
--As an HFT gets closer and closer to its panic zones it has to make its corrective orders more and more attractive.
--However, at the same time the HFT wants to maximise profits and mimimise losses.
--Thus a version of the tangent function would seem to suit our needs.
--The version proposed is shown below.
--As an HFT's inventory approaches the panic zone on either side it initially slowly makes the opposite side's orders more attractive and the offending side's less attractive.
--As it progresses closer and closer it does this more violently until eventually using market orders instead (See step 1).

   (nbop, nbbp) = if bop /= bbp 
                  then (bop, bbp) 
                  else (bbp + 0.3, bbp - 0.3)
   (bbp, bop) =  (minimum[upperbound,maximum[lowerbound,b]],minimum[upperbound,maximum[lowerbound,o]])  -- added 11/6/2014 CDC
                 where
                 (b,o)= (maximum [0,nbb - (0.5*ltpbound)*(1-(softlimit-1-inventory)/(softlimit+softlimit-2))],
                         maximum [0,nbo + (0.5*ltpbound)*(softlimit-1-inventory)/(softlimit+softlimit-2)]) -- (nbb + baseoffset, nbo + baseoffset) || updated 9/6/2014 CDC
   -- Price banding temporarily disabed and replaced by the above lines CDC 11/4/2014 and then reinstalled CDC 11/6/2014
   --
   -- bbp = max [0, min [(max [((nbb) + baseoffset), lowerbound]), upperbound]], if nbb > lowerbound    || CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
   --     = max [0, min [(max [(lowerbound + baseoffset), lowerbound]), upperbound]], otherwise       || CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
   -- bop = max [0, max [(min [((nbo) + baseoffset), upperbound]), lowerbound] ], if nbo < upperbound    || CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
   --     = max [0, max [(min [(upperbound + baseoffset), upperbound]), lowerbound] ], otherwise       || CDC 17/08/12 (ltp - ltpbound) replaced by lowerbound; ditto for upperbound
   baseoffset | hetero/=1  = -(((nbo - nbb) - 1)*(inventory / softlimit))      --(vertimultiplier * (-(tan (inventory * horimultiplier)))) ||Chris's pricing formula injected here.
              | hetero==1 && ( mod (round $systemTime 1)(round(myid+1))) == 0        = -(((nbo - nbb) - 1)*(inventory / softlimit))-(fromIntegral (mod (round $systemTime 1)(round (myid+1))))/(myid+1)
              | otherwise = -(((nbo - nbb) - 1)*(inventory / softlimit))+(fromIntegral $mod (round $systemTime 1) (round (myid+1)))/(myid+1)
   -- vertimultiplier = 3/(tan (softlimit * (pi/(2 * abslimit)))) || 4 or 3 used to be ltpbound
   -- horimultiplier = pi/(2 * abslimit)
   upperbound = (ltp + ltpbound)
   lowerbound = (ltp - ltpbound)
   nbb = if bb == 0
         then lowerbound  -- ltp, if bb = 0    || CDC 11/4/2014
         else bb
   nbo = if bo == 0 
         then upperbound  -- ltp, if bo = 0    || CDC 11/4/2014
         else bo

-- Step 3.
-- Then the HFT needs to figure out the total size of offers and bids it will make.
-- They do this by augmenting the total inventory an HFT may have by a "size multiplier" to get both the total bid and offer sizes.
-- The graphs and functions for the buy and sell size multipliers are below.
-- The Y offset in both is 0.5 as if the HFT has an equal position (0 inventory) then it doesn't mind whether it buys or sells
-- and ideally it would wish not to exceed half of its maximum inventory in either direction. (e.g. + or - 1500)
-- There are hard limits of 1 on either side, this is so that as the inventory approaches the panic zone it becomes one-sided
-- concentrating on bringing the inventory back to a middle position.
-- 
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

   totbidsize = if hetero/=1
                then maximum [0, softlimit - 1 - inventory]       --  -((inventory / softlimit) - 1) * (softlimit - 1)] || CDC 17/08/12 use max(0,UL-1-inventory) to fit with analytic model
                else (maximum [0, softlimit - 1 - inventory])*(fromIntegral ((mod(round $systemTime 1)(round(myid+1)))+1)) / (myid+1)
   totoffersize = if hetero/=1 -- -((-inventory / softlimit) - 1) * (softlimit - 1)]
                  then maximum [0, inventory - (-softlimit) - 1]  -- CDC 17/08/12 use max(0,inventory-LL-1) to fit with analytic model, where LL is negative
                  else maximum [0, inventory - (-softlimit) - 1]*(fromIntegral ((mod(round $systemTime 1)(round (myid+1)))+1)) /  (myid+1)
   -- neutralsize = (softlimit/(xth * 0.5))             || no longer used


--Step 3.
--Given the price and size we now split up the orders based upon a number of orders each HFT wishes to place each turn (this will be a constant).
--If the total size is less than the number of orders then we just place total size many orders of 1.

   bidsize :: Double
   bidsize =  fromIntegral $floor(totbidsize / (fromIntegral ordersperstepperside))     -- why entier
   offersize :: Double
   offersize = fromIntegral $floor(totoffersize / (fromIntegral ordersperstepperside))
   bidprep = if bidsize >=1
             then rep ordersperstepperside (maketuple bidsize )
             else rep (round totbidsize) (maketuple 1 )
   offerprep = if offersize >=1  
               then rep ordersperstepperside (maketuple offersize ) 
               else rep (round totbidsize) (maketuple 1 )
   maketuple sz gs = (sz, gs) --This takes a size and a price (made from a base price and gaussian number) to make a (size,price) tuple.

--hftlogic 1256 2000 2002 2000 2000 12 gaussians True 2700 defstubsize  || CDC 17/08/12 - added stubsize argument
--
--Step 4.
--We generate as many gaussians numbers as orders between +- the maximum desired spread (e.g. 3) and generate an order at baseprice + gaussiannumber.
--This is so that we have many small orders within and around the spread rather than few large orders by each HFT.
--If any one of the prices is outside the price band we reflect it about the price band. E.g. price band 1.7 and order of 1.78 would then become 1.62.

   bidgaussians = map f gaussiansforbidsandoffers                                 -- CDC 17/08/12 replaced (take ordersperstepperside mygaussians)
                  -- map ((fixgs).(+bbp).(* checkbb)) gaussiansforbidsandoffers   -- CDC 17/08/12 replaced (take ordersperstepperside mygaussians)
                  where
                  f x = if bb /= 0
                        then fixgs (nbbp + (x * foam * uv / (bb*6000)))
                        else fixgs (nbbp + (x * foam / 6000))
                  --checkbb = (foam*uv)/(bb*6000), if bb ~= 0
                  --        = (foam/6000), otherwise
   offergaussians = map f gaussiansforbidsandoffers --CDC 17/08/12 replaced (take ordersperstepperside (drop ordersperstepperside mygaussians))
                    --   map ((fixgs).(+nbop).(* checkbo)) gaussiansforbidsandoffers -- CDC 17/08/12 replaced (take ordersperstepperside (drop ordersperstepperside mygaussians))
                    where
                    f x = if bo /= 0  
                          then fixgs (nbop + (x * foam * uv / (bo*6000)))
                          else fixgs (nbop + (x * foam / 6000))
                    --checkbo = (foam*uv)/(bo*6000), if bo ~= 0
                    --        = (foam/6000), otherwise
   gaussiansforbidsandoffers = take ordersperstepperside ingaus                                     -- CDC 17/08/12
   -- gaussiansforoffers = take ordersperstepperside (drop ordersperstepperside foamedgaussians)    -- CDC 17/08/12
   -- foamedgaussians = map (*(foam/6000)) ingaus                                                   -- CDC 17/08/12
   -- mygaussians = take (ordersperstepperside * 3) (map (*(foam/6000)) ingaus) ||This is to bring the gaussian bounds to +- 3 instead of +- 6
   fixgs gs |(gs < lowerbound)   = lowerbound + (lowerbound - gs)    -- CDC 17/08/12 replaced (ltp-ltpbound) with lowerbound
            |(gs > upperbound)   = upperbound - (gs - upperbound)    -- CDC 17/08/12 replaced (ltp+ltpbound) with upperbound
            |otherwise           = gs

hftwrapper :: Agent_t
hftwrapper (Emptyagentstate) args any myid = hftwrapper1 delayedmessages (Newagstate (iinvent, [], getsent, (map (order_setuid "new HFT") [0..]), gses, [softlimit, abslimit, stubsin, stubsize, delay, usegauss, amx, hetero])) args any myid
                                              where
                                              delayedmessages = if delay==0 
                                                                then []
                                                                else rep (round delay) []
                                              iinvent -- = [12, 12, 15], if myid = 2 ||12, 15
                                                      -- = [6, 6, 3], if myid = 3   ||6, 3
                                                      -- ORIGINAL IINVENT CODE
                                                      = if delay /=0 
                                                        then [seedvent]  -- rep delay seedvent, if delay ~= 0 || CDC 9/4/14 - delay should be applied to msgs only!
                                                        else [seedvent]  -- n.b. Delay = 0 works in a different way by only trading on even timesteps.
                                                      -- = -softlimit, if myid = 2
                                                      -- = -(softlimit - 1), otherwise
                                                      --  = (-2500) + (abs ((randoms!!(myid^2)) * 5)) || very old
                                              seedvent = if (arg_findval ((show myid)++"HFTiseed") args) /= (-1)
                                                         then (arg_findval ((show myid)++"HFTiseed") args) 
                                                         else 0--Force initial inventory.         
                                              gses = gaussians
                                              getsent| sentiment == "Calm"      = Calm   
                                                     | sentiment == "Choppy"    = Choppy 
                                                     | sentiment == "Ramp"      = Ramp  
                                                     | sentiment == "Toxic"     = Toxic 
                                                     | otherwise    = error "Unrecognised sentiment."
                                                        where
                                                        sentiment = arg_getstr (si "eerc:1" args 0)
                                              softlimit | (arg_findval "SoftLimit" args) /= (-1) && hetero == 0  = (arg_findval "SoftLimit" args)
                                                        | (arg_findval "SoftLimit" args) == (-1)                 = defsoftlimit                   
                                                        | otherwise       = (arg_findval "SoftLimit" args) * (( (fromIntegral $mod (round $systemTime 1) (myid+1))+1) * 3/(2*fromIntegral(myid+1)))
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
                                              amx = if (arg_findval "amxcoef" args) /= (-1)
                                                    then arg_findval "amxcoef" args
                                                    else 1 
                                              usegauss = (arg_findval "UseGaussians" args) 
                                              hetero = if (arg_findval "hetero" args) /= (-1)
                                                       then arg_findval "hetero" args 
                                                       else 0 
hftwrapper anything args any myid = hftwrapper1 [] anything args any myid

--Trade on the head of the list. [a, b] ++ rest ++ [a]
--Do absolutely nothing [b] ++ rest ++ [b]
--Update inventory based on head of rest and incoming orders, issue trades. rest ++ [b] ++ [newinv]


hftwrapper1 delayedmsgs (Newagstate (invent:irest, oldorders, sent, uids, gses, [softlimit, abslimit, stubsin, stubsize, delay, usegauss, amx, hetero])) args ((time, messages, broadcasts) : simstateinfos) myid
     = (tracemsg ++ cancellations ++ (ordstomsg idtagdfinalorders)) : (hftwrapper1 newdelayedmsgs (Newagstate ((irest ++ [newinvent]), idtagdfinalorders, sent, remaininguids, (drop (ordersperstepperside * 3) gses), [softlimit, abslimit, stubsin, stubsize, delay, usegauss, amx, hetero])) args simstateinfos myid)
       where
       dmessages = if delay /= 0
                   then head delayedmsgs                      -- CDC 9/414 - changed implementation of delays - now applied to messages not inventories
                   else messages                                 -- CDC 9/414 - changed implementation of delays - now applied to messages not inventories
       newdelayedmsgs = if delay ==0 
                        then delayedmsgs                      -- CDC 9/414 - changed implementation of delays - now applied to messages not inventories
                        else (tail delayedmsgs)++[messages]      -- CDC 9/414 - changed implementation of delays - now applied to messages not inventories
       tracemsg = if time == 0
                  then [(datamessage (myid,0) ("HFT" ++ (show myid) ++ "i, HFT" ++ (show myid) ++ " Panicking?," ++ "SoftLimit-" ++ (show myid) ++ ","))] 
                  else [(datamessage (myid,0) ((show newinvent) ++ "," ++ (show panicking) ++ "," ++ (show softlimit) ++ ","))]
                  where
                  panicking = if (abs newinvent) >= softlimit
                              then 1 
                              else 0 
       cancellations = [] -- map (cancelmessage (myid, 1)) (map canceltuple oldorders)
                       -- where
                       -- canceltuple or = (myid, order_getuid or)
       (remaininguids, idtagdfinalorders) = if or [(delay /= 0), (( mod time 2) == 0)]
                                            then applyuids uids finalorders 
                                            else (uids, [])
       newinvent = foldr (+) invent (map getmovement mytrades)
                   where
                   mytrades = map (cpsafehd "hftwrapper1") (filter (not.null) (map msg_gettrade dmessages))   -- CDC 9/4/14 - changed messages to dmessages
                   getmovement o |(isBuySide o)    = (order_getsize o)    
                                 |(isSellSide o)   = (- (order_getsize o)) 
                                 | otherwise  = error "hftwrapper - getmovement - order type not recognised."

       finalorders = bsorders ++ ssorders
       bsorders = (map (tupletoorder False time myid) mybids)
       ssorders = (map (tupletoorder True time myid) myoffers)
       ordstomsg o = map (ordermessage (myid, 1)) o
       (mybids, myoffers) = hftlogic newinvent (si "first" xsum 0) (si "second" xsum 1) (si "third" xsum 9) underlyingvalue (fromIntegral myid) gses stubs softlimit abslimit stubsize usegauss amx hetero -- CDC 17/08/12 added stubsize  18/08/12 added usegauss
       stubs = if stubsin == 1
               then True 
               else  False
       underlyingvalue = if time < 0
                         then (value sent 0) 
                         else (value sent time)
       xsum = if time <= 0 
              then [1993, 2007, 500, 500, 1, 1, 500, 500, 2, 2000]  -- rep 9 0  [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or] --is final element ltp?
              else msg_getnumlistfrombcast (findG0bcast broadcasts)
              where
              findG0bcast [] = error ("No broadcast found?" ++ (show time))
              findG0bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 0)
                                  then f 
                                  else findG0bcast r
       applyuids uids orders = if null orders
                               then (uids, []) 
                               else (remaininguids, fixedorder : rest)
                               where
                               fixedorder = (safehd uids "fixedorder1") (safehd orders "fixedorder2")
                               (remaininguids, rest) = applyuids (tail uids) (tail orders)
       tupletoorder tf ti mid (size, price) = order_create price safesize ti (HFT mid) ty
                                              where
                                              safesize = if size < 0 
                                                         then error "hftwrapper - tupletoorder - safesize"
                                                         else size
                                              ty  |(tf == True) && price == (-1)        = (Sell Andkill)                            --Sell
                                                  |(tf == True) && price /= (-1)       = (Offer (Goodtilldate (ti + 1)))           --Offer
                                                  |(tf == False) && price == (-1)       = (Buy Andkill)                             --Buy
                                                  |(tf == False) && price /= (-1)      = (Bid (Goodtilldate (ti + 1)))             --Bid
           
-- end of HFT
-----------------------------------------