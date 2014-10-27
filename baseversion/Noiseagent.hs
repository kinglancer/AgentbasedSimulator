module Noiseagent(noisywrapper) where



import Comm
import Agent


--parameters
---------------------------------
ltpbound = 12
p_b = 0.5
p_s = 0.5
p_l = 0.35 --The following values give the correct percentages of limit orders, market orders and cancellations once put through the simulator.
p_m = 0.15 --Fiddle with them until you get a proper distribution of each.
p_c = 1 - (p_l + p_m)
p_in = 0.35
p_out = 0.65
sleep_min = 0                --What do we do with these?
sleep_max = secondstosteps 2 --What do we do with these?
ordermean = 4.5
orderstddev = 0.8
k = 1 --exponent for power law number generation.
------------------------------------------------

--check this whole logic again!!
noisylogic inventory myid ti [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, ord, ltp] inrands ingausses uv
  =  if (ty == None) 
     then emptyorder
     else order_create price volume ti (Intermediary myid) ty
     where

--1. "buyside" determines whether the order placed should be a buyside order or a sellside order with a certain probability.

     buyside = if (inrands!!2) > (p_b * 1000)
               then False  --50% chance of being buyside
               else True    --50% chance of being sellside

--2. "ty" determines whether the order should be a market order or a limit order.

     ty  | buyside && (inrands!!7 < (p_l * 1000))             = (Bid Goodtillcancelled)  
         | (not buyside) && (inrands!!7 < (p_l * 1000))          = (Offer Goodtillcancelled) 
         | buyside && (inrands!!7 < ((p_l + p_m) * 1000))     = (Buy Andkill)             
         | (not buyside) && (inrands!!7 < ((p_l + p_m) * 1000))  = (Sell Andkill)            
         |otherwise  = None  --This means our message is to be a cancellation message. 

--3. We draw a number from both a power law distribution and a log normal distribution for use later.

     powdval = (((inrands!!0)/100)^(-k)) / (10^13) --Max is 10
     logdval = (lognormal ((abs (head ingausses))+ 0.00001)) * 5 

--4. This generates a base price somewhere within the spread using a normal distribution. This is only used if the price chosen is within the spread.

     basepricein = bb + (abs (((inrands!!0)/1000) * (bo - bb)))

--5. Here we generate the price. If the price is within the spread (determined by p_in & p_out probabilities) we use our base price generated in step 4.
--If we are placing an order outside the spread then we use out power law distribution number pulled earlier to pick a price so far from the bb or bo depending on the side of the book our order is on.
--If the spread is difficult to determine (bb or bo return 0) then we just use the underlying value with a small offset decided by the side of the book the order is on.

     price| or [bb == 0, bo == 0]         = maximum [(minimum [uv + uvoffset, ltp+12]), ltp-12]                                       
          | (inrands!!4) < (p_in * 1000) = maximum [(minimum [basepricein,(ltp + ltpbound)]), (ltp - ltpbound)]                        --first inrands was gaussians
          | not buyside                    = maximum [(minimum [(bo + ((powdval/6) * (bo - bb))),(ltp + ltpbound)]), (ltp - ltpbound)]   --No idea.
          | buyside                     = maximum [(minimum [(bb - ((powdval/6) * (bo - bb))),(ltp + ltpbound)]), (ltp - ltpbound)] 
     uvoffset = if buyside 
                then -1
                else 1

--6. If the order being placed is a market order then the volume is just the total volume at the top of the book.
--   Andrei's paper said that "the volume of market orders is almost always the same as the best counterpart's" so I thought using the liquidity at the top of the book was a good idea.
--   At the moment the top of the book is defined by the Exchange as being all orders within 0.05% of the leading price.
--   If the order is a limit order then the size is determined by the number drawn from the log normal distribution earlier.
--   I then clamp both to a maximum 2000 and a minimum of 400.

     volume = maximum [(minimum [pvolume, 2000]), 400] --was 50
     pvolume 
            | (ty == (Buy Andkill))  = bsd  --volume of market orders is almost always the same as the best counterpart's?????
            | (ty == (Sell Andkill)) = ssd  --What should we do about these two?
            | otherwise = abs (logdval)  

--map ((*1000).(lognormal).(!1).(converse drop (rands 2))) [15,30..]
--sort (take 1000 (map ((converse (/) (10^13)).(converse (^) (-k)).(converse (/) 100).(lognormal).(!1).(converse drop (rands 2))) [15,30..]))

                        
lognormal x = (((x) * (2 * pi)^(-2))^(-1)) * exp((-0.5) * (log((x)))^2)                  --need to confirm

noisywrapper :: Agent_t
noisywrapper (Emptyagentstate) args any myid = noisywrapper (Newagstate ([0], [], getsent, (map (order_setuid "noisywrapper") [0..]), (map fromIntegral (drop 15 rnds)), [])) args any myid
                                               where
                                               rnds = if (arg_findval "randseed" args) /= -1
                                                      then drop myid randoms 
                                                      else drop (round $(fromIntegral myid) * (arg_findval "randseed" args)) randoms
                                               getsent| (arg_getstr (si "eerc:1" args 0)) == "Calm"            = Calm   
                                                      | (arg_getstr (si "errc:2" args 0)) == "Choppy"          = Choppy 
                                                      | (arg_getstr (si "errc:3" args 0)) == "Ramp"            = Ramp   
                                                      | (arg_getstr (si "errc:4" args 0)) == "Toxic"           = Toxic 
                                                      |  otherwise = error "Unrecognised sentiment."

noisywrapper (Newagstate ([invent], oldorders, sent, uids, rnds, any)) args ((ti, messages, broadcasts) : simstateinfos) myid 
     = [msgg] : (noisywrapper (Newagstate ([newinvent], ordersl, sent, remaininguids, (drop 15 rnds), any)) args simstateinfos myid)
       where
       (msgg, ordersl) | (ordr == emptyorder) && (not $null oldorders ) = ((cancelmessage (myid, 1) (myid, order_getuid (head oldorders))), (tail oldorders))
                       | (ordr == emptyorder)                            = (hiaton, oldorders) 
                       | otherwise                                       = ((ordermessage (myid, 1) (head forders)), oldorders ++ forders) 
       (remaininguids, forders) = applyuids uids [ordr]
       underlyingvalue = if ti < 0
                         then (value sent 0) 
                         else (value sent ti)
       newinvent = foldr (+) invent (map getmovement mytrades)
                   where
                   mytrades = map (cpsafehd "noisywrapper1") (filter (not.null) (map msg_gettrade messages))
                   getmovement o |(isBuySide o)    = (order_getsize o)    
                                 |(isSellSide o)  = (- (order_getsize o)) 
                                 | otherwise     = error "noisywrapper - getmovement - order type not recognised."
       ordr = noisylogic invent myid ti xsum rnds (drop (floor ((head rnds)/100)) gaussians) underlyingvalue --Must pass in different list of gaussians each time.
       xsum = if ti <= 0
              then [1993, 2007, 500, 500, 1, 1, 500, 500, 2, 2000]  -- rep 9 0  [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or]
              else msg_getnumlistfrombcast (findG0bcast broadcasts)
              where
              findG0bcast [] = error ("No broadcast found?" ++ (show ti))
              findG0bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 0)
                                  then f 
                                  else findG0bcast r          
                                  
--end of Noiseagent
--------------------------------------