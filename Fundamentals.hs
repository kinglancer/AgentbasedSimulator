module Fundamentals where

import Comm
import Agent


--Fundamentals
targetinvent = 40000
period = 300
booster = 2 --Just to make orders SMALLER(?).
ltpleeway = 12
maxordersz = 200


-- Information:
-- ----
-- The wrapper looks for an argument specifying whether this agent instance should behave as a buyer or seller.
-- The wrappers have periodic goals:
-- An inventory they wish to accrue over a period of time.
-- After this time period passes their inventory resets.
-- Their strategies become more aggressive as they approach their deadlines.
-- Now, details¡­
-- ?	First, a base price is calculated: this is simply the (target invent/period * (current time MODULO period)) + 1
-- ?	Next, we calculate a multiplier for this base size. This reflects our desire to order more if prices are favourable.
-- o	This is simply 1 if they aren¡¯t.
-- o	If prices are favourable, a.k.a. the underlying value of an item is higher than the best offer if we¡¯re buying and vice versa for selling then we multiply the difference between these two values by a predefined constant ¡°booster¡±.
-- ?	The product of the multiplier and base size is the new size, we then ensure this new size is less than the max order size, if it isn¡¯t then it becomes the max order size, then we check that the remaining stock we wish to purchase/sell is not smaller than this size, if it is we just place an order for the remaining stock we wish to buy/sell.
-- ?	We now calculate the price:
-- o	We simply use the underlying value if it is within the exchange enforced price bands and the current prices are favourable.
-- o	Otherwise buyers place orders at underlying value ¨C 2 and sellers at underlying value + 2. (Still respecting price banding of course)




fundlogic inventory myid ti [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, ord, ltp] inrands ingausses uv buyer sig
   = if buyer == True
     then order_create price actualsize ti sig (Bid Goodtillcancelled) 
     else order_create price actualsize ti sig (Offer Goodtillcancelled)
     where
     basesize = ((targetinvent/period) * (fromIntegral ti)) + 1 --A base price is calculated: this is simply the (target invent/period * (current time MODULO period)) + 1
     actualsize = if sz <= remaining
                  then minimum [maxordersz, sz]  --If order size is larger than max allowed order then place max allowed order. If size is smaller than remaining stock then place size.
                  else maximum [1, minimum [maxordersz,remaining]] -- Otherwise remaining stock.
                  where
                  sz = (basesize * multiplier)
                  remaining = (targetinvent - inventory) --Stock left to buy/sell.
  -- Multiplier for this base size. Reflects our desire to order more if prices are favourable.
     multiplier| or [((uv <= bo) && (buyer == True)), ((uv >= bb) && (buyer == False))]  = 1                                 --Prices aren't favourable.'
               | buyer == True                                                        = 1 + (booster * (abs (uv - bo)))    --Good for buying.
               | buyer ==False                                                       = 1 + (booster * (abs (uv - bb)))    --Good for selling.
     bonprix = not(or [((uv <= bo) && (buyer == True)), ((uv >= bb) && (buyer == False))])
     price| (between uv (ltp - ltpleeway) (ltp + ltpleeway)) && bonprix = uv                                                       --If prices favourable then offers are at the underlying value.
          | buyer == True                                               = maximum [ltp - ltpleeway, (minimum [ltp + ltpleeway, uv - 2])]   --Otherwise just below the underlying value for buyers.
          | buyer == False                                              = minimum [ltp + ltpleeway, (maximum [ltp - ltpleeway, uv + 2])]   --Otherwise just above the underlying value for buyers.


fundwrapper :: Agent_t
fundwrapper (Emptyagentstate) args any myid = fundwrapper (Newagstate ([0], [], getsent, (map (order_setuid "fundwrapper") [0..]),  (map fromIntegral(drop 15 rnds)), [])) args any myid
                                               where
                                               rnds = drop myid randoms
                                               getsent| (arg_getstr (si "eerc:1" args 0)) == "Calm"    = Calm      
                                                      | (arg_getstr (si "errc:2" args 0)) == "Choppy"  = Choppy    
                                                      | (arg_getstr (si "errc:3" args 0)) == "Ramp"    = Ramp      
                                                      | (arg_getstr (si "errc:4" args 0)) == "Toxic"   = Toxic     
                                                      | otherwise   = error "Unrecognised sentiment."

fundwrapper (Newagstate ([invent], oldorders, sent, uids, rnds, any)) args ((ti, messages, broadcasts) : simstateinfos) myid 
     = [msgg] : (fundwrapper (Newagstate ([newinvent], oldorders, sent, remaininguids, (drop 15 rnds), any)) args simstateinfos myid)
       where
       (msgg) = (ordermessage (myid, 1) (head forders))
       (remaininguids, forders) = applyuids uids [ordr]
       underlyingvalue = if ti < 0 
                         then (value sent 0)
                         else(value sent ti)
       (mytraderid, buyer)| (arg_getstr (args!!(myid-1))) == "FundBuyer"   = ((FundBuyer myid), True)   
                          | (arg_getstr (args!!(myid-1))) == "FundSeller"  = ((FundSeller myid), False) 
                          | otherwise = error "traderagent - unrecognised trader category"
       newinvent = foldr (+) invent (map getmovement mytrades)
                   where
                   mytrades = map (cpsafehd "fundwrapper1") (filter (not.null) (map msg_gettrade messages))
                   getmovement o |(isBuySide o)   = (order_getsize o)     
                                 |(isSellSide o)  = (- (order_getsize o))  
                                 |otherwise  = error "fundwrapper - getmovement - order type not recognised."
       ordr = fundlogic invent myid ti xsum rnds (gaussians) underlyingvalue buyer mytraderid
       xsum = if ti <= 0 
              then [1993, 2007, 500, 500, 1, 1, 500, 500, 2, 2000] -- rep 9 0  [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or]
              else msg_getnumlistfrombcast (findG0bcast broadcasts)
              where
              findG0bcast [] = error ("No broadcast found?" ++ (show ti))
              findG0bcast (f:r) = if (msg_isbroadcast f) && ((msg_getid f) == 0) 
                                  then f
                                  else findG0bcast r


--Helpers
-----------

                            