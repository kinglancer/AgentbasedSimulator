module Nicemime(nice_mime_wrapper) where

import Comm
import Agent
import Order




--nicemime
percent = 0.05 --For bs/ss depth near top.
percentleeway = 0.05 -- Accept no orders beyond this leeway
pricebandingvariation :: Double
pricebandingvariation = 12 --Price banding - Any new limit order must be within -+ 12.0 (48 ticks) of the last traded price. 
spiketrigger :: Double
spiketrigger = 600
ordermax::Double
ordermax = 2000 -- cmegroup.com E-mini FAQ
defmrt::Int
defmrt = 0 -- Minimum resting time.
maxordersallowed :: Double
maxordersallowed = 1000000 -- Total allowed shares on book for any one agent. 





--nice_mime takes its old state, a list of orders, its own ID and a list of cancellations and returns a new state and a list of outgoing messages
--It does this by first pruning the book for expired entries and cancelled ones, then it places the new limit orders on the book, then uncrosses the book,
--and finally attempts to execute all the market orders it received as well.
nice_mime :: Nice_mime_lob -> [Order] -> Int -> [(Int,Int)] -> Time -> (Nice_mime_lob, [Msg_t])
nice_mime reallyoldlob orders myid cancellations mrt
         -- If the lob is not spiked then just return the final state of the new lob as well as all the messages generated.
       | isspiked == False   = (flob, tmsg : ibmsg : bmsg : (msgs1 ++ msgs2 ++ msgs3 ++ cmsgs))
         -- If the lob is spiked then only return the log with limit orders appended and the relevent messages including a rejection to all market orders.
       | isspiked == True    = (fstlob, stmsg : sbmsg : (msgs1 ++ cmsgs ++ rejectallmorders ++ spikecast))
         where
         rejectallmorders = (map spikerejectorder morders)
         (oldlob, isspiked) = nice_mime_lob_isspiked reallyoldlob
         spikecast = [(broadcastmessage (myid,1) (broadcast_str (Str "The order book is spiking.")))]
         sbmsg = (broadcastmessage (myid,0) (broadcast_numlist (nice_mime_lob_getsummary fstlob ((length tsnorders) + (length morders)))))
         stmsg =if (nice_mime_lob_gettime reallyoldlob) /= (-1)   
                then (datamessage (myid,0) (nice_mime_lob_gettrace fstlob)) 
                else (datamessage (myid,0) ((nice_mime_lob_gettraceheaders)))
         bmsg = (broadcastmessage (myid,0) (broadcast_numlist (nice_mime_lob_getsummary flob ((length tsnorders) + (length morders)))))
         bcast_numlist :: Broadcast_t                        -- really weird, type ambiguous
         bcast_numlist = broadcast_numlist (nice_mime_lob_getsummary fstlob ((length tsnorders) + (length morders)))
         ibmsg = (debugmessage (myid,0) (show bcast_numlist)) -- CDC 2014
         tmsg = if (nice_mime_lob_gettime reallyoldlob) /= (-1)  
                then (datamessage (myid,0) (nice_mime_lob_gettrace flob)) 
                else (datamessage (myid,0) ((nice_mime_lob_gettraceheaders)))
         (norders, morders) = ((filter (myor (isOtype "Bid") (isOtype "Offer")) orders), (filter ((myor (myor ((==) (Sell Andkill)) ((==) (Sell Orkill)) ) (myor ((==) (Buy Andkill)) ((==) (Buy Orkill)) )).order_gettype) orders))
         tsnorders = zipfunc (map order_setfractime [0..]) norders    --fractional timestamped limit orders
         (prunedlob, cmsgs) = nice_mime_lob_prune (nice_mime_lob_incrementtime oldlob) cancellations myid mrt
         (fstlob, msgs1) = (nice_mime_lob_appendorders prunedlob tsnorders myid  mrt)
         (sndlob, msgs2) = (nice_mime_lob_uncross_book fstlob myid)
         flob = (nice_mime_lob_updatestats wlob)
         (wlob, msgs3) = executeMorders sndlob morders
                         where
                         executeMorders mylob [] = (mylob, [])
                         executeMorders mylob (top:tail) = returntuple
                                                           where
                                                           returntuple = if isspiked == False 
                                                                         then (myflob, messages ++ rest) 
                                                                         else (myflob, rest)
                                                           (myilob, messages, order) = if top /= emptyorder 
                                                                                       then  nice_mime_lob_execute_order mylob top myid 
                                                                                       else error ("nice_mime - top is an empty order.")
                                                           (myflob, rest) = if isspiked == False
                                                                            then executeMorders myilob tail 
                                                                            else (pslob, (map spikerejectorder (top:tail)))
                                                           (pslob, isspiked) = nice_mime_lob_triggerspike mylob top --pslob = potentially spiked lob
         spikerejectorder ordr = (ackmessage (myid, (order_gettraderidno ordr)) 6 ordr "Temporarily stopped trading. Stop-spike protection triggered.")

nice_mime_lob_gettime (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = time

--4.	The book is now checked to see if it is crossed and if so, uncrossed. (nice_mime_lob_uncross_book)
--     a.	If there are no bids, no offers or the book is not crossed then just return the state received.
--     b.	Otherwise, take the best bid and execute it
--
-- TEMPORARILY DISABLED - CDC 4/4/2014
--
nice_mime_lob_uncross_book :: Nice_mime_lob -> Int -> (Nice_mime_lob, [Msg_t])
nice_mime_lob_uncross_book (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) myid = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), []) -- , if or [(bids = []), (offers = [])]
--nice_mime_lob_uncross_book lob myid = (lob, []), if (nice_mime_lob_getbestbid lob) < (nice_mime_lob_getbestoffer lob)
--                                    = (finallob, moremessages ++ messages), otherwise
--                                      where
--                                      (finallob, moremessages) = nice_mime_lob_uncross_book newlob myid
--                                      (newlob, messages, retorder) = (nice_mime_lob_execute_order lob (nice_mime_lob_getleaderfrombids lob) myid), if (nice_mime_lob_getleaderfrombids lob) ~= emptyorder ||May want to do something with retorder...
--                                                                   = error ("nice_mime_lob_uncross_book - getleaderfrombids returned emptyorder " ++ (show lob)), otherwise

shownice_mime_lob (Nice_mime_lob sent totlist bids offers time ticksize ltp sts)
 = "\nNice_mime_lob @ t" ++ (show time) ++ "\nlast traded price: " ++ (show ltp) ++ "\nbids: " ++ (show bids) ++ "\noffers: " ++ (show offers) 


--     nice_mime_lob_execute_order
--i.	This takes an order and executes trades between it and the leading orders on the opposite side.
--ii.	It updates/removes orders as it executes trades and creates the relevant trade messages (trademessage).
--iii.	The price traded at is the price specified by the older of the two messages. (Hence the fractional times mentioned earlier)
--iv.	Many specific cases, each is presented in the where block below.
nice_mime_lob_execute_order :: Nice_mime_lob -> Order -> Int -> (Nice_mime_lob, [Msg_t], Order) --order is remaining order.
nice_mime_lob_execute_order (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) order myid
       |(not (nice_mime_lob_checkorderallowed totlist order)) && (or [((order_gettype order) == (Buy Andkill)), ((order_gettype order) == (Sell Andkill))])                                                                                      
          = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), [(ackmessage (myid, (order_gettraderidno order)) 1 order "buy/sell was too big.")], order)    
       |(or [(not (nice_mime_lob_checkorderallowed totlist order)), ((nice_mime_lob_getsellsideliquidity (Nice_mime_lob sent totlist bids offers time ticksize ltp sts)) < (order_getsize order))]) && ((order_gettype order) == (Buy Orkill))  
          = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), [(ackmessage (myid, (order_gettraderidno order)) 1 order "buy or kill was too big.")], order)  -- (Buy Orkill) too big
       |(or[(not (nice_mime_lob_checkorderallowed totlist order)), ((nice_mime_lob_getbuysideliquidity (Nice_mime_lob sent totlist bids offers time ticksize ltp sts)) < (order_getsize order))]) && ((order_gettype order) == (Sell Orkill))  
          = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), [(ackmessage (myid, (order_gettraderidno order)) 1 order "sell or kill was too big.")], order)  -- (Sell Orkill) too big
       | otherwise = (finallob, restofmsgs, ro)  --Potential problem area for missing messages
          where

          (finallob, restofmsgs, ro)   --Cases below

                                       --If the order is a buy or sell and the order has been exhausted then just return the lob & return emptyorder.
                                    | ((order_getsize order) == 0) && (or [((order_gettype order) == (Buy Andkill)),((order_gettype order) == (Sell Andkill)), ((order_gettype order)== (Sell Orkill)), ((order_gettype order) == (Buy Orkill))]) 
                                     = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), [(ackmessage (myid, (order_gettraderidno order)) 0 order "")], emptyorder) 
                                        

                                       --If you've run out of liquidity on a side and you're dealing with a limit order just return.
                                    | null list && (or [(isOtype "Offer" order),(isOtype "Bid" order)])  
                                     = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), [], order)
                                        

                                       --If you've run out of liquidity on a side and you're dealing with a X and kill market order then return reject.
                                    | null list   
                                     = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), [(ackmessage (myid, (order_gettraderidno order)) 2 order "no liquidity on opposite side.")], order)
                                        


                                       --If the order is a bid and no liquidity remains on the first sell side level then return the lob without the exhausted level & remaining order.
                                    | ((length (snd (safehd list "nice_mime_lob_execute_order1"))) == 0) && (isOtype "Bid" order)  
                                     = ((Nice_mime_lob sent totlist nbids (tail list) time ticksize ltp sts), [], order) 

                                       --If the order is a (Buy Andkill) and no liquidity remains on the first sell side level then continue iterating without the exhausted level.
                                    | ((length (snd (safehd list "nice_mime_lob_execute_order2"))) == 0) && ((order_gettype order) == (Buy Andkill)) 
                                     = nice_mime_lob_execute_order (Nice_mime_lob sent totlist bids (tail list) time ticksize ltp sts) order myid 
                                        

                                       --If the order is an offer and no liquidity remains on the first buy side level then return the lob without the exhausted level & remaining order.
                                    | ((length (snd (safehd list "nice_mime_lob_execute_order3"))) == 0) && (isOtype "Offer" order)   
                                     = ((Nice_mime_lob sent totlist (tail list) noffers time ticksize ltp sts), [], order)

                                       --If the order is a (Sell Andkill) and no liquidity remains on the first sell buy level then continue iterating without the exhausted level.
                                    | ((length (snd (safehd list "nice_mime_lob_execute_order4"))) == 0) && ((order_gettype order) ==(Sell Andkill))  
                                     = nice_mime_lob_execute_order (Nice_mime_lob sent totlist (tail list) offers time ticksize ltp sts) order myid

                                       --If the order has been exhausted and it's not a (Buy Andkill) or a (Sell Andkill) then return the lob with it removed.
                                    | ((order_getsize order) == 0)  
                                     = (tickAndOrderRemovedLob,[], emptyorder)

                                       --If the old order is non-zero then match! (Recall price banding from nice_mime_lob_checkorderallowed ensures that SSL will never be triggered)
                                    | ((order_getsize order) > 0)   
                                     = ((nice_mime_lob_inccumtradevol voltr (nice_mime_lob_inctrades nlob)), msgs ++ nmsgs, forder)

                                    |((order_getsize order) < 0)   = error ("nice_mime_lob_execute_order - The order went into the negative." ++ (show order)) 
                                    |otherwise                     = error "nice_mime_lob_execute_order - There's something you've missed entirely."
                                       where
                                       nbids = nice_mime_lob_replaceorder bids order False
                                       noffers = nice_mime_lob_replaceorder offers order False
                                       tickAndOrderRemovedLob | (isOtype "Bid" order)    = (Nice_mime_lob sent totlist removeTickAndOrder offers time ticksize ltp sts)
                                                              | (isOtype "Offer" order)  = (Nice_mime_lob sent totlist bids removeTickAndOrder time ticksize ltp sts)
                                                              | otherwise                = error "nice_mime_lob_execute_order - Tried to remove order for a (Sell Andkill) or (Buy Andkill)."
                                       removeTickAndOrder |not $null(snd (safehd removeOrder "nice_mime_lob_execute_order5"))  = removeOrder
                                                          |null (snd (safehd removeOrder "nice_mime_lob_execute_order6"))      = (tail removeOrder) 
                                                          |otherwise         = error "nice_mime_lob_execute_order - Paranoia consoling error for removeTickAndOrder."
                                       removeOrder = if (order_samesignature order (safehd (snd (safehd otherlist "nice_mime_lob_execute_order10")) "nice_mime_lob_execute_order9")) 
                                                     then ((fst (safehd otherlist "nice_mime_lob_execute_order7")), (tail (snd (safehd otherlist "nice_mime_lob_execute_order8")))) : (tail otherlist) 
                                                     else error ("nice_mime_lob_execute_order - Mismatching orders at heads of lists.\n" ++ (show order) ++"\n" ++ (show (safehd (snd (safehd otherlist "nice_mime_lob_execute_order10")) "nice_mime_lob_execute_order9")) )
                                       (intermediateorder, norder2, msgs, nltp, voltr) = nice_mime_makeatrade order order2 myid
                                       order2 = (safehd (snd (safehd list "nice_mime_lob_execute_order11")) "nice_mime_lob_execute_order12")
                                       (nlob, nmsgs, forder) = nice_mime_lob_execute_order intermediatelob intermediateorder myid
                                       intermediatelob = nice_mime_lob_setltp nltp (nice_mime_lob_updatetotal (nice_mime_lob_updatetotal notilob order2 norder2) order intermediateorder)
                                       notilob|((order_getsize norder2) > 0) && (isSellSide norder2)   = (Nice_mime_lob sent totlist bids (nice_mime_lob_replaceorder offers norder2 False) time ticksize (order_getprice order2) sts)
                                              |((order_getsize norder2) == 0) && (isSellSide norder2)    = (Nice_mime_lob sent totlist bids (nice_mime_lob_replaceorder offers norder2 True) time ticksize (order_getprice order2) sts)
                                              |((order_getsize norder2) > 0) && (isBuySide norder2)    = (Nice_mime_lob sent totlist (nice_mime_lob_replaceorder bids norder2 False) offers time ticksize (order_getprice order2) sts)
                                              |((order_getsize norder2) == 0) && (isBuySide norder2)     = (Nice_mime_lob sent totlist (nice_mime_lob_replaceorder bids norder2 True) offers time ticksize (order_getprice order2) sts)
                                              |otherwise   = error "nice_mime_lob_execute_order - intermediate lob could not be generated. intermediate order problems."

          list |isBuySide order              = offers --The opponent list to the order being executed.
               |isSellSide order             = bids 
               |order == emptyorder          = error ("nice_mime_lob_execute_order - Emptyorder " ++ (show order)) 
          otherlist| isSellSide order   = offers --The list that contains the order being executed.
                   | isBuySide order    = bids 
                   | order == emptyorder = error ("nice_mime_lob_execute_order - Emptyorder " ++ (show order))

offersareempty (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
  = if (length offers) == 0
    then True 
    else False

nice_mime_lob_updatetotlist :: [Double] -> Order -> Order -> [Double]
nice_mime_lob_updatetotlist totlist oldorder neworder 
       = newtotlist
         where
         (Nice_mime_lob sent newtotlist bids offers time ticksize ltp sts) = nice_mime_lob_updatetotal (Nice_mime_lob sent totlist [] [] (-1) 25 1700 []) oldorder neworder

nice_mime_lob_setltp nltp (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = (Nice_mime_lob sent totlist bids offers time ticksize nltp sts)


nice_mime_lob_updatetotal (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) oldorder neworder 
          = (Nice_mime_lob sent newtotlist bids offers time ticksize ltp sts)
            where
            newtotlist = (take trid totlist) ++ [newvalue] ++ (drop (trid+1) totlist)
            trid = order_gettraderidno neworder
            difference = if or [order_samesignature neworder oldorder, oldorder == emptyorder]
                         then (order_getsize oldorder) - (order_getsize neworder) 
                         else error "nice_mime_lob_updatetotal - Orders don't match!"
            newvalue | isBuySide neworder  = (totlist!!trid) + difference 
                     | isSellSide neworder = (totlist!!trid) - difference 

nice_mime_lob_replaceorder :: [Tick] -> Order -> Bool -> [Tick] --Bool = delete if True replace if False
nice_mime_lob_replaceorder list iorder flag = newlist
                                              where
                                              order = if flag == True
                                                      then [] 
                                                      else [iorder]
                                              newlist = if tindex /= -1 
                                                        then (take tindex list) ++ newtick ++ (drop (tindex + 1) list) 
                                                        else error ("nice_mime_lob_replaceorder - findindexoffirst failed " ++ (show list)++ "\n" ++ (show iorder))
                                              tindex = findindexoffirst list ((== (order_getprice iorder)).fst)                       -- not sure
                                              newtick = if oindex /= -1 
                                                        then [((order_getprice iorder),(take oindex olist) ++ order ++ (drop (oindex + 1) olist))]
                                                        else error ("nice_mime_lob_replaceorder - findindexoffirst failed " ++ (show olist) ++ "\n" ++ (show iorder))
                                              olist = if tindex /= -1 
                                                      then snd (list!!tindex) 
                                                      else error ("nice_mime_lob_replaceorder - findindexoffirst failed " ++ (show list)++ "\n" ++ (show iorder))
                                              oindex = findindexoffirst olist (order_samesignature iorder)
                                        

-- nice_mime_lob_isspiked checks to see if the lob state passed as an argument is spiked.
-- If so it decrements one from the remaining spike time and returns the new state & a boolean indicating that the lob is spiked.
-- Otherwise it just returns the state it received as well as False.
nice_mime_lob_isspiked :: Nice_mime_lob -> (Nice_mime_lob,Bool)
nice_mime_lob_isspiked (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
    | (length sts) < 31  = error "nice_mime_lob_isspiked - stats too short"
    | (sts!!30) /= 0 = (nlob, True)
    | otherwise  = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), False)
      where
      nlob = (Nice_mime_lob sent totlist bids offers time ticksize ltp (replace 30 sts ((sts!!30) - 1))) 

nice_mime_lob_triggerspike :: Nice_mime_lob -> Order -> (Nice_mime_lob,Bool)
nice_mime_lob_triggerspike (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) morder 
  = if consensus == True 
    then (newlob,consensus)
    else ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts), consensus)
    where
    newlob = (Nice_mime_lob sent totlist bids offers time ticksize ltp (replace 30 sts (secondstosteps 4)))
    consensus = if or [((finaltradedprice >= (ltp - spiketrigger)) && (finaltradedprice <= (ltp + spiketrigger))), (finaltradedprice == (-1))]  
                then False 
                else True
    list |isBuySide morder       = offers --The opponent list to the order being executed.
         |isSellSide morder      = bids 
         |morder == emptyorder    = error ("triggerspike1" ++ (show morder))
    finaltradedprice = getfinaltradedprice list (order_getsize morder) (-1)
                       where
                       getfinaltradedprice ticks orderleft lastprice = if or [(orderleft < 0), (null ticks)] 
                                                                       then lastprice 
                                                                       else getfinaltradedprice tickrest neworderleft tickprice
                                                                       where
                                                                       tickrest = (tail ticks)
                                                                       tickprice = (fst (head ticks))
                                                                       neworderleft = orderleft - tickfullsize
                                                                       tickfullsize = (foldr (+) 0 (map order_getsize orders))
                                                                       orders = (snd (head ticks))


nice_mime_makeatrade :: Order -> Order -> Int -> (Order, Order, [Msg_t], Double, Double)
nice_mime_makeatrade order1 order2 myid = (norder1, norder2, [(trademessage (myid, order_gettraderidno order1) cnorder1 cnorder2), (trademessage (myid, order_gettraderidno order2) cnorder2 cnorder1)], tradeprice, tradedvol)
                                           where
                                           tradeprice = getrestingprice order1 order2
                                           cnorder1 = order_newsize (order_setprice tradeprice norder1) order1diff --Order with corrected price.
                                           cnorder2 = order_newsize (order_setprice tradeprice norder2) order2diff --Order with corrected price.
                                           norder1 = if ((order_getsize order1) - (order_getsize order2)) <= 0 
                                                     then order_newsize order1 0
                                                     else order_newsize order1 ((order_getsize order1) - (order_getsize order2))
                                           norder2 = if ((order_getsize order2) - (order_getsize order1)) <= 0 
                                                     then order_newsize order2 0
                                                     else order_newsize order2 ((order_getsize order2) - (order_getsize order1))
                                           order1diff = (order_getsize order1) - (order_getsize norder1) --Traded amount
                                           order2diff = (order_getsize order2) - (order_getsize norder2) --Traded amount
                                           tradedvol = order_getsize cnorder2




nice_mime_emptylob = Nice_mime_lob Calm [0,0..] [] [] (-1) 25 2000 (rep 34 0)

nice_mime_lob_inctrades (Nice_mime_lob sent totlist bids offers time ticksize ltp (tr:rest)) = (Nice_mime_lob sent totlist bids offers time ticksize ltp ((tr+1):rest))

nice_mime_wrapper :: Agent_t
nice_mime_wrapper state args ((time, messages, broadcasts) : simstateinfos) myid 
       =  msgs : nice_mime_wrapper (Nicemimestate newstate) args simstateinfos myid 
          where
          mymrt = if (arg_findval "MinimumRestingTime" args) /= (-1)
                  then round $arg_findval "MinimumRestingTime" args 
                  else defmrt
          checkstate = if state == Emptyagentstate 
                       then nice_mime_lob_setsent getsent (nice_mime_emptylob) 
                       else (internalstate state) -- look here
                       where
                       internalstate (Nicemimestate astate) = astate
          (newstate, msgs) = nice_mime checkstate orders myid (concat (map msg_getcanceltuple messages)) mymrt
          orders   = (filterorders messages)
                     where
                     filterorders qx = msg_getorders qx
          getsent |(arg_getstr (args!!0)) == "Calm"     = Calm  
                  |(arg_getstr (args!!0)) == "Choppy"   = Choppy 
                  |(arg_getstr (args!!0)) == "Ramp"     = Ramp   
                  |(arg_getstr (args!!0)) == "Toxic"    = Toxic 
                  |otherwise                           = error "Unrecognised sentiment."

nice_mime_lob_setsent sent (Nice_mime_lob osent totlist bids offers time ticksize ltp sts) = (Nice_mime_lob sent totlist bids offers time ticksize ltp sts)



--nice_mime_lob_appendorders appends a list of orders to the book should they meet certain criteria and generates their ack messages.
--Otherwise it rejects them and creates an ack message corresponding to the reason it was rejected.
--Below is a summary of how it works.
--i.	Take the order at the front of the list of orders.
--ii.	Recursively call (nice_mime_lob_appendorders) with the rest of the orders.
--    1.	The base case is when the order list is [] and it just returns the state it received with no messages.
--iii.	Round the order to the nearest tick. For bids we round down and for offers we round up.
--iv.	Append order to the book returned form the recursive call in the respective list + tick & generate a successful ack message unless one of the following criteria are met, in which case do not append and create unsuccessful ack message with the corresponding ack code.
--    1.	The order is set to expire before (time + minimum resting time).  Ackcode: 7
--    2.	The order exceeds the maximum size for a single order. Ackcode: 1
--    3.	The order causes the party who made it to exceed their total number of contracts allowed on the book. Ackcode: 4
--    4.	The order was not within LTP +/- priceband. Ackcode: 3

nice_mime_lob_appendorders (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) orders myid mrt
     = if null orders  
       then (Nice_mime_lob sent totlist bids offers time ticksize ltp sts, [])
       else (finallob, anack : otheracks)             
       where
       (finallob, otheracks) = nice_mime_lob_appendorders newlob (tail orders) myid mrt
       fixedorder = fixtick (getticksize (safehd orders "nice_mime_lob_appendorders1") ticksize) (safehd orders "nice_mime_lob_appendorders2")

                        --If price is negative, REJECT.
       (newlob, anack) | (order_getprice fixedorder) < 0
                        = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts) ,(ackmessage (myid, (order_gettraderidno fixedorder)) 3 fixedorder "the order was not in the window of acceptable prices"))
                       
                         -- If order does not obey minimum resting time REJECT.
                       | and [((order_getexpiry fixedorder) < (time + mrt)),((order_getexpiry fixedorder) /= (-1))] 
                        = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts) ,(ackmessage (myid, (order_gettraderidno fixedorder)) 7 fixedorder "MRT not obeyed."))
                       
                         --If bid is within leeway and it is less than the ordermaximum.
                       |(isOtype "Bid" fixedorder) && ((order_getsize fixedorder) <= ordermax) && ((order_getprice fixedorder) >= (ltp - pricebandingvariation)) && 
                         (nice_mime_lob_checkorderallowed totlist fixedorder) && ((order_getprice fixedorder) <= (ltp + pricebandingvariation))  
                        = (myupdatelob (Nice_mime_lob sent totlist (putintick ticksize bids fixedorder) offers time ticksize ltp sts), 
                           (ackmessage (myid, (order_gettraderidno fixedorder)) 0 fixedorder ""))            
                       
                         --If offer is within leeway and is less than ordermaximum.
                       |(isOtype "Offer" fixedorder) && ((order_getsize fixedorder) <= ordermax) && ((order_getprice fixedorder) <= (ltp + pricebandingvariation)) && 
                         (nice_mime_lob_checkorderallowed totlist fixedorder) && ((order_getprice fixedorder) >= (ltp - pricebandingvariation)) 
                        = (myupdatelob (Nice_mime_lob sent totlist bids (putintick ticksize offers fixedorder) time ticksize ltp sts), (ackmessage (myid, (order_gettraderidno fixedorder)) 0 fixedorder ""))
                                          
                         --If not, REJECT.
                       |(order_getsize fixedorder) > ordermax 
                        = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts) ,(ackmessage (myid, (order_gettraderidno fixedorder)) 1 fixedorder "the order was too large."))
                       
                         --If not, REJECT.
                       | not (nice_mime_lob_checkorderallowed totlist fixedorder) 
                        = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts) ,(ackmessage (myid, (order_gettraderidno fixedorder)) 4 fixedorder "Exceeded total allowed contracts on the book."))
                       
                         --If not, REJECT.
                       |otherwise  
                        = ((Nice_mime_lob sent totlist bids offers time ticksize ltp sts) ,(ackmessage (myid, (order_gettraderidno fixedorder)) 3 fixedorder "the order was not in the window of acceptable prices"))
       myupdatelob x = nice_mime_lob_updatetotal x emptyorder fixedorder



putintick ticksize ticks order 
    | (mymod (order_getprice order)  (getticksize order ticksize)) /= 0   = error ("Order is not on tick. price 1: " ++ (show (order_getprice order)) ++ " price2: " ++ (show (getticksize order ticksize)))
    | null ticks    = [((order_getprice order), [order])]
    | (fst (safehd ticks "putintick3")) == (order_getprice order)  = ((fst (safehd ticks "putintick1")), order : (snd (safehd ticks "putintick2"))) : (tail ticks)
    | (or [(((fst (safehd ticks "putintick4")) < (order_getprice order)) && (isOtype "Bid" order)), (((fst (safehd ticks "putintick7")) > (order_getprice order)) && (isOtype "Offer" order))])  = ((order_getprice order), [order]) : ticks
    | (fst (safehd ticks "putintick6")) /= (order_getprice order) = (safehd ticks "putintick5") : (putintick ticksize (tail ticks)) order 

fixtick ticksize order | ( mymod (order_getprice order) ticksize) == 0     = order                                              
                       | isOtype "Bid" order                              = order_setprice roundeddownprice order            
                       | isOtype "Offer" order                            = order_setprice (roundeddownprice + ticksize) order
                       | otherwise               = error ("fixtick - unrecognized ordertype " ++ (show (order_gettype order)))
                         where
                         roundeddownprice = ((order_getprice order) - ( mymod (order_getprice order) ticksize))

getticksize :: Order -> Double -> Double
getticksize o t   = if False 
                    then error "getticksize - Not yet implemented."
                    else 0.25    -- cmegroup.com E-mini FAQ

nice_mime_lob_checkorderallowed :: [Double] -> Order -> Bool
nice_mime_lob_checkorderallowed totlist ordr 
                                = if ((order_getsize ordr) <= ordermax) && ((abs total) <= maxordersallowed) 
                                  then True 
                                  else False
                                  where
                                  total | isBuySide ordr   = (totlist!!(order_gettraderidno ordr)) - (order_getsize ordr)
                                        | isSellSide ordr  = (totlist!!(order_gettraderidno ordr)) + (order_getsize ordr)

nice_mime_lob_getbestbid (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = if (and [((length bids) /= 0), (not $null(filter ((not).(null).snd) bids))]) 
                                                                                          then (fst (safehd (filter ((not).(null).snd) bids) "nice_mime_lob_getbestbid1"))
                                                                                          else 0
nice_mime_lob_getbestoffer (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = if (and [((length offers) /= 0), (not $null(filter ((not).(null).snd) offers))])
                                                                                            then (fst (safehd (filter ((not).(null).snd) offers) "nice_mime_lob_getbestoffer")) 
                                                                                            else 0
nice_mime_lob_getsizeofbids (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = foldr (+) 0 (map ((length).snd) bids)
nice_mime_lob_getsizeofoffers (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = foldr (+) 0 (map ((length).snd) offers)
nice_mime_lob_getbuysideliquidity (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = (foldr (+) 0 (foldr (++) [] (map ((map order_getsize).snd) bids))) --bids
nice_mime_lob_getsellsideliquidity (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = (foldr (+) 0 (foldr (++) [] (map ((map order_getsize).snd) offers))) --offers
nice_mime_lob_getbuysidelevels (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = length (filter ((not).(null).snd) bids)
nice_mime_lob_getsellsidelevels (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = length (filter ((not).(null).snd) offers)
nice_mime_lob_getbuysidedepthneartop (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
      = (foldr (+) 0 (map order_getsize (concat (map (snd) (filter (((>) (bb * (1 - percent))).fst) bids)))))
        where
        bb = (nice_mime_lob_getbestbid (Nice_mime_lob sent totlist bids offers time ticksize ltp sts))
nice_mime_lob_getbuysidedepth (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
      = (foldr (+) 0 (map order_getsize (concat (map (snd) bids))))
nice_mime_lob_getsellsidedepthneartop (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
      = (foldr (+) 0 (map order_getsize (concat (map (snd) (filter (((<) (bo * (1 + percent))).fst) offers)))))
        where
        bo = (nice_mime_lob_getbestoffer (Nice_mime_lob sent totlist bids offers time ticksize ltp sts))
nice_mime_lob_getsellsidedepth (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
      = (foldr (+) 0 (map order_getsize (concat (map (snd) offers))))

nice_mime_lob_clearemptyticks (Nice_mime_lob sent totlist bids offers time ticksize ltp sts)
      = (Nice_mime_lob sent totlist newbids newoffers time ticksize)
        where
        newbids = (filter ((not).(null).snd) bids)
        newoffers = (filter ((not).(null).snd) offers)

nice_mime_lob_getltp (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = ltp


--2.	(nice_mime_lob_prune) - The state passed in as the argument is pruned for expired entries and entries that are to be cancelled according to the list cancellations.
--     a.	It does this by going through both data structures holding the orders on the book entry by entry checking that they both haven¡¯t expired and are not present in the list of cancellations otherwise it removes them from the book.
--     b.	While doing this any time an order is removed from the data structure a cancellation message is created. The set of these is named (cmsgs)
nice_mime_lob_prune :: Nice_mime_lob -> [(Int,Int)] -> Int -> Time -> (Nice_mime_lob, [Msg_t])
nice_mime_lob_prune (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) cancellations myid mrt
       = ((Nice_mime_lob sent newtotlist newbids newoffers time ticksize ltp sts),cmsgs1 ++ cmsgs2)
         where
         (newbids, itotlist, cmsgs1) = pruneticks bids totlist
         (newoffers, newtotlist, cmsgs2) = pruneticks offers itotlist 
         pruneticks [] totlist = ([], totlist, [])
         pruneticks (tick:ticks) totlist = if (not $null(snd newtick)) 
                                           then ((newtick : newticks), newtots, cmsgs ++ rcmsgs)
                                           else (newticks, newtots, cmsgs ++ rcmsgs)
                                           where
                                           (newtick, newtots, cmsgs) = prunetick tick itots
                                           (newticks, itots, rcmsgs) = pruneticks ticks totlist
         prunetick (price, orders) totlist = ((price, neworders), newtotlist, cmsgs)
                                             where
                                             (neworders, newtotlist, cmsgs) = pruneorders orders totlist
         pruneorders [] totlist = ([], totlist, []) 
         pruneorders (order:rest) totlist| and [time < (order_getexpiry order), (order_getexpiry order) /= (-1)]     = ((order:newrest), itotlist, cmsgs)
                                         | or [and [((findindexoffirst cancellations ((==) ordersig)) /= (-1)), time >= (order_gettime order) + mrt], timedout (order_gettype order)] = (newrest, newtotlist, ((ackmessage (myid, (order_gettraderidno order)) 5 order "cancelled."):cmsgs))
                                         | otherwise = ((order:newrest), itotlist, cmsgs)
                                           where
                                           (newrest, itotlist, cmsgs) = pruneorders rest totlist
                                           newtotlist = nice_mime_lob_updatetotlist itotlist order (order_newsize order 0)
                                           ordersig = (order_gettraderidno order, order_getuid order)
                                           timedout (Bid (Goodtilldate expires)) = (expires <= time)
                                           timedout (Offer (Goodtilldate expires)) = (expires <= time)
                                           timedout any = False

nice_mime_lob_incrementtime (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) = (Nice_mime_lob sent totlist bids offers (time + 1) ticksize ltp sts)



--helper functions
--------------------------------

myor a b arg = or [(a arg), (b arg)]
myand f1 f2 arg = (f1 arg) && (f2 arg)

findindexoffirst :: [a] -> (a -> Bool) -> Int
findindexoffirst list condition = findex list condition 0
                                  where
                                  findex lst c x |(null lst)                            = -1
                                                 |(c (safehd lst "findindexoffirst1"))  = x
                                                 |otherwise                             = findex (tail lst) c (x+1)


nice_mime_lob_getleaderfrombids (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
                      | and [(not $null bids),(not $null(filter ((not).(null).snd) bids))]   = (safehd (snd (safehd (filter ((not).(null).snd) bids) "nice_mime_lob_getleaderfrombids3")) "nice_mime_lob_getleaderfrombids1")
                      | (null bids)   = error ("Nice_mime_lob_getleaderfrombids - empty tick list\n" ++ (show bids))
                      | otherwise     = error ("Nice_mime_lob_getleaderfrombids - empty tick\n" ++ (show bids))

nice_mime_lob_getleaderfromoffers (Nice_mime_lob sent totlist bids offers time ticksize ltp sts) 
                     | and [(not $null offers),(not $null(filter ((not).(null).snd) offers))]  = (safehd (snd (safehd (filter ((not).(null).snd) offers) "nice_mime_lob_getleaderfromoffers1"))) 
                     | null offers     = error ("Nice_mime_lob_getleaderfromoffers - empty tick list\n" ++ (show offers))
                     | otherwise           = error ("Nice_mime_lob_getleaderfromoffers - empty tick\n" ++ (show offers))

nice_mime_lob_getsummary :: Nice_mime_lob -> Int -> [Double]
nice_mime_lob_getsummary xlob or = [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or1, ltp]
                                   where
                                   bb = nice_mime_lob_getbestbid(xlob)
                                   bo = nice_mime_lob_getbestoffer(xlob)
                                   bsld = nice_mime_lob_getbuysideliquidity(xlob)
                                   ssld = nice_mime_lob_getsellsideliquidity(xlob)
                                   bsls = fromIntegral $nice_mime_lob_getbuysidelevels(xlob)
                                   ssls = fromIntegral $nice_mime_lob_getsellsidelevels(xlob)
                                   bsd = nice_mime_lob_getbuysidedepthneartop(xlob)
                                   ssd = nice_mime_lob_getsellsidedepthneartop(xlob)
                                   ltp = nice_mime_lob_getltp(xlob)
                                   or1 = fromIntegral or


--Updates the exchange statistics held internally to reflect the new changes using (nice_mime_lob_updatestats), these statistics will be later sent out as a broadcast to all agents.
nice_mime_lob_updatestats :: Nice_mime_lob -> Nice_mime_lob
nice_mime_lob_updatestats (Nice_mime_lob sent totlist bids offers ti ticksize ltp [tr, uv, squerr, cumsquerr, samples, opercent,
                                                          minlbs,maxlbs,lbs,maxddbs, 
                                                          minlss,maxlss,lss,maxddss,
                                                          minlbstop,maxlbstop,lbstop,maxddbstop, 
                                                          minlsstop,maxlsstop,lsstop,maxddsstop,
                                                          zerolbs, maxzerolbs, zerolss, maxzerolss,
                                                          zerolbstop, maxzerolbstop, zerolsstop, maxzerolsstop, stop, cumtrades, assetreturn, oltp]) 
    = (Nice_mime_lob sent totlist bids offers ti ticksize ltp newstats)
      where
      percent1 = if opercent == 0 
                 then 0.05
                 else opercent
      newsamples = samples + 1
      newuv = value sent (ti+1)
      newsquerr = (ltp - newuv)^2
      newcumsquerr = cumsquerr + newsquerr
      -- max drawdown buyside total liquidity :-
      newlbs = nice_mime_lob_getbuysideliquidity (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
      newmaxlbs = maximum [maxlbs, newlbs]
      ddbs = maxlbs - minimum [ newlbs, minlbs]
      newmaxddbs = maximum [maxddbs, ddbs]
      newminlbs = if (maxlbs == newmaxlbs)
                  then minimum [newlbs, minlbs]
                  else newmaxlbs
      -- max drawdown sellside total liquidity :-
      newlss = nice_mime_lob_getsellsideliquidity (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
      newmaxlss = maximum [maxlss, newlss]
      ddss = maxlss - minimum [ newlss, minlss]
      newmaxddss = maximum [maxddss, ddss]
      newminlss =if (maxlss == newmaxlss)  
                 then minimum [newlss, minlss] 
                 else newmaxlss
      -- max drawdown buyside top 5% of book :-
      newlbstop = nice_mime_lob_getbuysidedepthneartop (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
      newmaxlbstop = maximum [maxlbstop, newlbstop]
      ddbstop = maxlbstop - minimum [ newlbstop, minlbstop]
      newmaxddbstop = maximum [maxddbstop, ddbstop]
      newminlbstop = if (maxlbstop == newmaxlbstop) 
                     then minimum [newlbstop, minlbstop]
                     else newmaxlbstop
      -- max drawdown sellside top 5% of book :-
      newlsstop = nice_mime_lob_getsellsidedepthneartop (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
      newmaxlsstop = maximum [maxlsstop, newlsstop]
      ddsstop = maxlsstop - minimum [ newlsstop, minlsstop]
      newmaxddsstop = maximum [maxddsstop, ddsstop]
      newminlsstop = if (maxlsstop == newmaxlsstop) 
                     then minimum [newlsstop, minlsstop] 
                     else newmaxlsstop
      newzerolbs       = if newlbs == 0 
                         then zerolbs + 1 
                         else 0
      newmaxzerolbs    = if newlbs == 0 
                         then maxzerolbs 
                         else maximum[maxzerolbs, zerolbs]
      newzerolss       = if newlss == 0
                         then zerolss + 1 
                         else 0 
      newmaxzerolss    = if newlss == 0  
                         then maxzerolss 
                         else maximum[maxzerolss, zerolss]
      newzerolbstop    = if newlbstop == 0
                         then zerolbstop + 1 
                         else 0
      newmaxzerolbstop = if newlbstop == 0  
                         then maxzerolbstop 
                         else maximum [maxzerolbstop, zerolbstop]
      newzerolsstop    = if newlsstop == 0 
                         then zerolsstop + 1
                         else 0
      newmaxzerolsstop = if newlsstop == 0
                         then maxzerolsstop 
                         else maximum [maxzerolsstop, zerolsstop]
      newassetreturn   = if oltp /= 0
                         then (ltp / oltp) - 1
                         else -1
      newstats = [tr, newuv, newsquerr, newcumsquerr, newsamples, percent1,
                                                          newminlbs,newmaxlbs,newlbs,newmaxddbs, 
                                                          newminlss,newmaxlss,newlss,newmaxddss,
                                                          newminlbstop,newmaxlbstop,newlbstop,newmaxddbstop, 
                                                          newminlsstop,newmaxlsstop,newlsstop,newmaxddsstop,
                                                          newzerolbs, newmaxzerolbs, newzerolss, newmaxzerolss,
                                                          newzerolbstop, newmaxzerolbstop, newzerolsstop, newmaxzerolsstop, stop, cumtrades, newassetreturn, ltp
                 ]


nice_mime_lob_inccumtradevol newvol (Nice_mime_lob sent totlist bids offers ti ticksize ltp stats)
 = (Nice_mime_lob sent totlist bids offers ti ticksize ltp newstats)
   where
   newstats = (take 31 stats) ++ [((si "inccumtradevol" stats 31) + newvol)] ++ (drop 32 stats)

nice_mime_lob_gettrace :: Nice_mime_lob -> [Char]
nice_mime_lob_gettrace (Nice_mime_lob sent totlist bids offers ti ticksize ltp [tr, uv, squerr, cumsquerr, samples, opercent,
                                                          minlbs,maxlbs,lbs,maxddbs, 
                                                          minlss,maxlss,lss,maxddss,
                                                          minlbstop,maxlbstop,lbstop,maxddbstop, 
                                                          minlsstop,maxlsstop,lsstop,maxddsstop,
                                                          zerolbs, maxzerolbs, zerolss, maxzerolss,
                                                          zerolbstop, maxzerolbstop, zerolsstop, maxzerolsstop, stop, cumtrades, assetreturn, ltp1]) 
    = (show ti) ++ "," ++                                               -- A. Time 
      (show lbs) ++ "," ++                                              -- B. Buy Side Liquidity
      (show lss) ++ "," ++                                              -- C. Sell Side Liquidity
      (show midprice) ++ "," ++                                         -- D. Mid Price
      (show bestbid) ++ "," ++                                          -- E. Best Bid 
      (show bestoffer) ++ "," ++                                        -- F. Best Offer 
      (show (bestoffer - bestbid)) ++ "," ++                            --    Spread
      (show (ltp)) ++ "," ++                                            -- G. Last Traded Price
      (show tr)  ++ "," ++                                              -- H. Cumulative Trades
      (show bsls)  ++ "," ++                                            -- I. Buy Side Levels 
      (show ssls)  ++","++                                              -- J. Sell Side Levels
      (show lbs)  ++","++                                               -- K. Buy Side Depth
      (show lss)  ++","++                                               -- L. Sell Side Depth
      (show uv) ++ "," ++                                               -- M. Current value
      (show lbstop)  ++","++                                            -- N. Buy Side Depth Near Top
      (show lsstop)  ++","++                                            -- O. Sell Side Depth Near Top
      (show rms)  ++ "," ++                                             -- P. Cumulative Root Mean Squared error LTP-UV
      (show maxddbs)  ++ "," ++                                         -- Q. Maximum drawdown of buyside liquidity
      (show maxddss)  ++ "," ++                                         -- R. Maximum drawdown of sellside liquidity
      (show maxddbstop)  ++ "," ++                                      -- S. Maximum drawdown of buyside liquidity top 5% of book
      (show maxddsstop)  ++ "," ++                                      -- T. Maximum drawdown of sellside liquidity top 5% of book
      (show maxzerolbs)  ++ "," ++                                      -- U. Maximum zero buyside liquidity
      (show maxzerolss)  ++ "," ++                                      -- V. Maximum zero sellside liquidity
      (show maxzerolbstop)  ++ "," ++                                   -- W. Maximum zero buyside liquidity top 5% of book
      (show maxzerolsstop)  ++  "," ++                                  -- X. Maximum zero sellside liquidity top 5% of book
      (show stop) ++ "," ++                                             -- Y. Timesteps left to stop trading for.
      (show cumtrades) ++ "," ++                                        -- Z. Cumulative trade volume.
      (show assetreturn) ++ ","                                         -- AA. Asset return
--       ++ "\n"
        where
        midprice = (nice_mime_lob_getmidprice (Nice_mime_lob sent totlist bids offers ti ticksize ltp []))
        bestbid = nice_mime_lob_getbestbid (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
        bestoffer = nice_mime_lob_getbestoffer (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
        bsls = nice_mime_lob_getbuysidelevels (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
        ssls = nice_mime_lob_getsellsidelevels (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
        lbs = nice_mime_lob_getbuysidedepth (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
        lss = nice_mime_lob_getsellsidedepth (Nice_mime_lob sent totlist bids offers ti ticksize ltp [])
        msquerr = if (samples > 0) 
                  then cumsquerr / samples 
                  else cumsquerr
        rms     = sqrt msquerr

nice_mime_lob_gettraceheaders = "Exchange Time, Buy Side Liquidity, Sell Side Liquidity, Mid Price, Best Bid, Best Offer, Spread, Last Traded Price, Cumulative Trades, Buy Side Levels, Sell Side Levels, Buy Side Depth, Sell Side Depth, Current Value, Buy side depth near top, Sell side depth near top, cumulative root mean square error, max drawdown of bsl, max drawdown of ssl, max drawdown of bsl top, max drawdown of ssl top, max zero bsl, max zero ssl, max zero bsl top, max zero ssl top, stop logic countdown, Cumulative Trade Volume, Asset Return,"

nice_mime_lob_getmidprice :: Nice_mime_lob -> Double
nice_mime_lob_getmidprice lob
 = (bidprice + offerprice) / 2
    where
    bidprice = nice_mime_lob_getbestbid lob
    offerprice = nice_mime_lob_getbestoffer lob


        

--Test without agent, just use a probe order.
--             -Incoming bid uncrossed no buyside liquidity
--             -Incoming bid uncrossed existing buyside liquidity
--             -Incoming bid crossed matches one offer. 
--             -Incoming bid crossed matches 30% of one offer. E.g. bid 100 from offer 300
--             -Incoming bid crossed matches four offers.
--             -Ditto for offers.
--             -incoming buy no sellside liquidity. 
--             -incoming buy matches one offer.
--             -incoming buy matches four offers. 
--             -incoming buy matches 30% of one offer. E.g. buy 100 from offer 300
--             -Ditto for sells.
--             -Add in tests that trigger constraints. E.g. bid price outside price band limit or net traders exceeds 100,000 etc.

--Below here are all of the test suite functions.
nm_dts = messages
         where
         messages = foldr success [] [test1, test2, test3, test4, test5, test6,otest1, otest2, otest3, otest4, otest5, otest6, btest1, btest2, btest3, btest4, btest5, stest1, stest2, stest3, stest4, stest5, edge1, edge2, edge3, edge4, edge5, edge6, edge7, edge8, edge9, edge10, canceltest]
         test1 = (nice_mime_lob_getsizeofbids (lobwith nmel 1 (Bid Goodtillcancelled))) == 1 --Incoming bid uncrossed no buyside liquidity
         test2 = (nice_mime_lob_getsizeofbids (lobwith (lobwith nmel 1 (Bid Goodtillcancelled)) 1 (Bid Goodtillcancelled))) == 2 --Incoming bid uncrossed existing buyside liquidity
         test3 = (length (filter msg_istrade (msgsfrm (lobwith nmel 1 (Offer Goodtillcancelled)) 1 (Bid Goodtillcancelled)))) == 2 --Incoming bid crossed matches one offer. 
         test4 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 1 (Offer Goodtillcancelled) 300) 1 (Bid Goodtillcancelled)))) == 2 --Incoming bid crossed matches 30% of one offer. p1: trade happens
         test5 = (nice_mime_lob_getsizeofoffers (lobwith (lobwithsz nmel 1 (Offer Goodtillcancelled) 300) 1 (Bid Goodtillcancelled))) == 1 --Incoming bid crossed matches 30% of one offer. p2: offer stays on book
         test6 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 4 (Offer Goodtillcancelled) 25) 1 (Bid Goodtillcancelled)))) == 8 --Incoming bid crossed matches four offers.
         otest1 = (nice_mime_lob_getsizeofoffers (lobwith nmel 1 (Offer Goodtillcancelled))) == 1 --Incoming (Offer Goodtillcancelled) uncrossed no buyside liquidity
         otest2 = (nice_mime_lob_getsizeofoffers (lobwith (lobwith nmel 1 (Offer Goodtillcancelled)) 1 (Offer Goodtillcancelled))) == 2 --Incoming (Offer Goodtillcancelled) uncrossed existing buyside liquidity
         otest3 = (length (filter msg_istrade (msgsfrm (lobwith nmel 1 (Bid Goodtillcancelled)) 1 (Offer Goodtillcancelled)))) == 2 --Incoming (Offer Goodtillcancelled) crossed matches one (Bid Goodtillcancelled). 
         otest4 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 1 (Bid Goodtillcancelled) 300) 1 (Offer Goodtillcancelled)))) == 2 --Incoming (Offer Goodtillcancelled) crossed matches 30% of one (Bid Goodtillcancelled). p1: trade happens
         otest5 = (nice_mime_lob_getsizeofbids (lobwith (lobwithsz nmel 1 (Bid Goodtillcancelled) 300) 1 (Offer Goodtillcancelled))) == 1 --Incoming (Offer Goodtillcancelled) crossed matches 30% of one (Bid Goodtillcancelled). p2: (Bid Goodtillcancelled) stays on book
         otest6 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 4 (Bid Goodtillcancelled) 25) 1 (Offer Goodtillcancelled)))) == 8 --Incoming (Offer Goodtillcancelled) crossed matches four Bids.
         btest1 = myand (((==) 1).(length)) (msg_isreject.head) (filter msg_isack (msgsfrm nmel 1 (Buy Andkill))) --Incoming buy no sellside liquidity.
         btest2 = (length (filter msg_istrade (msgsfrm (lobwith nmel 1 (Offer Goodtillcancelled)) 1 (Buy Andkill)))) == 2 --Incoming buy matches one offer
         btest3 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 4 (Offer Goodtillcancelled) 25) 1 (Buy Andkill)))) == 8 --Incoming buy matches four offers.
         btest4 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 1 (Offer Goodtillcancelled) 300) 1 (Buy Andkill)))) == 2 --Incoming buy crossed matches 30% of one offer. p1: trade happens
         btest5 = (nice_mime_lob_getsizeofoffers (lobwith (lobwithsz nmel 1 (Offer Goodtillcancelled) 300) 1 (Buy Andkill))) == 1 --Incoming buy crossed matches 30% of one offer. p2: offer stays on book
         stest1 = myand (((==) 1).(length)) (msg_isreject.head) (filter msg_isack (msgsfrm nmel 1 (Sell Andkill))) --Incoming (Sell Andkill) no Buyside liquidity.
         stest2 = (length (filter msg_istrade (msgsfrm (lobwith nmel 1 (Bid Goodtillcancelled)) 1 (Sell Andkill)))) == 2 --Incoming (Sell Andkill) matches one bid
         stest3 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 4 (Bid Goodtillcancelled) 25) 1 (Sell Andkill)))) == 8 --Incoming (Sell Andkill) matches four bids.
         stest4 = (length (filter msg_istrade (msgsfrm (lobwithsz nmel 1 (Bid Goodtillcancelled) 300) 1 (Sell Andkill)))) == 2 --Incoming (Sell Andkill) crossed matches 30% of one bid. p1: trade happens
         stest5 = (nice_mime_lob_getsizeofbids (lobwith (lobwithsz nmel 1 (Bid Goodtillcancelled) 300) 1 (Sell Andkill))) == 1 --Incoming (Sell Andkill) crossed matches 30% of one bid. p2: bid stays on book
         edge1 =  (msg_isreject.head) (filter msg_isack (snd(nice_mime nmel [(order_create 1785 25 0 (FundSeller 2) (Offer Goodtillcancelled))] 1 [] 0)))--(Offer Goodtillcancelled) too far from last traded price.
         edge2 =  (msg_isreject.head) (filter msg_isack (snd(nice_mime nmel [(order_create 1615 25 0 (FundSeller 2) (Bid Goodtillcancelled))] 1 [] 0)))--(Bid Goodtillcancelled) too far from last traded price.
         edge3 =  ((not).msg_isreject.head) (filter msg_isack (snd(nice_mime nmel [(order_create 1711 25 0 (FundSeller 2) (Offer Goodtillcancelled))] 1 [] 0)))--(Offer Goodtillcancelled) just within ltp leeway.
         edge4 =  ((not).msg_isreject.head) (filter msg_isack (snd(nice_mime nmel [(order_create 1689 25 0 (FundSeller 2) (Bid Goodtillcancelled))] 1 [] 0)))--(Bid Goodtillcancelled) just within ltp leeway.
         edge5 =  ((not).msg_isreject.head) (filter msg_isack (snd(nice_mime nmel [(order_create 1700 2000 0 (FundSeller 2) (Bid Goodtillcancelled))] 1 [] 0)))--(Bid Goodtillcancelled) is at order limit.
         edge6 =  (msg_isreject.head) (filter msg_isack (snd(nice_mime nmel [(order_create 1616 2001 0 (FundSeller 2) (Bid Goodtillcancelled))] 1 [] 0)))--(Bid Goodtillcancelled) is over order limit.
         edge7 =  ((not).msg_isreject.head) (filter msg_isack (snd(nice_mime (lobwithsz nmel 2 (Offer Goodtillcancelled) 2000) [(order_create 1616 2000 0 (FundSeller 2) (Buy Andkill))] 1 [] 0)))--(Buy Andkill) is at order limit.
         edge8 =  (msg_isreject.head) (filter msg_isack (snd(nice_mime (lobwithsz nmel 2 (Offer Goodtillcancelled) 2000) [(order_create 1616 2001 0 (FundSeller 2) (Buy Andkill))] 1 [] 0)))--(Buy Andkill) is over order limit.
         edge9 = ((not).msg_isreject.head) (filter msg_isack (snd(nice_mime (lobwithsz nmel 49 (Offer Goodtillcancelled) 2000) [(order_create 1616 2000 0 (FundSeller 2) (Offer Goodtillcancelled))] 1 [] 0))) -- (Buy Andkill) is at total order limit.
         edge10 = (msg_isreject.head) (filter msg_isack (snd(nice_mime (lobwithsz nmel 50 (Offer Goodtillcancelled) 2000) [(order_create 1616 2000 0 (FundSeller 2) (Offer Goodtillcancelled))] 1 [] 0))) -- (Buy Andkill) exceeds total order limit.
         canceltest = (nice_mime_lob_getsizeofbids (fst (nice_mime (lobwith (lobwith nmel 1 (Bid Goodtillcancelled)) 1 (Bid Goodtillcancelled)) [] 1 [(2, -1)] 0))) == 0 --Incoming bid uncrossed existing buyside liquidity
--         teststopspike = (lobwithszandinc nmel 51 12 (Offer Goodtillcancelled) (Buy Andkill) 2)





success :: Bool -> [Char] -> [Char]
success n o = "Test result: " ++ nt ++ "\n" ++ o
              where
              nt = if n == True 
                   then "Success."
                   else "Failure."


lobwithsz lob n t sz = (fst (nice_mime lob (rep n (order_create 2000 sz 0 (FundSeller 2) t)) 1 [] 0))
lobwith lob n t = (fst (nice_mime lob (rep n (order_create 2000 100 0 (FundSeller 2) t)) 1 [] 0))
msgsfrm lob n t = (snd (nice_mime lob (rep n (order_create 2000 100 0 (FundSeller 2) t)) 1 [] 0))
nmel = nice_mime_emptylob





replace :: Int -> [a] ->a -> [a]
replace n list object = (take n list) ++ [object] ++ (drop (n+1) list)

getrestingprice :: Order -> Order -> Double
getrestingprice x y| or [((order_getfractime y) == (-1)),((order_gettime x) < (order_gettime y)), (((order_gettime x) == (order_gettime y)) && ((order_getfractime x) < (order_getfractime y)) && ((order_getfractime x) /= (-1)))]  = order_getprice x
                   | or [((order_getfractime x) == (-1)),((order_gettime y) < (order_gettime x)), (((order_gettime y) == (order_gettime x)) && ((order_getfractime y) < (order_getfractime x)) && ((order_getfractime x) /= (-1)))]  = order_getprice y

rekciuq or lob = quickerl lob or
quicker lob or = nice_mime (fst lob) [or] 1 [] 0
quickerl lob or = nice_mime (fst lob) or 1 [] 0
quickerc lob c = nice_mime (fst lob) [] 1 [c] 0
shownm x = shownice_mime_lob (fst x)
showmsgs x = concat (map showmsg_t (snd x))
firstlob = quicker (nmel, []) (order_setuid "nicemime tests" 1 (order_create 1712 1 0 (FundSeller 2) (Offer Goodtillcancelled)))
secondlob = quicker firstlob (order_create 1712 1 0 (FundSeller 2) (Buy Andkill))
thirdlob = quicker secondlob (order_create 1712 1 0 (FundSeller 2) (Buy Andkill))
fourthlob = quicker thirdlob (order_create 1716 1 0 (FundSeller 2) (Offer Goodtillcancelled))
fifthlob = quicker fourthlob (order_create 1716 1 0 (FundSeller 2) (Buy Andkill))
sixthlob = quickerl fifthlob [(order_create 1716 1 0 (FundSeller 2) (Offer Goodtillcancelled)),(order_create 1716 1 0 (FundSeller 2) (Buy Andkill))]
seventhlob = quicker sixthlob (order_create 1704 1 0 (FundSeller 2) (Bid Goodtillcancelled))
iterateylob n = thelob
                where
                thelob = giterate seventhlob (torders n)
                giterate mylob [] = mylob
                giterate mylob orders = giterate (quickerl mylob (head orders)) (tail orders) 
thespiketriggeringlob = quicker (iterateylob 60) (order_create 1716 1 0 (FundSeller 2) (Sell Andkill))
ninthlob = quickerl thespiketriggeringlob [(order_create 2436 1 0 (FundSeller 2) (Offer Goodtillcancelled)), (order_create 2436 1 0 (FundSeller 2) (Bid Goodtillcancelled)), (order_create 1712 1 0 (FundSeller 2) (Buy Andkill))]
alittlewhilelater = (while steps ninthlob (rekciuq []))
                    where
                    steps = ((secondstosteps 4) - 2)
                    while n a f = if n > 0 
                                  then while (n - 1) (f a) f
                                  else f a
onemoreforluck = quickerl alittlewhilelater []
cancellob = quickerc firstlob (2,1)

pricetodoubleorder price = [(order_create price 1 0 (FundSeller 2) (Offer Goodtillcancelled)),(order_create price 1 0 (FundSeller 2) (Buy Andkill))]
torders n = (zipfunc (rep n pricetodoubleorder) [1728, 1740..] )     
                                       
--nicemime end                                       
----------------------------------------------------------------------------------------