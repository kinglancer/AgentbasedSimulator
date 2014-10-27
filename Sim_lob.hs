module Sim_lob where               -- need to put more consideration on the structure

import Sim_orderlist
import Order
import Comm

--LIMIT ORDER BOOK
--
--For this deliberately simplified implementation only four order types are supported - (Buy Andkill), (Sell Andkill), (Bid Goodtillcancelled), (Offer Goodtillcancelled) - and we assume that any 
--submitted order stays on the book until filled.  Note that this means that there are no cancellations of orders, and that when an 
--order is submitted the trader does not specify when the order will expire.
--
--What happens if a market order ((Buy Andkill) or (Sell Andkill)) arrives yet there is no liquidity available on the book to fill that order?  
--There are many ways to solve this - e.g. reject the order, place the market order on the book as a limit order, or queue 
--the market order so that it receives priorty whenever liquidity returns to the book.  In this simplified version of an 
--order book we choose to queue any unfilled market orders.  




-- Implementation of lob




instance C_Lob Lob  where 
   emptylob = LOB emptyorderlist emptyorderlist [] [] [] 0 [] Calm [] (2000,0,0,0,0,0.05,(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0),(0,0,0,0))  
   primed_emptylob = match (order_create 2050 500 0 Phantom (Offer Goodtillcancelled)) (match (order_create 1950 500 0 Phantom (Bid Goodtillcancelled)) emptylob) --Intermediaries sell 10k and kill everything.
   testlob x | x==1 = LOB onebidlow oneofferhigh [] [] [] 0 [] Calm [] emptystats                       -- one bid, one offer, uncrossed
             | x==2 = LOB onebidhigh oneofferlow [] [] [] 0 [] Calm [] emptystats                       -- one bid, one offer, crossed
             | x==3 = LOB onebidhigh threeofferslowbig [] [] [] 0 [] Calm [] emptystats                 -- one bid, three offers, crossed, one trade
             | x==4 = LOB onebidhigh threeofferslowsmall [] [] [] 0 [] Calm [] emptystats               -- one bid, three offers, crossed, three trades
             | x==5 = LOB threebidshighbig oneofferlow [] [] [] 0 [] Calm [] emptystats                 -- three bids, one offer, crossed, one trade
             | x==6 = LOB threebidshighsmall oneofferlow [] [] [] 0 [] Calm [] emptystats               -- three bids, one offer, crossed, three trades
             | x==7 = LOB threebidshighsmall oneofferlow [bw] [] [] 0 [] Calm [] emptystats             -- three bids, one offer, crossed, three trades, one buy waiting
             | x==8 = LOB threebidshighsmall oneofferlow [] [sw] [] 0 [] Calm [] emptystats             -- three bids, one offer, crossed, three trades, one sell waiting
             | x==9 = LOB threebidshighsmall oneofferlow [bw] [sw] [] 0 [] Calm [] emptystats           -- three bids, one offer, crossed, three trades, one buy & one sell waiting
             | x==10 = LOB onebidlow emptyorderlist [] [] [] 0 [] Choppy [] emptystats
             | otherwise = emptylob
               where
               emptystats = (0,0,0,0,0,0, (0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0))
               onebidlow = ol_insert 1950 emptyorderlist (order_create 1950 500 0 Phantom (Bid Goodtillcancelled))
               onebidhigh = ol_insert 2050 emptyorderlist (order_create 2050 500 0 Phantom (Bid Goodtillcancelled))
               oneofferhigh = ol_insert 2050 emptyorderlist (order_create 2050 500 0 Phantom (Offer Goodtillcancelled))
               oneofferlow = ol_insert 1950 emptyorderlist (order_create 1950 500 0 Phantom (Offer Goodtillcancelled))
               threebidshighsmall = foldr (gensmallorders (Bid Goodtillcancelled)) emptyorderlist [2050,2050,2050]
               threebidshighbig = foldr (genbigorders (Bid Goodtillcancelled)) emptyorderlist [2050, 2050, 2050]
               threeofferslowsmall = foldr (gensmallorders (Offer Goodtillcancelled)) emptyorderlist [1950, 1950, 1950]
               threeofferslowbig = foldr (genbigorders (Offer Goodtillcancelled)) emptyorderlist [1950, 1950, 1950]
               gensmallorders x a b = ol_insert a b (order_create a 100 0 Phantom x)
               genbigorders x a b = ol_insert a b (order_create a 500 0 Phantom x)
               bw = order_create 5000 100 0 Phantom (Buy Andkill)
               sw = order_create 5000 100 0 Phantom (Sell Andkill)
   
   
   lob_getstats (LOB b o bw sw tr ti or sen tinfo stats)
       = stats
   
   lob_increment_time (LOB b o bw sw tr ti or sen tinfo stats)
       = LOB b o bw sw tr (ti+1) or sen tinfo stats 
   
   lob_clear_trades (LOB b o bw sw tr ti or sen tinfo stats)
       = LOB b o bw sw [] ti or sen tinfo stats 
   
   
   lob_setpercent x (LOB b o bw sw tr ti or sen tinfo (uv, ltp, squerr, cumsquerr, samples, percent, a1, a2, a3, a4, a5, a6))
       = (LOB b o bw sw tr ti or sen tinfo (uv, ltp, squerr, cumsquerr, samples, x, a1, a2, a3, a4, a5, a6))
   
   lob_getpercent (LOB b o bw sw tr ti or sen tinfo (uv, ltp, squerr, cumsquerr, samples, percent, a1, a2, a3, a4, a5, a6))
       = percent
   
   lob_updatestats (LOB b o bw sw tr ti or sen tinfo (uv, ltp, squerr, cumsquerr, samples, percent,
                                                             (minlbs,maxlbs,lbs,maxddbs), 
                                                             (minlss,maxlss,lss,maxddss),
                                                             (minlbstop,maxlbstop,lbstop,maxddbstop), 
                                                             (minlsstop,maxlsstop,lsstop,maxddsstop),
                                                             (zerolbs, maxzerolbs, zerolss, maxzerolss),
                                                             (zerolbstop, maxzerolbstop, zerolsstop, maxzerolsstop)
                                                        )) 
       = LOB b o bw sw tr ti or sen tinfo newstats 
         where
         newsamples = samples + 1
         newuv = value sen (ti+1)
         newltp = if (null tr)
                  then ltp 
                  else order_getprice (fst (head tr))
         newsquerr = (newltp - newuv)^2
         newcumsquerr = cumsquerr + newsquerr
         -- max drawdown buyside total liquidity :-
         newlbs = (ol_getliquidity b) 
         newmaxlbs = maximum [maxlbs, newlbs]
         ddbs = maxlbs - minimum [ newlbs, minlbs]
         newmaxddbs = maximum [maxddbs, ddbs]
         newminlbs = if (maxlbs == newmaxlbs)
                     then minimum [newlbs, minlbs] 
                     else newmaxlbs
         -- max drawdown sellside total liquidity :-
         newlss = (ol_getliquidity o) 
         newmaxlss = maximum [maxlss, newlss]
         ddss = maxlss - minimum [ newlss, minlss]
         newmaxddss = maximum [maxddss, ddss]
         newminlss = if (maxlss == newmaxlss)
                     then minimum [newlss, minlss] 
                     else newmaxlss
         -- max drawdown buyside top 5% of book :-
         newlbstop = (ol_getdepthneartop (Bid Goodtillcancelled) percent b) 
         newmaxlbstop = maximum [maxlbstop, newlbstop]
         ddbstop = maxlbstop - minimum [ newlbstop, minlbstop]
         newmaxddbstop = maximum [maxddbstop, ddbstop]
         newminlbstop = if (maxlbstop == newmaxlbstop)
                        then minimum [newlbstop, minlbstop]
                        else newmaxlbstop
         -- max drawdown sellside top 5% of book :-
         newlsstop = (ol_getdepthneartop (Offer Goodtillcancelled) percent o) 
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
                            else maximum [maxzerolbs, zerolbs]
         newzerolss       = if newlss == 0
                            then zerolss + 1
                            else 0
         newmaxzerolss    = if newlss == 0
                            then maxzerolss
                            else maximum [maxzerolss, zerolss]
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
         newstats = (newuv, newltp, newsquerr, newcumsquerr, newsamples, percent,
                                                             (newminlbs,newmaxlbs,newlbs,newmaxddbs), 
                                                             (newminlss,newmaxlss,newlss,newmaxddss),
                                                             (newminlbstop,newmaxlbstop,newlbstop,newmaxddbstop), 
                                                             (newminlsstop,newmaxlsstop,newlsstop,newmaxddsstop),
                                                             (newzerolbs, newmaxzerolbs, newzerolss, newmaxzerolss),
                                                             (newzerolbstop, newmaxzerolbstop, newzerolsstop, newmaxzerolsstop)
                    )
   
   lob_gettrace ob
       = (show (lob_gettime ob)) ++ "," ++                                 ---A. Time 
         (show (lob_getbuysideliquidity ob)) ++ "," ++                     ---B. Buy Side Liquidity
         (show (lob_getsellsideliquidity ob)) ++ "," ++                    ---C. Sell Side Liquidity
         (show (lob_getmidprice ob)) ++ "," ++                             ---D. Mid Price
         (show (lob_getbestbid ob)) ++ "," ++                              ---E. Best Bid 
         (show (lob_getbestoffer ob)) ++ "," ++                            ---F. Best Offer  
         (show (ltp)) ++ "," ++                                            ---G. Last Traded Price
         (show (length (lob_gettrades ob)))  ++ "," ++                     ---H. Cumulative Trades
         (show (lob_getbuysidelevels ob))  ++ "," ++                       ---I. Buy Side Levels 
         (show (lob_getsellsidelevels ob))  ++","++                        ---J. Sell Side Levels
         (show (lob_getbuysidedepth ob))  ++","++                          ---K. Buy Side Depth
         (show (lob_getsellsidedepth ob))  ++","++                         ---L. Sell Side Depth
         (show uv) ++ "," ++                                               ---M. Current value
         (show (lob_getbuysidedepthneartop ob))  ++","++                   ---N. Buy Side Depth Near Top
         (show (lob_getsellsidedepthneartop ob))  ++","++                  ---O. Sell Side Depth Near Top
         (show rms)  ++ "," ++                                             ---P. Cumulative Root Mean Squared error LTP-UV
         (show maxdrawdownbs)  ++ "," ++                                   ---Q. Maximum drawdown of buyside liquidity
         (show maxdrawdownss)  ++ "," ++                                   ---R. Maximum drawdown of sellside liquidity
         (show maxdrawdownbstop)  ++ "," ++                                ---S. Maximum drawdown of buyside liquidity top 5% of book
         (show maxdrawdownsstop)  ++ "," ++                                ---T. Maximum drawdown of sellside liquidity top 5% of book
         (show maxzerolbs)  ++ "," ++                                      ---U. Maximum zero buyside liquidity
         (show maxzerolss)  ++ "," ++                                      ---V. Maximum zero sellside liquidity
         (show maxzerolbstop)  ++ "," ++                                   ---W. Maximum zero buyside liquidity top 5% of book
         (show maxzerolsstop)  ++                                          ---X. Maximum zero sellside liquidity top 5% of book
          "\n"
         where
         (uv, ltp, squerr, cumsquerr, samples, percent, bsdrawdown, ssdrawdown, bsdrawdowntop, ssdrawdowntop, zeros, zerostop) = lob_getstats ob
         (b1, b2, b3, maxdrawdownbs) = bsdrawdown
         (s1, s2, s3, maxdrawdownss) = ssdrawdown
         (b1top, b2top, b3top, maxdrawdownbstop) = bsdrawdowntop
         (s1top, s2top, s3top, maxdrawdownsstop) = ssdrawdowntop
         (z1, maxzerolbs, z3, maxzerolss) = zeros
         (z1top, maxzerolbstop, z3top, maxzerolsstop) = zeros
         msquerr = if (samples > 0)
                   then cumsquerr / samples
                   else cumsquerr
         rms     = sqrt msquerr
   
   lob_gettime (LOB b o bw sw tr ti or sen tinfo stats) = ti
   lob_getbestbid (LOB b o bw sw tr ti or sen tinfo stats) 
           = bestbid
             where
             bestbid = fst (ol_last b)
   
   lob_getbestoffer (LOB b o bw sw tr ti or sen tinfo stats) 
           = bestoffer
             where
             bestoffer = fst (ol_first o)
   
   lob_getsentiment (LOB b o bw sw tr ti or sen tinfo stats) = sen
   lob_setsentiment sent (LOB b o bw sw tr ti or sen tinfo stats) = LOB b o bw sw tr ti or sent tinfo stats
   
   lob_getbuysideliquidity (LOB b o bw sw tr ti or sen tinfo stats)
       = (ol_getliquidity b) 
   
   lob_getsellsideliquidity (LOB b o bw sw tr ti or sen tinfo stats)
       = (ol_getliquidity o)
   
   lob_getbuysidelevels (LOB b o bw sw tr ti or sent tinfo stats)
       = ol_getlevels b
   
   lob_getbuysidedepth (LOB b o bw sw tr ti or sent tinfo stats)
       = ol_getdepth (Bid Goodtillcancelled) b
   
   lob_getbuysidedepthneartop (LOB b o bw sw tr ti or sent tinfo (a1,a2,a3,a4,a5,percent,b1,b2,b3,b4,b5,b6))
       = ol_getdepthneartop (Bid Goodtillcancelled) percent b
   
   lob_getsellsidelevels (LOB b o bw sw tr ti or sent tinfo stats)
       = ol_getlevels o
   
   lob_getsellsidedepth (LOB b o bw sw tr ti or sent tinfo stats)
       = ol_getdepth (Offer Goodtillcancelled) o
   
   lob_getsellsidedepthneartop (LOB b o bw sw tr ti or sent tinfo (a1,a2,a3,a4,a5,percent,b1,b2,b3,b4,b5,b6)) 
       = ol_getdepthneartop (Offer Goodtillcancelled) percent o
   
   lob_getmidprice (LOB b o bw sw tr ti or sen tinfo stats)
       = ((fst (ol_last b)) + (fst (ol_first o)) ) / 2 -- (x + y)/2
   --      where
   --      x = 0, if (isemptyorderlist b)                        || A stub bid
   --        = (ol_gethighestprice b), otherwise
   --      y = 0, if (isemptyorderlist o)                        || A strange stub offer 
   --        = (ol_getlowestprice o), otherwise
   
   lob_getlasttradedprice (LOB b o bw sw tr ti or sen tinfo stats)
       = if (null tr)
         then 0 
         else order_getprice (fst (head tr))
   
   lob_gettraderinfo tid (LOB b o bw sw tr ti or sen tinfo stats)
       = f tinfo
         where
         f [] = emptytraderinfo_item
         f ((t,a,b,c):xs) = if (t==tid) 
                            then (t,a,b,c)
                            else f xs
   
   lob_settraderinfo (t1,a1,b1,c1) (LOB b o bw sw tr ti or sen tinfo stats)            --continue 5/7/2014
       = LOB b o bw sw tr ti or sen newtinfo stats
         where
         newtinfo = f (t1,a1,b1,c1) tinfo
         f (t1,a1,b1,c1) []                 = [(t1,a1,b1,c1)]
         f (t1,a1,b1,c1) ((t2,a2,b2,c2):xs) | t1==t2 = ((t1,a1,b1,c1):xs)
                                            | otherwise = (t2,a2,b2,c2):(f (t1,a1,b1,c1) xs)

   
   
   lob_getordernum (LOB b o bw sw tr ti or sen tinfo stats)
       = length or
   
   newbid p x (LOB b o bw sw tr ti or sen tinfo stats) 
           = execute_trades newlob
             where
             newlob = LOB (ol_insert p b x) o bw sw tr ti or sen tinfo stats
   
   newoffer p x (LOB b o bw sw tr ti or sen tinfo stats) 
           = execute_trades newlob
             where
             newlob = LOB b (ol_insert p o x) bw sw tr ti or sen tinfo stats
   
   neworder x (LOB b o bw sw tr ti or sen tinfo stats)
          | null or              = (LOB b o bw sw tr ti [x] sen tinfo stats)    --We're clearing out the order list because we don't want to see the whole thing each iteration.
          |(order_gettime (head or)) == (order_gettime x)         = (LOB b o bw sw tr ti (x:or) sen tinfo stats)
          | otherwise                                             = (LOB b o bw sw tr ti [x] sen tinfo stats)
   
   newtrade p x (LOB b o bw sw tr ti or sen tinfo stats) 
           = execute_trades newlob
             where
             newlob = if (order_gettype x == (Buy Andkill))
                      then LOB b o (bw++[x]) sw tr ti or sen tinfo stats 
                      else LOB b o bw (sw++[x]) tr ti or sen tinfo stats
   
   lob_gettrades (LOB b o bw sw tr ti or sen tinfo stats) = tr
   
    -- Matching order = 1. buys - 2. sells - 3. crossed book
   execute_trades (LOB bb oo [] [] tr ti or sen tinfo stats) 
           = uncross_book (LOB bb oo [] [] tr ti or sen tinfo stats) -- This is being matched every time...
   execute_trades (LOB bb oo [] sw tr ti or sen tinfo stats) 
           = uncross_book (LOB newbb oo [] newsw newtr ti or sen tinfo stats)
             where
             (newbb, newsw, newtr) = if (length or) == 1  
                                     then execute_sells (bb, sw, [], []) 
                                     else execute_sells (bb, sw, tr, [])
   execute_trades (LOB bb oo bw [] tr ti or sen tinfo stats) 
           = uncross_book (LOB bb newoo newbw [] newtr ti or sen tinfo stats)
             where
             (newoo, newbw, newtr) = if (length or) == 1
                                     then execute_buys (oo, bw, [], [])
                                     else execute_buys (oo, bw, tr, [])
   execute_trades (LOB bb oo bw sw tr ti or sen tinfo stats) 
           = uncross_book (LOB bb2 oo2 bw2 sw2 tr2 ti or sen tinfo stats)
             where
             (bb2, sw2, tr2) = execute_sells (bb, sw, tr1, []) 
             (oo2, bw2, tr1) = if (length or) == 1 
                               then execute_buys (oo, bw, [], [])
                               else execute_buys (oo, bw, tr, [])
   
   

   
   
   is_crossed_book (LOB bb oo bw sw tr ti or sen tinfo stats)
       |(isemptyorderlist bb)                         = False
       |(isemptyorderlist oo)                         = False
       |(highestbidprice >= lowestsellprice)          = True
       | otherwise                                    = False
         where
         highestbidprice = ol_gethighestprice bb
         lowestsellprice = ol_getlowestprice oo
   
    ---to uncross a crossed book, we pop the lowest offer and turn it into a market order for execution
    ---- anything left after execution goes back as an offer, otherwise pop next offer and repeat
   uncross_book (LOB bb oo bw sw tr ti or sen tinfo stats)
       = if  not (is_crossed_book (LOB bb oo bw sw tr ti or sen tinfo stats)) 
         then (LOB bb oo bw sw tr ti or sen tinfo stats)
         else uncross_book (LOB newbb newoo bw sw newtr ti or sen tinfo stats)
         where
         (lowest_offer, remaining_offers) = ol_poplowest oo                                              -- pop the lowest offer
         (bb1, s1, tr1) = ol_fill_cross bb lowest_offer (Sell Andkill) []                                          --try to execute against bids
         newoo = if not(isemptyorder s1) 
                 then ol_clean (ol_insert (order_getprice s1) remaining_offers s1)          --anything remaining goes onto newoo
                 else ol_clean remaining_offers
         newbb = ol_clean bb1                                                                   -- what's left after trying to match lowest offer against bids
         newtr = tr1 ++ tr                                                                      -- the old tr plus any executions
   
   
   -- The matching engine takes in an order and a start book and returns a resulting book. --= error (show (lob_gettrades ob)), if (# (lob_gettrades ob)) > 0 ||Can use this to generate appropriate CRASH.     
    
   match x ob | (ty==(Bid Goodtillcancelled))                     = lob_updatestats (newbid p x (neworder x ob)) 
              | (ty==(Offer Goodtillcancelled))                   = lob_updatestats (newoffer p x (neworder x ob))
              | or [(ty==(Sell Andkill)), (ty==(Buy Andkill))]    = lob_updatestats (newtrade p x (neworder x ob))
              | (ty==None)                                        = lob_updatestats ob 
                where
                ty = (order_gettype x)
                p  = (order_getprice x)
   
   
   --The match_m1 matching engine takes in an order and a start book and returns a resulting book.
   --Includes attenuation of takers.
   --
   match_m1 x ob| ty==(Bid Goodtillcancelled)                     = lob_updatestats (newbid p x (neworder x ob))
                | ty==(Offer Goodtillcancelled)                   = lob_updatestats (newoffer p x (neworder x ob))
                | or [(ty==(Sell Andkill)), (ty==(Buy Andkill))]  = lob_updatestats (f ty)
                | (ty==None)                                      = lob_updatestats ob 
                | otherwise                                       = lob_updatestats ob 
                   where
                   ty = order_gettype x
                   p  = order_getprice x
                   size = order_getsize x
                   bs_liquidity = lob_getbuysideliquidity ob   
                   ss_liquidity = lob_getsellsideliquidity ob
                   bs_ratio =if (bs_liquidity /=0)  
                             then (size / bs_liquidity) 
                             else 1
                   ss_ratio = if (ss_liquidity /=0) 
                              then (size / ss_liquidity) 
                              else 1
                   tinfo = lob_gettraderinfo (order_gettraderid x) ob
                   (tid, ltaken, penalty, timeapplied)  = tinfo
                   current_time = (lob_gettime ob) + 1
                   this_trader_being_attenuated
                         |(tinfo == emptytraderinfo_item)                   = False
                         |(current_time < (timeapplied + penalty))          = True
                   f (Buy Andkill)|(ss_ratio > 0.020)              = (lob_settraderinfo newtinfo ob)                    
                                  |(ss_ratio > 0.015)              = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))           
                                  |(ss_ratio > 0.010)              = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))           
                                  |(ss_ratio > 0.005)              = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))           
                                  | this_trader_being_attenuated   = ob                                                
                                  | otherwise                      = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob)))
                                     where
                                     newtinfo = (tid, ss_ratio, penalty, current_time)
                                     penalty| (ss_ratio > 0.020)    = 400
                                            | (ss_ratio > 0.015)    = 200
                                            | (ss_ratio > 0.010)    = 100
                                            | (ss_ratio > 0.005)    = 50
                                            |  otherwise            = 0
                   f (Sell Andkill)|(bs_ratio > 0.020)             = (lob_settraderinfo newtinfo ob)                    
                                   |(bs_ratio > 0.015)             = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))          
                                   |(bs_ratio > 0.010)             = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))          
                                   |(bs_ratio > 0.005)             = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))          
                                   |this_trader_being_attenuated   = ob                                               
                                   |otherwise                      = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob)))
                                      where
                                      newtinfo = (tid, bs_ratio, penalty, current_time)
                                      penalty| (ss_ratio > 0.020)    = 400  
                                             | (ss_ratio > 0.015)    = 200  
                                             | (ss_ratio > 0.010)    = 100  
                                             | (ss_ratio > 0.005)    = 50   
                                             |  otherwise            = 0    
   
   --The match_2 matching engine extends match_m1 by repricing all bids > 35% away from current best bid and all
   --offers > 35% away from current best offer.
   match_m2 x ob | (ty==(Bid Goodtillcancelled))   = lob_updatestats checkbid   
                 | (ty==(Offer Goodtillcancelled)) = lob_updatestats checkoffer 
                 | otherwise                         = lob_updatestats (match_m1 x ob)
                   where
                   ty         = order_gettype x
                   p          = order_getprice x
                   checkbid   | or[(bestbid==0),(bidgap <= 0.35)]  = match_m1 x ob
                              | (p > bestbid)                     = match_m1 (order_setprice (bestbid*1.35) x) ob
                              | otherwise                          = match_m1 (order_setprice (bestbid*0.65) x) ob
                   checkoffer | or[(bestoffer==0),(offergap <= 0.35)]  = match_m1 x ob
                              | (p < bestoffer)                       = match_m1 (order_setprice (bestoffer*0.65) x) ob
                              | otherwise                             = match_m1 (order_setprice (bestoffer*1.35) x) ob
                   bestbid    = lob_getbestbid ob
                   bestoffer  = lob_getbestoffer ob
                   bidgap     = if bestbid /= 0   
                                then abs(bestbid - p) / bestbid
                                else 0.01
                   offergap   = if bestoffer /= 0  
                                then abs(bestoffer - p) / bestoffer
                                else 0.01
   
   --The match_m3 matching engine is similar to match_m1 but stricter in that it uses the ratio against the available depth
   --at the top of the book.
   match_m3 x ob | (ty==(Bid Goodtillcancelled))                        = lob_updatestats ((newbid p x (neworder x ob))) 
                 | (ty==(Offer Goodtillcancelled))                      = lob_updatestats ((newoffer p x (neworder x ob)))
                 | or [(ty==(Sell Andkill)), (ty==(Buy Andkill))]       = lob_updatestats (f ty)
                 | otherwise                                            = lob_updatestats ob
                   where
                   ty = order_gettype x
                   p  = order_getprice x
                   size = order_getsize x
                   bs_liquidity = lob_getbuysidedepthneartop ob 
                   ss_liquidity = lob_getsellsidedepthneartop ob
                   ss_low = (ss_liquidity < bs_liquidity) && (((bs_liquidity - ss_liquidity)/maximum[bs_liquidity,1]) > 0.25)
                   bs_low = (bs_liquidity < ss_liquidity) && (((ss_liquidity - bs_liquidity)/maximum[ss_liquidity,1]) > 0.25)
                   --bs_ratio :: Double
                   bs_ratio = if (bs_liquidity /=0) 
                              then (size / bs_liquidity)
                              else 1 
                   --ss_ratio :: Double           
                   ss_ratio = if (ss_liquidity /=0)    
                              then ( size /  ss_liquidity) 
                              else 1
                   tinfo = lob_gettraderinfo (order_gettraderid x) ob
                   (tid, ltaken, penalty, timeapplied)  = tinfo
                   current_time = (lob_gettime ob) + 1
                   this_trader_being_attenuated
                          | (tinfo == emptytraderinfo_item)            = False
                          | (current_time < (timeapplied + penalty))   = True 
                   f (Buy Andkill) | (ss_low)              = ob                                                 
                                   | (ss_ratio > 0.020)    = (lob_settraderinfo newtinfo ob)                    
                                   | (ss_ratio > 0.005)    = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))             
                                   | this_trader_being_attenuated    = ob                                                 
                                   | otherwise  = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob)))
                                      where
                                      newtinfo :: Traderinfo_item
                                      newtinfo = (tid, ss_ratio, penalty, current_time)
                                      penalty = penalty_function ss_ratio (0,0,0)
                   f (Sell Andkill)| (bs_low)             = ob                                           
                                   | (bs_ratio > 0.020)   = (lob_settraderinfo newtinfo ob)                   
                                   | (bs_ratio > 0.005) = (lob_settraderinfo newtinfo (newtrade p x (neworder x ob)))             
                                   | this_trader_being_attenuated   = ob                                                 
                                   | otherwise    = (lob_settraderinfo emptytraderinfo_item (newtrade p x (neworder x ob))) 
                                     where
                                     newtinfo = (tid, bs_ratio, penalty, current_time)
                                     penalty = penalty_function bs_ratio (0,0,0)
                                     
   match_m4 (alpha, beta, gamma, delta, epsilon) x ob            --not in use? what are meanings of those five parameters
                 = lob_updatestats (xmatch_m4 ty)
                   where
                   ty = order_gettype x
                   p = order_getprice x
                   size = order_getsize x
                   bestbid =  lob_getbestbid ob
                   bestoffer =  lob_getbestoffer ob
                   xmatch_m4 (Buy Andkill)   = f ss_ratio newtrade
                   xmatch_m4 (Bid Goodtillcancelled)   = if (p > bestoffer)  
                                                         then f ss_ratio newbid
                                                         else (newbid p x (neworder x ob))
                   xmatch_m4 (Sell Andkill)  = f bs_ratio newtrade
                   xmatch_m4 (Offer Goodtillcancelled) =if (p < bestbid) 
                                                        then f bs_ratio newoffer
                                                        else (newoffer p x (neworder x ob))
                   xmatch_m4 any   = error ("xmatch_m4: bad order type: "++(show any))
                   bs_liquidity =lob_getbuysidedepthneartop ob 
                   ss_liquidity =lob_getsellsidedepthneartop ob
                   bs_ratio = if bs_liquidity /= 0 
                              then (size / bs_liquidity)
                              else  1
                   ss_ratio = if ss_liquidity /= 0
                              then (size / ss_liquidity)
                              else 1
                   tinfo = lob_gettraderinfo (order_gettraderid x) ob
                   (tid, ltaken, penalty, timeapplied) = tinfo
                   current_time = (lob_gettime ob) + 1
                   this_trader_being_attenuated | (tinfo == emptytraderinfo_item)            = False
                                                | (current_time < (timeapplied + penalty))   = True
                                                | otherwise                                  = False
                   f ratio g | this_trader_being_attenuated  = ob
                             | (ratio > delta)               = (lob_settraderinfo newtinfo ob)  -- originally 0.02
                             | (ratio > epsilon)             = (lob_settraderinfo newtinfo (g p x (neworder x ob))) -- originally 0.005
                             | otherwise                        = (lob_settraderinfo emptytraderinfo_item (g p x (neworder x ob)))
                                  where
                                  newtinfo = (tid, ratio, penalty, current_time)
                                  penalty = penalty_function ratio (alpha, beta, gamma)
      
   showlob (LOB b o bw sw tr ti or sen tinfo stats)
       = "Limit Order Book at time t="++(show ti)++" with current value="++(show cv)++"\n-----------------------------\n"++
         "Offers:\n" ++ (show o) ++ "\n" ++
         "Bids:\n" ++ (show b) ++ "\n" ++
         "Buys Waiting: " ++ (show bw) ++ "\n" ++
         "Sells Waiting: " ++ (show sw) ++ "\n" ++
         "Trades Done: " ++ (show tr) ++ "\n" ++
         "Orders Received: " ++ (show or) ++ "\n" ++
         "Sentiment: " ++ (show sen) ++ "\n"
         where
         (cv,a1,a2,a3,samp, percent,a4,a5,a6,a7,a8,a9) = stats



probelob x = (match probe2 (match probe1 (testlob x)))
             where
             probe:: Order
             probe = order_create 5000 100 0 Phantom (Offer Goodtillcancelled)
             probe1:: Order
             probe1 = order_create 1950 48 0 (Intermediary 1) (Offer Goodtillcancelled)
             probe2:: Order
             probe2 = order_create 1950 48 0 (Intermediary 2) (Offer Goodtillcancelled)
             probe3:: Order
             probe3 = order_create 1950 48 0 (Intermediary 3) (Offer Goodtillcancelled)
             probe4:: Order
             probe4 = order_create 1950 48 0 (Intermediary 4) (Offer Goodtillcancelled)
emptytraderinfo_item = (Phantom, 0, 0, 0)  

execute_sells:: (Bids, Sells_waiting, Trades_done, Sells_waiting) -> (Bids, Sells_waiting, Trades_done)
execute_sells (bb, [], tr, notdone) 
    = (bb, notdone, tr)
execute_sells (bb, (s:sw), tr, notdone)                    --take sells in time order - front of list is oldest
    = execute_sells (newbb, sw, newtr, newnotdone)
      where
      (newbb, newtr, newnotdone) = if (isemptyorder s1)  
                                   then (ol_clean bb1, tr1++tr, notdone)  -- then the market order was completely filled
                                   else (ol_clean bb1, tr1++tr, (s1:notdone))
      (bb1, s1, tr1) = ol_fill bb s (Sell Andkill) []

execute_buys:: (Offers, Buys_waiting, Trades_done, Buys_waiting) -> (Offers, Buys_waiting, Trades_done)
execute_buys (oo, [], tr, notdone) = (oo, notdone, tr) 
execute_buys (oo, (b:bw), tr, notdone)                    --take buys in time order - front of list is oldest
    = execute_buys (newoo, bw, newtr, newnotdone)
      where
      (newoo, newtr, newnotdone) = if (isemptyorder b1)
                                   then (ol_clean oo1, tr1++tr, notdone)  -- then the market order was completely filled
                                   else (ol_clean oo1, tr1++tr, (b1:notdone))
      (oo1, b1, tr1) = ol_fill oo b (Buy Andkill) []   
              
-- The penalty function determines the time attenuation to be applied to a trader according to the ratio of the size of the order to the available liquidity    
penalty_function::Double -> (Double,Double,Double) -> Int                                                                                                    
penalty_function ratio (alpha, beta, gamma)                                                                                                                     
             = if (ratio <= 0.005)                                                                                                                              
               then 0                                                                                                                                           
               else minimum [floor(alpha*exp(ratio*beta)),round gamma]                                                          






--The lob broadcast type

--contents are bestbid, bestoffer, buy+sell side liquidities, buy+sell side levels, buy+sell side depths near top



--type Lobsummary_t = [Double] 
getlobsummary :: Lob -> Lobsummary_t
getlobsummary xlob = [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd, or]
                     where
                     bb = lob_getbestbid(xlob)
                     bo = lob_getbestoffer(xlob)
                     bsld = lob_getbuysideliquidity(xlob)
                     ssld = lob_getsellsideliquidity(xlob)
                     bsls = fromIntegral $lob_getbuysidelevels(xlob)
                     ssls = fromIntegral $lob_getsellsidelevels(xlob)
                     bsd = lob_getbuysidedepthneartop(xlob)
                     ssd = lob_getsellsidedepthneartop(xlob)
                     or = fromIntegral $lob_getordernum(xlob)

lobS_getbestbid [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = bb
lobS_getbestoffer [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = bo
lobS_getbuysideliquidity [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = bsld
lobS_getsellsideliquidity [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = ssld
lobS_getbuysidelevels [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = bsls
lobS_getsellsidelevels [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = ssls
lobS_getbuysidedepthneartop [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = bsd
lobS_getsellsidedepthneartop [bb, bo, bsld, ssld, bsls, ssls, bsd, ssd] = ssd

--showlobsummary_t lobsum = "Best bid: " ++ (show bb) ++ "\nBest offer: " ++ (show bo) ++ "\nBuy side liquidity: " ++ (show bsld) ++ "\nSell side liquidity: " ++ (show ssld) ++ "\nBuy side levels: " ++ (show bsls) ++ "\nSell side levels: " ++ (show ssls) ++ "\nBuy side depth near top: " ++ (show bsd) ++ "\nSell side depth near top: " ++ (show ssd)
--                          where
--                          bb = lobS_getbestbid lobsum
--                          bo = lobS_getbestoffer lobsum
--                          bsld = lobS_getbuysideliquidity lobsum
--                          ssld = lobS_getsellsideliquidity lobsum
--                          bsls = lobS_getbuysidelevels lobsum
--                          ssls = lobS_getsellsidelevels lobsum
--                          bsd = lobS_getsellsidedepthneartop lobsum
--                          ssd = lobS_getsellsidedepthneartop lobsum
