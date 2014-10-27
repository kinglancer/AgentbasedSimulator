{-# LANGUAGE FlexibleInstances #-}                   -- type Order is type synonym, could not use on type class       
{-# LANGUAGE TypeSynonymInstances #-} 

module Sim_orderlist where

import Order
import Comm

--------------------------------
--ORDERLIST

--The bids and offers will be subject to the same ordering criterion (which we may wish to vary later) so make a generic abstype for inserting and deleting orders from the data structure
--implementation of orderlist   


instance C_Orderlist Orderlist  where 
    emptyorderlist = []
    
    ol_first [] = (0,[])
    ol_first (x:xs) = x
    
    ol_last [] = (0,[])
    ol_last [x] = x
    ol_last (x:xs) = ol_last xs
    
    isemptyorderlist [] = True
    isemptyorderlist any = False
    
    ol_insert p [] o = [(p,[o])]
    ol_insert p1 ((p2,xs):ys) o
               | p2>p1    = ((p1,[o]):((p2,xs):ys))
               | p2<p1    = (p2,xs): (ol_insert p1 ys o) 
               | p2 == p1 = (p1,newxs):ys   
                             where
                             newxs = insert o xs
                             insert o [] = [o]
                             insert x (y:ys) 
                                 = if (order_gettime x) < (order_gettime y)  
                                   then x:(y:ys)         -- timeorder
                                   else y:(insert x ys)   
    
    ol_delete p [] o = []
    ol_delete p1 ((p2,xs):ys) o
              | p2>p1    = ((p2,xs):ys)    
              | p2<p1    = (p2,xs):(ol_delete p1 ys o)   
              | p2 == p1 = (p1,newxs):ys
                           where
                           newxs = delete o xs
                           delete o [] = []
                           delete x (y:ys)
                               = if (order_equals x y)  
                                 then ys
                                 else y: (delete x ys)    
                  
    
    ol_gethighestprice [] = error "Can't get highest price in empty list of orders"
    ol_gethighestprice [(p,xs)] = p
    ol_gethighestprice (x:xs) = ol_gethighestprice xs
    
    ol_getlowestprice [] = error "Can't get lowest price in empty list of orders"
    ol_getlowestprice ((p,xs):ys) = p
    
    ol_getliquidity [] = 0
    ol_getliquidity ((p,os):xs) = (foldr (+) 0 (map order_getsize os)) + (ol_getliquidity xs)
    
    ol_getlevels [] = 0
    ol_getlevels any = length any
    
    -- We define orderbook list depth as the sum of sizes of orders sitting at one price level at one end of the book
    -- We need to take an ordertype parameter to decide whether to look at the lowest price on the orderbook list (Offers)
    -- or the highest price o the orderbook list (Bids)
    ol_getdepth ty    []          = 0
    ol_getdepth (Offer Goodtillcancelled) ((p,xs):ys) 
                                  = sumsizes xs
                                    where
                                    sumsizes [] = 0
                                    sumsizes (o:os) = (order_getsize o) + (sumsizes os)
    ol_getdepth (Bid Goodtillcancelled)   ((p,xs):[]) 
                                  = sumsizes xs
                                    where
                                    sumsizes [] = 0
                                    sumsizes (o:os) = (order_getsize o) + (sumsizes os)
    ol_getdepth (Bid Goodtillcancelled)   ((p,xs):ys) = ol_getdepth (Bid Goodtillcancelled) ys
    
    -- We define orderbook list depth near top" as the sum of sizes of orders sitting at one price level at one end of the book
    -- plus sizes of other orders sitting on book with a price within X% of the top price.
    -- We need to take an ordertype parameter to decide whether to look at the lowest price on the orderbook list (Offers)
    -- or the highest price o the orderbook list (Bids)
    ol_getdepthneartop ty    percent []          = 0
    ol_getdepthneartop (Offer Goodtillcancelled) percent ((p,xs):ys) 
                                                 = (sumsizes xs) + (f p ys)
                                                   where
                                                   sumsizes [] = 0
                                                   sumsizes (o:os) = (order_getsize o) + (sumsizes os)
                                                   f p [] = 0
                                                   f p ((p1,xs):ys) = if (p1<(p*(1+percent)))    
                                                                      then (sumsizes xs) + (f p ys) 
                                                                      else 0
    ol_getdepthneartop (Bid Goodtillcancelled)   percent ys          = ol_getdepthneartop (Offer Goodtillcancelled) percent (reverse ys)
    
    
    ol_poplowest [] = error "Can't pop lowest order from an empty list of orders"
    ol_poplowest ((p,[]):ys) = ol_poplowest ys
    ol_poplowest ((p,(x:xs)):ys) = (x, ((p,xs):ys))
    
    ol_clean  any = isort (filter (((not.null).snd)) any) []
                    where
                    isort [] any = any
                    isort (x:xs) any = isort xs (insert x any)
                    insert x [] = [x]
                    insert (a,b) ((c,d):ys) = if a<c    
                                              then (a,b):((c,d):ys)
                                              else (c,d):(insert (a,b) ys)
    
    
    --
    -- ol_fill takes a list of resting limit orders, a market order, a (Buy Andkill)/(Sell Andkill) indicator and a list of trades
    --     - The (Buy Andkill)/(Sell Andkill) indicator tells us in which direction to search the order list
    --     - The list of trades should start off as empty, since it is the list of fills to satisfy the market order
    -- ol_fill returns the remaining order list after removing the fills, the remaining market order if it wasn't competely filled,
    -- and the list of fills (the trades)
    --
    ol_fill []           m any tr = (emptyorderlist, m, tr)
    ol_fill ((p2,[]):ys) m (Buy Andkill) tr         --easy - head of list is lowest (Offer Goodtillcancelled) - try it first
            = ol_fill ys m (Buy Andkill) tr
    ol_fill ((p2, (x:xs)):ys) m (Buy Andkill)  tr
           | isemptyorder m         = (((p2,(x:xs)):ys), emptyorder, tr) 
           | msize == xsize         = (((p2,xs):ys), emptyorder, newtr2)
           | msize < xsize          = (((p2,(newx:xs)):ys), emptyorder, newtr2)
           | otherwise              = ol_fill ((p2,xs):ys) newm (Buy Andkill) newtr
              where
              xsize = order_getsize x
              msize = order_getsize m
              newm = order_newsize m (msize - xsize)
              newtr = (x, (order_newprice (order_newsize m xsize) (order_getprice x))): tr
              newx = order_newsize x (xsize - msize)
              newtr2 = (order_newsize x msize, (order_newprice m (order_getprice x))):tr
    ol_fill os m (Sell Andkill) tr = ol_fill (ol_reverse os) m (Buy Andkill) tr 
    ol_fill os m any tr = error "Cannot fill order - market order not provided"
    
     --
     -- ol_fill_cross is used for a crossed book.  In this case the market order isn't really a market order - it is
     -- an (Offer Goodtillcancelled) limit order that is crossed with a (Bid Goodtillcancelled) limit order.  We therefore execute at the mean price.
     -- Also, we have to keep checking after each execution whether the book is still crossed or not - we no longer
     -- have access to the book, so we check whether (mprice > xprice) which would mean no longer crossed.
    
    ol_fill_cross []           m any tr = (emptyorderlist, m, tr)
    ol_fill_cross ((p2,[]):ys) m (Buy Andkill) tr        --easy - head of list is lowest (Offer Goodtillcancelled) - try it first
            = ol_fill_cross ys m (Buy Andkill) tr
    ol_fill_cross ((p2, (x:xs)):ys) m (Buy Andkill)  tr
           | mprice > xprice     = (((p2,(x:xs)):ys), m, tr)
           | isemptyorder m      = (((p2,(x:xs)):ys), emptyorder, tr)
           | msize == xsize       = (((p2,xs):ys), emptyorder, newtr2)
           | msize < xsize       = (((p2,(newx:xs)):ys), emptyorder, newtr2)
           | otherwise           = ol_fill_cross ((p2,xs):ys) newm (Buy Andkill) newtr
              where
              xsize = order_getsize x
              msize = order_getsize m
              xprice = order_getprice x
              mprice = order_getprice m
              executeprice = (mprice + xprice)/2
              newm = order_newsize m (msize - xsize)
              newtr = (order_newprice x executeprice, (order_newprice (order_newsize m xsize) executeprice)): tr
              newx = order_newsize x (xsize - msize)
              newtr2 = (order_newprice (order_newsize x msize) executeprice, (order_newprice m executeprice)):tr
    ol_fill_cross os m (Sell Andkill) tr = ol_fill_cross (ol_reverse os) m (Buy Andkill) tr 
    ol_fill_cross os m any tr = error "Cannot fill crossed order - (Offer Goodtillcancelled) not provided"
    
    
    ol_reverse [] = []
    ol_reverse (x:xs) = (ol_reverse xs) ++ [x]
    
    showorderlist [] = "ENDOL"
    showorderlist ((p,xs):ys) 
        = (show p) ++ ":   " ++ (show xs) ++ "\n" ++ (showorderlist ys)
    
    