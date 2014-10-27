{-# LANGUAGE GADTs #-} 
{-# LANGUAGE FlexibleInstances #-} 
module Comm where                        --common types and functions

import Baserands
import Data.Time.Clock   
import System.IO.Unsafe  

import Data.Fixed(mod')   

--------------------------------------------------------------------
--BASIC TYPES

type Price = Double
type Size = Double                --really clumsy. It should be Int, but there are a lot of computations with Doubles.  
type Time = Int
type Inventory = Size


data Ordertype = Bid Limitorder_t | Offer Limitorder_t | Sell Marketorder_t | Buy Marketorder_t | None | Abort
                 deriving (Show,Eq)
data Marketorder_t = Orkill | Andkill
                     deriving (Show,Eq)
data Limitorder_t = Goodtillcancelled | Goodtilldate Time  -- Not sure where these two should be - Minimumquantity | Maximumquantity | Day
                    deriving (Show,Eq)
data Traderid = Phantom | Intermediary Int | HFT Int |  FundSeller Int | FundBuyer Int |  Small Int | Opportunistic Int |
             SSTaker Int | BSTaker Int | BSMaker Int | SSMaker Int | Maker Int | Taker Int
             deriving (Show,Eq)
data Sentiment = Calm | Choppy | Ramp | Toxic
                 deriving (Show,Eq)                            --need to test Eq
                                 
type Order = (Price, Size, (Time, Time), Traderid, Ordertype, Int)  


type Orderlist = [(Price, [Order])]  

data Lob = LOB Bids Offers Buys_waiting Sells_waiting Trades_done Time Orders_received Sentiment Traderinfo Statstype
           deriving (Eq) 
           
type Lobsummary_t = [Double]            
type Lobtrace = [[Char]]
type Bids = Orderlist
type Offers = Orderlist
type Buys_waiting = [Order]
type Sells_waiting = [Order]
type Trades_done = [(Order, Order)]
type Orders_received = [Order]
type Traderinfo = [Traderinfo_item]
type Traderinfo_item = (Traderid, Percent_liquidity_taken, Penalty_applied, Time)   -- time is time penalty was applied
type Percent_liquidity_taken = Double
type Penalty_applied = Time               
type Statstype = (Price,Price,Double,Double,Double,Double, --  underlying value, last traded price, squared error, cumulative squared error,samples,percent,
                  (Size,Size,Size,Size),               --  minliquiditybs, maxliquiditybs, liquiditybs, maxdrawdownbs,
                  (Size,Size,Size,Size),               --  minliquidityss, maxliquidityss, liquidityss, maxdrawdownss
                  (Size,Size,Size,Size),               --  minliquiditybstop, maxliquiditybstop, liquiditybstop, maxdrawdownbstop,
                  (Size,Size,Size,Size),               --  minliquiditysstop, maxliquiditysstop, liquiditysstop, maxdrawdownsstop
                  (Size,Size,Size,Size),               --  zeroliquiditybs, maxzeroliquiditybs, zeroliquidityss, maxzeroliquidityss
                  (Size,Size,Size,Size)                --  zeroliquiditybstop, maxzeroliquiditybstop, zeroliquiditysstop, maxzeroliquiditysstop
                 )
               



data Msg_t = Hiaton | Cancelmessage (Int,Int) (Int,Int) | Message (Int,Int) [Arg_t] | Ordermessage (Int,Int) Order | Trademessage (Int,Int) Order Order| Broadcastmessage (Int,Int) Broadcast_t | Datamessage (Int,Int) [Char] | Ackmessage (Int,Int) Int Order [Char] | Debugmessage (Int,Int) [Char]
             deriving (Show,Eq)   --(from,to) identifiers 
             
             
data Broadcast_t = Numlistbroadcast [Double] | Strbroadcast Str 
                   deriving (Show,Eq)  -- Add your self-defined broadcasts here             
             
data Str = EmptyString | Str [Char]
           deriving (Show,Eq)  
--data Arg_t  = EmptyArg | Arg (Str,Int )
data Arg_t  where{
     EmptyArg :: Arg_t;
     Arg :: (Str,Double)-> Arg_t                       -- need more discussion
}   deriving (Show,Eq)  
arg_getstr :: Arg_t -> [Char]
arg_getstr (Arg ((Str x),y)) = x
arg_getnum (Arg ((Str x),y)) = y
arg_findval key [] = -1
arg_findval key ((Arg ((Str x),y)):t) =  if x == key 
                                         then y 
                                         else arg_findval key t
arg_findstr key [] = ""
arg_findstr key ((Arg ((Str x),y)):t) =  if y == key 
                                         then x 
                                         else arg_findstr key t    
                                         


                                         
type Agent_t      = Agentstate_t -> [Arg_t] -> [(Int, [Msg_t], [Msg_t])] -> Int -> [[Msg_t]]  --Agents take a tuple of (time, messages, broadcasts) last num is id from sim
data Agentstate_t = Agentstate (Double, Double, Int, Lob) 
                    | Emptyagentstate 
                    | Exchstate Lob 
                    | Nicemimestate Nice_mime_lob 
                    | Traderstate (Price, Price, Double, Sentiment, Double, [Order -> Order]) 
                    | Newagstate ([Double], [Order], Sentiment, [Order -> Order], [Double], [Double])  --traderstate is old_bestbid, old_bestoffer, old_ordernum
                    deriving (Eq)  
emptyagentstate = Emptyagentstate     -- we don't yet know what this will or should be                         
instance Eq (Order -> Order) where
      x == y = True  
      x /= y = not (x == y)                                                       
                                         
data Nice_mime_lob = Nice_mime_lob Sentiment Listoftotals Nubids Nuoffers Time Ticksize Lasttradedprice Stats 
                     deriving (Eq)     -- need to be confirmed    
type Listoftotals = [Double]                                                                                             
type Stats = [Double]                                                                                                    
type Nubids = [Tick]                                                                                                  
type Nuoffers = [Tick]                                                                                                
type Ticksize = Double                                                                                                   
type Tick = (Double, [Order])                                                                                            
type Lasttradedprice = Double                                         
                                         
                                         
                                         
                                                  
---------------------------------------------------------------------



--TYPE CLASSES
-------------------------------------------------------------------------------
class C_order order where
    emptyorder :: order
    isemptyorder :: order -> Bool
    order_create :: Price -> Size -> Time -> Traderid -> Ordertype -> order
    order_getprice :: order -> Price
    order_setprice :: Price -> order -> order
    order_getuid :: order -> Int
    order_setuid :: [Char] -> Int -> order -> order
    order_gettime :: order -> Time
    order_setfractime :: Time -> order -> order
    order_getfractime :: order -> Time
    order_gettype :: order -> Ordertype
    order_gettraderid :: order -> Traderid
    order_gettraderidno :: order -> Int
    order_getsize :: order -> Size
    order_newsize :: order -> Size -> order
    order_newprice :: order -> Price -> order
    order_equals :: order -> order -> Bool
    order_samesignature :: order -> order -> Bool
    order_getexpiry :: order -> Time
    showorder :: order -> [Char]
    
    
class C_Orderlist orderlist where
    emptyorderlist :: orderlist
    isemptyorderlist :: orderlist -> Bool
    ol_first :: orderlist -> (Price,[Order])
    ol_last :: orderlist -> (Price,[Order])
    ol_insert :: Price -> orderlist -> Order -> orderlist
    ol_delete :: Price -> orderlist -> Order -> orderlist
    ol_reverse :: orderlist -> orderlist
    ol_gethighestprice :: orderlist -> Price
    ol_getlowestprice :: orderlist -> Price
    ol_getliquidity :: orderlist -> Size
    ol_getlevels :: orderlist -> Int                      -- neet to confirm 
    ol_getdepth :: Ordertype -> orderlist -> Size          -- neet to confirm
    ol_getdepthneartop :: Ordertype -> Double -> orderlist -> Size   -- neet to confirm
    ol_poplowest :: orderlist -> (Order, orderlist)
    ol_clean :: orderlist -> orderlist
    ol_fill :: orderlist -> Order -> Ordertype -> [(Order, Order)] -> (orderlist, Order, [(Order, Order)])
    ol_fill_cross :: orderlist -> Order -> Ordertype -> [(Order, Order)] -> (orderlist, Order, [(Order, Order)])
    showorderlist :: orderlist -> [Char]
    


class C_Lob lob where
     emptylob :: lob
     primed_emptylob :: lob
     testlob :: Int -> lob
     lob_increment_time :: lob -> lob 
     lob_clear_trades :: lob -> lob 
     lob_updatestats :: lob -> lob
     lob_setpercent :: Double -> lob -> lob
     lob_getpercent :: lob -> Double
     lob_gettime :: lob -> Time
     lob_getbestbid :: lob -> Price
     lob_getbestoffer :: lob -> Price
     lob_getsentiment :: lob -> Sentiment
     lob_setsentiment :: Sentiment -> lob -> lob
     lob_getbuysideliquidity :: lob -> Size
     lob_getsellsideliquidity :: lob -> Size
     lob_getmidprice :: lob -> Price
     lob_getlasttradedprice :: lob -> Price
     lob_getordernum :: lob -> Int
     lob_getbuysidelevels :: lob -> Int
     lob_getbuysidedepth :: lob -> Size
     lob_getbuysidedepthneartop :: lob -> Size 
     lob_getsellsidelevels :: lob -> Int
     lob_getsellsidedepth :: lob -> Size
     lob_getsellsidedepthneartop :: lob -> Size
     lob_gettraderinfo :: Traderid -> lob -> Traderinfo_item
     lob_settraderinfo :: Traderinfo_item -> lob -> lob
     lob_gettrace :: lob -> [Char]
     lob_getstats :: lob -> Statstype
     newbid :: Price -> Order -> lob -> lob
     newoffer :: Price -> Order -> lob -> lob
     newtrade :: Price -> Order -> lob -> lob
     neworder :: Order -> lob -> lob
     lob_gettrades :: lob -> Trades_done
     execute_trades :: lob -> lob
     is_crossed_book :: lob -> Bool
     uncross_book :: lob -> lob
     match :: Order -> lob -> lob
     match_m1 :: Order -> lob -> lob  
     match_m2 :: Order -> lob -> lob
     match_m3 :: Order -> lob -> lob
     match_m4 :: (Double,Double,Double,Double,Double) -> Order -> lob -> lob
     showlob :: lob -> [Char]


class C_msg_t msg_t where 
    hiaton :: msg_t    
    msg_getid :: msg_t -> Int
    msg_getfromid :: msg_t -> Int
    showmsg_t :: msg_t -> [Char]
    message :: (Int,Int) -> [Arg_t] -> msg_t
    ordermessage :: (Int,Int) -> Order -> msg_t
    cancelmessage :: (Int,Int) -> (Int,Int) -> msg_t -- First tuple is from/to, second tuple is tid/ouid
    trademessage :: (Int,Int) -> Order -> Order -> msg_t
    datamessage :: (Int, Int) -> [Char] -> msg_t
    debugmessage :: (Int,Int) -> [Char] -> msg_t
    ackmessage :: (Int, Int) -> Int -> Order -> [Char] -> msg_t --0 accept, 1 order too large, 2 no more liquidity, 3 outside sliding window of acceptable prices, 4 too many contracts on book, 5 order cancelled, 6 book is spiked, 7 minimum resting time not obeyed.
    broadcastmessage :: (Int,Int) -> Broadcast_t -> msg_t
    msg_isbroadcast :: msg_t -> Bool
    msg_isdata :: msg_t -> Bool
    msg_disptrace :: msg_t -> [Char]
    msg_getbroadcast :: msg_t -> Broadcast_t
    msg_getorders :: [msg_t] -> [Order] -- returns orders from list of messages, ignores non ordermessages
    msg_getnumlistfrombcast :: msg_t -> [Double]
    msg_gettrade :: msg_t -> [Order]
    msg_isack :: msg_t -> Bool
    msg_getackcode :: msg_t -> Int
    msg_istrade :: msg_t -> Bool
    msg_isreject :: msg_t -> Bool --Named this isreject instead of isack to avoid nameclashing and confusion.
    msg_getcanceltuple :: msg_t -> [(Int,Int)]
    msg_getackdorder :: msg_t -> Order
    


class C_broadcast_t broadcast_t where 
    showbroadcast_t :: broadcast_t -> [Char]
    broadcast_getnumlist :: broadcast_t -> [Double]
    broadcast_numlist :: [Double] -> broadcast_t
    broadcast_str  :: Str -> broadcast_t



-------------------------------------------------------------------------------

-----------------------------------------------------------------------------       
--DISTRIBUTIONS

--rands generates random numbers in the range 0 - 1,000
randoms :: [Int]
randoms = baserands ++ randoms



gaussians :: [Double]
gaussians = mygaussians (map fromIntegral randoms) -- range is -6000 to 6000
            where
            mygaussians [] = []
            mygaussians (a:b:c:d:e:f:g:h:i:j:k:l:rest) = ((a+b+c+d+e+f+g+h+i+j+k+l) - 6000):(mygaussians rest)

-- value is a function that gives the underlying (fundamental) value of the stock being traded as a function of time.  It can be uniform
-- (the easiest case) or rising, falling or sinusoidal.

value:: Sentiment->Time->Double
value Calm   t = 2000
value Choppy t = ((sines !!t) + 1.0  ) * 1500 + 1000
value Ramp   t = ([2000,1999 ..]!! t) 
value Toxic  t = if t < 40
                 then 2000 
                 else 5

sines:: [Double]
sines = [sin x | x <- [0.0,0.1 .. (2*pi)]] ++ sines


------------------------------------------------------------------------------------
--Helper function
rep n x = take n (repeat x) 

systemTime :: Num a=> a -> Double                           --need to verify
systemTime x = unsafePerformIO $ getTime
               where
               getTime = getCurrentTime >>= return . fromRational . toRational .utctDayTime   --using monad

cpsafehd caller x = safehd x caller
safehd :: [a] -> [Char] -> a
safehd x caller = if not(null x)
                  then head x 
                  else error ("safehd - from " ++ caller)

zipfunc x [] = []
zipfunc [] x = []
zipfunc (f:rf) (a:ra) = ((f a) : (zipfunc rf ra))
si warn list n = if n < (length list)  
                 then  list!!n
                 else error warn

mysuperor f1 f2 x = or [(f1 x), (f2 x)]



mymod:: Double -> Double -> Double           -- be careful of the type
mymod x 0 = error "mymod applied to zero" 
--mymod x y  | y>x  = x              
--           | otherwise = x - (fromIntegral(floor(x/y)))*y  

--mymod x y = mymod1 x y                      -- version3
--            where 
--            mymod1 x y | (y>x) = x
--                       | otherwise = mymod1(x-y) y
mymod x y = mod' x y                          --version 2
--mymod x y |(x >= 2*y)    = mymod (x-y) y    --version 1
--          |(y > x)       = x             
--          |otherwise     = x-y  



--------------------------------------------------------------------------