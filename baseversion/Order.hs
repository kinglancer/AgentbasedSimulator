{-# LANGUAGE FlexibleInstances #-}                   -- type Order is type synonym, could not use on type class       
{-# LANGUAGE TypeSynonymInstances #-}               

module Order where


import Comm


instance C_order Order  where
   emptyorder = (0, 0, (0,0), Phantom, None, 0)
   isemptyorder (0, 0, (0,0), x, any, 0) = True
   isemptyorder any = False
   order_create p s t tid ty = (p, s, (t, -1), tid, ty, -1)
   order_getprice (p, s, t, tid, ty, ouid) = p
   order_setprice newp (p, s, t, tid, ty, ouid) = (newp, s, t, tid, ty, ouid)
   order_setuid caller newuid (p, s, t, tid, ty, (-1)) = (p, s, t, tid, ty, newuid)
   order_setuid caller x y  = error ("order_setuid - Resetting uid of an order.\n" ++ (show y) ++ "\n" ++ (show x) ++ "\n" ++ (show caller))
   order_getuid (p, s, t, tid, ty, ouid) = ouid
   order_gettime (p, s, (t, ts), tid, ty, ouid) = t
   order_setfractime ftime (p, s, (t, (-1)), tid, ty, ouid) = (p, s, (t, ftime), tid, ty, ouid)
   order_setfractime x y = error "order_setfractime - Resetting fractime of an order."
   order_getfractime (p, s, (t, ts), tid, ty, ouid) = ts
   order_getsize (p, s, t, tid, ty, ouid) = s
   order_gettype (p, s, t, tid, ty, ouid) = ty
   order_gettraderid (p, s, t, tid, ty, ouid) = tid
   order_gettraderidno (p, s, t, (Phantom), ty, ouid) = 0 -- error ("order_gettraderidno - Took tid of phantom: " ++ (show p) ++ " " ++(show s)++ " " ++(show t)++ " " ++(show ty))
   order_gettraderidno (p, s, t, (Intermediary tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (HFT tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (FundSeller tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (FundBuyer tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (Small tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (Opportunistic tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (SSTaker tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (BSTaker tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (SSMaker tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (Maker tid), ty, ouid) = tid
   order_gettraderidno (p, s, t, (Taker tid), ty, ouid) = tid
   order_getexpiry (p, s, t, tid, (Offer (Goodtilldate etime)), ouid) = etime
   order_getexpiry (p, s, t, tid, (Bid (Goodtilldate etime)), ouid) = etime
   order_getexpiry anythingelse = -1 -- error situation
   order_newsize (p, s, t, tid, ty, ouid) news  = if (news >= 0)
                                                  then (p, news, t, tid, ty, ouid) 
                                                  else error ("Updating order to negative size " ++ (show news))
   order_newprice (p, s, t, tid, ty, ouid) newp = if (newp >= 0) 
                                                  then (newp, s, t, tid, ty, ouid)
                                                  else error ("Updating order to negative price " ++ (show newp))
   order_equals (p1, s1, t1, tid1, ty1, ouid1) (p2, s2, t2, tid2, ty2, ouid2) | (p1==p2)&&(t1==t2)&&(tid1==tid2)&&(ty1==ty2)&&(ouid1==ouid2) = True     --not sure about these two functions
                                                                              | otherwise = False
   order_samesignature (p1, s1, t1, tid1, ty1, ouid1) (p2, s2, t2, tid2, ty2, ouid2) | (p1==p2)&&(t1==t2)&&(tid1==tid2)&&(ty1==ty2)&&(ouid1==ouid2) = True
                                                                                     | otherwise = False

   
   showorder (p, s, t, tid, ty, ouid) 
       = "(uid: " ++ (show ouid) ++ ", $" ++ (show p) ++ ", " ++ (show s) ++ " shares, at " ++ (show t) ++ " secs,  from " ++ (show tid) ++ ", of type " ++ (show ty) ++ ")\n     "