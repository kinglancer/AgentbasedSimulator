
module Messages where

import Order
import Comm


                                         
                                         
                                         
--The message type & broadcast message type 
---------------------------------

instance C_msg_t Msg_t  where
    hiaton = Hiaton   
    
    --constructors.
    message x args  = Message x args
    debugmessage x y = Debugmessage x y
    ordermessage x anOrder = Ordermessage x anOrder
    broadcastmessage x broadcast = Broadcastmessage x broadcast
    cancelmessage fromto idtuple = Cancelmessage fromto idtuple
    trademessage fromto ordera orderb = Trademessage fromto ordera orderb
    datamessage from dat = Datamessage from dat
    ackmessage fromto ackd x msgg = (Ackmessage fromto ackd x msgg)
    
    --msg_t functions
    msg_getid Hiaton = 0
    msg_getid (Message (from,to) args) = to
    msg_getid (Ordermessage (from,to) args) = to
    msg_getid (Broadcastmessage (from,to) args) = to
    msg_getid (Trademessage (from,to) ordera orderb) = to
    msg_getid (Datamessage (from, to) dat) = to
    msg_getid (Ackmessage (d,e) b c m) = e
    msg_getid (Cancelmessage (f,t) b) = t
    msg_getid (Debugmessage (f,t) m) = t
    msg_getfromid Hiaton = 0
    msg_getfromid (Message (from,to) args) = from
    msg_getfromid (Ordermessage (from,to) args) = from
    msg_getfromid (Broadcastmessage (from,to) args) = from
    msg_getfromid (Trademessage (from,to) ordera orderb) = from
    msg_getfromid (Datamessage (from, to) dat) = from
    msg_getfromid (Ackmessage (d,e) b c m) = d
    msg_getfromid (Cancelmessage (f,t) b) = f
    showmsg_t Hiaton = "\nHiaton"
    showmsg_t (Datamessage x dat) = ""
    showmsg_t (Cancelmessage x (trader, order)) = "\nMessage from/to " ++ (show x) ++ ": Trader #" ++ (show trader) ++ " is cancelling order " ++ (show order)
    showmsg_t (Message x args) = "\nMessage from/to "++(show x)++" ["++(concat (map arg_getstr args)) ++ "]\n"
    showmsg_t (Ordermessage x args) = "\nMessage from/to "++(show x)++" "++(showorder args) ++ ""
    showmsg_t (Broadcastmessage x args) = "\nBroadcast from/to group "++(show x)++" ["++(showbroadcast_t args) ++ "]"
    showmsg_t (Trademessage (from,to) ordera orderb) = "\n==========\n\nTo: " ++ (show to) ++ "\nOrder: \n" ++ (show ordera) ++ "\n\n matched with\n\nOrder:" ++ (show orderb) ++ "\n\n=========="
    showmsg_t (Ackmessage (d,e) b c m) = "\nAckmessage(" ++ (show d) ++ "->" ++ (show e) ++ "): The following order " ++ text ++ (show c)
                                         where
                                         text| b /= 0       = "was unsuccessful because " ++ m ++ ": \n"
                                             | b == 5       = "was cancelled successfully." 
                                             | otherwise    = "was successful: \n"
    showmsg_t (Debugmessage ft m) = "\n+=+=+=+=+=+=+=+=+=+=+=+=+=\nDebug message:" ++ (show ft) ++ "\n" ++ m ++ "\n+=+=+=+=+=+=+=+=+=+=+=+=+="
    msg_disptrace (Datamessage x dat) = dat
    msg_disptrace any = ""
    
    
    
    msg_getnumlistfrombcast (Broadcastmessage a b) = broadcast_getnumlist b
    msg_isbroadcast (Broadcastmessage a b) = True
    msg_isbroadcast any = False
    msg_isdata (Datamessage a b) = True
    msg_isdata any = False
    msg_isack (Ackmessage a b c m) = True
    msg_isack any = False
    msg_getackcode (Ackmessage a b c m) = b
    msg_getackcode any = error "msg_getackcode - calling get ack code on non-ack message."
    msg_getackdorder (Ackmessage a b c m) = c
    msg_getackdorder any = error "msg-getackdorder - Called this on a non-ack message"
    msg_istrade (Trademessage (from,to) ordera orderb) = True
    msg_istrade any = False
    msg_isreject (Ackmessage a b c m) = b /= 0
    msg_isreject any = error "msg_isreject called on non-ack message."
    msg_getbroadcast (Broadcastmessage a b) = b
    msg_getbroadcast any = error "Make sure you're trying to get the broadcast FROM a broadcast message first..."
    msg_getorders [] = []
    msg_getorders ((Ordermessage a b) : rest) = b : (msg_getorders rest)
    msg_getorders (any:rest) = msg_getorders rest
    msg_gettrade (Trademessage (from,to) ordera orderb) = [ordera, orderb]
    msg_gettrade any = []
    msg_getcanceltuple (Cancelmessage x b) = [b]
    msg_getcanceltuple any = []
    
    
    
    
    
--map msg_getbroadcast







--The implementation of broadcasts




instance C_broadcast_t Broadcast_t  where 
    showbroadcast_t (Numlistbroadcast nums) = "LOBbcast contents: ["++(show nums) ++ "]"
    showbroadcast_t (Strbroadcast string) = show string
    broadcast_getnumlist (Numlistbroadcast lobsum) = lobsum
    
    broadcast_numlist ob = Numlistbroadcast ob
    broadcast_str string = Strbroadcast string    