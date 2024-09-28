/-  *netlas
|_  upd=update
++  grow
  |%
  ++  noun  upd
  ++  json
    =,  enjs:format
    ^-  ^json
    ?-    -.upd
      %testupdate
           %+  frond  'init'
             %-  pairs
              :~  ['ack' (numb ack.upd)]  ==
      ::  We can construct more complicated pair
      ::  structures, but atomic is more simple.
    ==  :: End ?-
  --
++  grab
  |%
  ++  noun  update
  --
++  grad  %noun
--