/-  *netlas
|_  act=action
++  grow
  |%
  ++  noun  act
  --
::  We don't currently use this, this goes through the console.
++  grab
  |%
  ++  noun  action
  ++  json
    =,  dejs:format
    |=  jon=json
    ^-  action
    %.  jon
    %-  of
    :~ 
        [%testaction (se %p)]
    ==
  --
++  grad  %noun
--