/-  spider
/+  strandio
=,  strand=strand:spider
=,  strand-fail=strand-fail:libstrand:spider
|%
++  process-lanes
  |=  [target=@p lanes=(list lane:ames)]
  =/  m  (strand ,~)
  ^-  form:m
  ?~  `(list lane:ames)`lanes
    %-  (slog leaf+"No route for {(scow %p target)}." ~)
    (pure:m ~)
  =/  lroute  (skip lanes |=(a=lane:ames -.a))
  ?~  lroute
    %-  (slog leaf+"No direct route for {(scow %p target)}." ~)
    (pure:m ~)
  =/  ip  +:(scow %if p.i.lroute)
  =/  port  (skip (scow %ud (cut 5 [1 1] p.i.lroute)) |=(a=@tD =(a '.')))
  %-  (slog leaf+"{ip}:{port}" ~)
  (pure:m ~)
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  utarget  !<  (unit @p)  arg
?~  utarget
  (strand-fail %no-arg ~)
=/  target  u.utarget
;<  lanes=(list lane:ames)  bind:m  (scry:strandio (list lane:ames) /ax//peers/(scot %p target)/forward-lane)
;<  ~                       bind:m  (process-lanes target lanes)
(pure:m !>(~))