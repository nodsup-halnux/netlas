::  Similar to default-agent except crashes everywhere
:: Gall Agent Is just a door that has a bowl for a sample
:: The Agent and bowl structures live in agent:gall
^-  agent:gall
|_  bowl:gall
++  on-init
  ::(list card this) was just shorthand for (list card <theagentitself!>)
  ^-  (quip card:agent:gall agent:gall)
  !!
++  on-save
  ^-  vase
  !!
:: And thats all...
++  on-load
  |~  old-state=vase
  ^-  (quip card:agent:gall agent:gall)
  !!
::
++  on-poke
  |~  in-poke-data=cage
  ^-  (quip card:agent:gall agent:gall)
  !!
::
++  on-watch
  |~  path
  ^-  (quip card:agent:gall agent:gall)
  !!
::
++  on-leave
  |~  path
  ^-  (quip card:agent:gall agent:gall)
  !!
::
++  on-peek
  |~  path
  ^-  (unit (unit cage))
  !!
::
++  on-agent
  |~  [wire sign:agent:gall]
  ^-  (quip card:agent:gall agent:gall)
  !!
::
++  on-arvo
  |~  [wire =sign-arvo]
  ^-  (quip card:agent:gall agent:gall)
  !!
::
++  on-fail
  |~  [term tang]
  ^-  (quip card:agent:gall agent:gall)
  !!
--