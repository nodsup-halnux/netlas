::  dbug: agent wrapper for generic debugging tools
::  This agent's logic is largely contained in ++poke and 
:: ++scry. Every other arm is default.
::    usage: %-(agent:dbug your-agent)
::
|%
:: Inline poke structure
+$  poke
  $%  [%bowl ~]
      [%state grab=cord]
      [%incoming =about]
      [%outgoing =about]
  ==
:: In line about structure
+$  about
  $@  ~
  $%  [%ship =ship]
      [%path =path]
      [%wire =wire]
      [%term =term]
  ==
:: Agent Arm
++  agent
    :: An agent Gate
  |=  =agent:gall
  ^-  agent:gall
  !.
   :: 10 arms inside a door
  |_  =bowl:gall
  :: this is a tistar virtual arm expression. It will
  ::be inserted into the front of every arm below.
  ::The second is the irregular SS of 
  +*  this  .
      ag    ~(. agent bowl)
  ::  Poke Arm - most fleshed out because we interact this way
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall agent:gall)
    ::Wathep but T and F case inverted
    ?.  ?=(%dbug mark)
      ::If not the dbug mark...F case
      =^  cards  agent  (on-poke:ag mark vase)
      ::send whatever cards were there, no state update
      [cards this]
    :: If it is a mark...T case
    =/  dbug
      !<(poke vase)
    =;  =tang
      ((%*(. slog pri 1) tang) [~ this])
    :: Third argument of  =; rune
    ?-  -.dbug
        :: sell will pp vase to a tank.
      %bowl   [(sell !>(bowl))]~
    
    ::  our mark is a state
        %state
      =?  grab.dbug  =('' grab.dbug)  '-'
      =;  product=^vase
        [(sell product)]~
      =/  state=^vase
        ::  if the underlying app has implemented a /dbug/state scry endpoint,
        ::  use that vase in place of +on-save's.
        ::
        =/  result=(each ^vase tang)
          (mule |.(q:(need (need (on-peek:ag /x/dbug/state)))))
        ?:(?=(%& -.result) p.result on-save:ag)
      %+  slap
        (slop state !>([bowl=bowl ..zuse]))
      (ream grab.dbug)
    :: The incoming mark
        %incoming
      =;  =tang
        ?^  tang  tang
        [%leaf "no matching subscriptions"]~
      %+  murn
        %+  sort  ~(tap by sup.bowl)
        |=  [[* a=[=ship =path]] [* b=[=ship =path]]]
        (aor [path ship]:a [path ship]:b)
      |=  [=duct [=ship =path]]
      ^-  (unit tank)
      =;  relevant=?
        ?.  relevant  ~
        `>[path=path from=ship duct=duct]<
      ?:  ?=(~ about.dbug)  &
      ?-  -.about.dbug
        %ship  =(ship ship.about.dbug)
        %path  ?=(^ (find path.about.dbug path))
        %wire  %+  lien  duct
               |=(=wire ?=(^ (find wire.about.dbug wire)))
        %term  !!
      ==
    :: outgoing mark
        %outgoing
      =;  =tang
        ?^  tang  tang
        [%leaf "no matching subscriptions"]~
      %+  murn
        %+  sort  ~(tap by wex.bowl)
        |=  [[[a=wire *] *] [[b=wire *] *]]
        (aor a b)
      |=  [[=wire =ship =term] [acked=? =path]]
      ^-  (unit tank)
      =;  relevant=?
        ?.  relevant  ~
        `>[wire=wire agnt=[ship term] path=path ackd=acked]<
      ?:  ?=(~ about.dbug)  &
      ?-  -.about.dbug
        %ship  =(ship ship.about.dbug)
        %path  ?=(^ (find path.about.dbug path))
        %wire  ?=(^ (find wire.about.dbug wire))
        %term  =(term term.about.dbug)
      ==
    ==
  ::  Scry Interface
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?.  ?=([@ %dbug *] path)
      (on-peek:ag path)
    ?+  path  [~ ~]
       :: `` notation makes [~ ~] units. & is just %.y
      [%u %dbug ~]                 ``noun+!>(&)
      [%x %dbug %state ~]          ``noun+!>(on-save:ag)
      [%x %dbug %subscriptions ~]  ``noun+!>([wex sup]:bowl)
    ==
  :: Defaulted Arm
  ++  on-init
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  on-init:ag
    [cards this]
  :: Defaulted Arm
  ++  on-save   on-save:ag
  :: Defaulted Arm
  ++  on-load
    |=  old-state=vase
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-load:ag old-state)
    [cards this]

  ::  We Can't subscribe, this is all default.
  ++  on-watch
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-watch:ag path)
    [cards this]
  ::  We Can't leave, as cant subscribe
  ++  on-leave
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-leave:ag path)
    [cards this]
  :: No need to handle a subscribe response.
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-agent:ag wire sign)
    [cards this]
  :: No arvo calls, just dealing at the Gall level
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-arvo:ag wire sign-arvo)
    [cards this]
  ::
  ++  on-fail
    |=  [=term =tang]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-fail:ag term tang)
    [cards this]
  --
--