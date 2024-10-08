:: Our structure file.
/-  *netlas
::  default-agent used to default un-implemented arms.
/+  default-agent, dbug, agentio
::  *NOT* tisfas.  Build an import a file at path. In this
:: instance, it is our simple sail page ~
/=  display  /app/netlas/display

::  Set for our ~?  conditional gates to fire
=/  debugmode  %.y
|%
+$  versioned-state
$%  state-0
==
+$  state-0  netlas-state
::  shorthand to reference card type.
+$  card  card:agent:gall
--
::  Pin the state
=|  state-0  
::  Tis-tar deferred expression. state ref's state-0
::  Which is in the LH slot of our subject (-).
=*  state  -
::  Our sample app starts here (10 arm door).
^-  agent:gall
|_  =bowl:gall
+*  this     .
    default  ~(. (default-agent this %|) bowl)
    io  ~(. agentio bowl)
::
++  on-init
    ~?  debugmode  
        ~&  "on-init"  ":Initializing Netlas"
      ^-  (quip card:agent:gall agent:gall)
      ::  This is an arvo call that binds the path 
      ::  /ttt/display to localhost:8XXX.
      :_  this  
      [(~(arvo pass:io /bind) %e %connect `/'netlas' %netlas)]~
::
++  on-save   !>(state)
++  on-load  
  |=  old=vase
  ^-  (quip card _this)
  ::  Shorthand for: Irregular form of cen-tis
  ::  ` creates a cell with a ~ in the front (no cards)
  `this(state !<(state-0 old))
++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ::  A bar-ket is stuffed in the gate, so as to
    ::  compartmentalize our little HTTP
    ::  server code into the on-poke arm.
    |^ 
        ::Our $-arm
        ^-  (quip card _this)
        ~?  debugmode  ~&  '%ttt on-poke arm hit:'  mark
        :: Unrecognized actions do nothing, instead of crashing.
        ?+  mark
            ::If non mark, then check for pokes:
            =/  act  !<(action vase)
            ?-  -.act
                %testaction  ~&  "testpoke received"  `this
            ==  ::  End of ?-
::
            ::The only other possible mark - a browser request
            %handle-http-request
                (handle-http !<([@ta inbound-request:eyre] vase))
        ==  ::End ?+  
    ::End $-arm
::
        ++  handle-http 
        :: Eyre takes our browser poke, and passes 
        :: a complex request structure.
        |=  [rid=@ta req=inbound-request:eyre]
            ^-  (quip card _this)
            :: First check to see if user auth'ed.
            ?.  authenticated.req
                :_  this
                %^    give-http 
                    rid 
                [307 ['Location' '/~/login?redirect='] ~] 
                ~
::
                :: Check if we have a non-GET req.
                ?+  method.request.req
                    :_  this
                    %^      give-http
                        rid
                        :-    405
                        :~    ['Content-Type' 'text/html']
                            ['Content-Length' '31']
                            ['Allow' 'GET, POST']
                            ==
                    (some (as-octs:mimes:html '<h1>405 - Forbidden Req</h1>'))
::
                    %'GET'
                    ~?  debugmode  
                    ~&  "%handle-http-req:"  
                        " GET Request received"
                    =/  appstate  !<(netlas-state on-save)  
                    :: The board can be ~, but the gamestate itself cannot.
                    :: If this happens we have a serious error.
                    =/  ourpage  (make-200 rid (display bowl appstate))
                        :_  this  ourpage
                        
                == ::End ?+
        ::  End handle-http gate
::
        ++  make-200
        |=  [rid=@ta dat=octs]
        ^-  (list card)
            %^    give-http
                rid
            :-  200
            :~  ['Content-Type' 'text/html']
                ['Content-Length' (crip ((d-co:co 1) p.dat))]
            ==
            [~ dat]
::
        :: Used to generate a non-200 series response.
        :: Just a stack of (complex) cards returned.
        ++  give-http
        |=  [rid=@ta hed=response-header:http dat=(unit octs)]
            ^-  (list card)
            :~  [%give %fact ~[/http-response/[rid]] %http-response-header !>(hed)]
                [%give %fact ~[/http-response/[rid]] %http-response-data !>(dat)]
                [%give %kick ~[/http-response/[rid]] ~]
            ==
    -- ::End |^
++  on-peek  on-peek:default
++  on-watch
  |=  =path
    ^-  (quip card _this)
    ?~  path  !!
      ~?  debugmode  
        ~&  "on-watch netlas path is:"  i.path
      ::  Our cards generated by the HTTP arm get sent 
      ::  to Eyre,  but also our on-watch arm.  This is 
      ::  done to suppress behaviour - which confuses 
      ::  Eyre and gives us a 500 Error.
      ?:  &(=(our.bowl src.bowl) ?=([%http-response *] path))
      `this
      `this
::    |=  =path  
::        ^-  (quip card _this)
::        ?~  path  ~&  "Warning: on-watch path is ~"  !!
::            ~&  "on-watch %netlas app FE subscribe... "  ~&  "path is:"  ~&  path
::            ::give a fact back - let FE know it sub was OK.
::            :_  this  [%give %fact ~[path] %netlas-update !>(`update`[%testupdate ack=1])]~
++  on-arvo
    |=  [=wire =sign-arvo]
        ^-  (quip card _this)
    ?:  ?&(=([%bind ~] wire) =(%eyre -.sign-arvo))
        ~?  debugmode  
        ~&  "wire="  ~&  wire  ~&  ",sign-arvo"  sign-arvo
    ~&  'Arvo bind confirmed. Hosted at localhost:<yourport#>/netlas/display.'
        `this
        ::  %.n
        ~&  '(!) Error: Arvo rejected frontend binding.'
        `this
++  on-leave  on-leave:default
++  on-agent  on-agent:default
++  on-fail   on-fail:default
--