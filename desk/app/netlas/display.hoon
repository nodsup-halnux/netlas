::  Import strucutre file.
/-  *netlas
:: Our Sail page is a gate that renders HTML that we
::  serve to our localhost website.
::  Bowl and gamestate supplied to gate. Bowl isn't
::  currently used, but kept for generality.
|=  [bol=bowl:gall nstate=netlas-state]
  |^  ^-  octs
::  The nested gate calls below produce a format chain
::  that take us from xml manx structures, to outputted
::  html.
        %-  as-octs:mimes:html
      %-  crip
    %-  en-xml:html
  ^-  manx

::  In the Sail guide, data formatting is just called
::  inside the sail elements, using ;+ and ;*.  Here,
::  data computation is separated for simplicity.
::  Notes about Sail:  Use a mictar rune for each 
::  new %+ turn and sub-elements generated. Using one 
::  mictar with multiple levels of loop and/or 
::  sub-elements leads to ruin.  When adding id and 
::  css attributes, its tag#css.class in that order!
::
;html
  ;head
    ;title: tictactoe
    ;meta(charset "utf-8");
    ;style
      ;+  ;/  style
    ==  ::style
    ;script(type "module")
      :: oust removes sig from tape conversion.
      ;+  ;/  (script (oust [0 1] <our.bol>))
    ==
  ==  ::head
  ;body
    ;h1: %netlas - Urbit Network Visualizer:
    ;h2: See the map below...  
    ;br;
    ;br;  
    ;p: ~ Place map here ~
  == ::body
== ::html

++  style
  ^~
  %-  trip
'''
    body {
      background-color:#333333;
      color:#c6a615;
      text-align: center;
      font-weight:bold;
    }
    h1 {
      font-size: 56pt;
    }
    h2 {
      font-size: 30pt;
      margin-top: 8px;
      margin-bottom:8px;
    }
    div  {  
      font-size: 16pt;
    }
'''
::  JS Comments are ugly, so explanation placed here.
::  We don't have a session.js as we don't use npm build 
::  with urbithttp-api. So there is no window.ship 
::  variable in session.  api.ship must be interpolated, 
::  and pulled from our bowl.
::
::  No authentication needed, as page is inside our app
::  (simple constructor used).
::
::  api.subscribe will request to Eyre.  
::  Will hit app's the ++on-watch arm.
::
::  function check_callback handles update from app
::
::  Curly braces are escaped \{ as compiler interprets
::  these in a tape as starting an interpolation site.
::
::  In closing, the unpleasant look of JS
::  slammed in a gate beats dealing a bloated 
::  node_modules folder, and minified code.

++  script
  |=  our-bowl=tape
  ^-  tape 
  """
  import urbitHttpApi from 'https://cdn.skypack.dev/@urbit/http-api';

  const api = new urbitHttpApi('', '', 'netlas');
  api.ship = '{our-bowl}';

  var subID = api.subscribe(\{
    app: 'netlas',
    path: '/netlas-sub',
    event: check_callback,
    err:  check_error
  })

  function check_error(err) \{
    console.log(err);
  }

  function check_callback(upd) \{
    console.log(upd);
    if ('testupdate' in upd) \{
      console.log('Eyre Channel Subscription is: ' + api.uid + ', path: /netlas-sub' );
    }
  }

  console.log('Sail page JS successfully loaded.');
  """
--