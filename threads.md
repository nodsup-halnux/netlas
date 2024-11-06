## Threads:

We First start by realizing that threads are a necessary **pain-in-the-ass**, that allow us to perform complex IO and segment unsafe computations for our Gall Apps.

I lack the functional programming experience with notions such as Monadic functions and IO, so this will be a bottom-up grind of a notes/tutorial document.

For the purposes of these notes, we will ignore Khan as it involves computations and threads outside of Vere (in the OS userspace).  All computations we consider will be within Arvo (minus fetching JSON or using some Eyre/Iris functionality).

Its time to begin:

##  What are Threads:

###  Uses:
- Threads are used to allow for Complex IO that may or may not return an output.
    - Recall that a Gall App acts like a transition function, with a deterministic output.
    - Putting non-blocking/async requests into a Gall Arm can easily invalidate this requirement. So we use threads, which return a %fail if anything goes wrong, allowing Gall to transition gracefully.
- For doing batteries of tests.

### Basic Facts:
- Threads live in the /ted folder.
- Calling a Thread in Dojo:  `-deskname!threadname`. In `%base` you can just do `-threadname`.
- Thread management is done by %spider, which lives in app space.
- Libraries+Files involved:
    - spider.hoon: high-level management, and imports libstrand.hoon
    - strand.hoon: **Empty!** Main functions and definitions for strand are in lull.hoon, and in spider.hoon, referenced in the ++  rand arm.  These are in the global namespace, so imported.
    - strandio.hoon:  Contains lots of high-level gates that package common thread/strand operations. Useful.
- A thread contains a number of strands, which are the basic child unit of compilation. Think of Thread as a kind of Process.


### Thread Definitions:

- **Remember:** libstrand=strand, and strand.hoon had its functionality put in lull.hoon, and other places.
- Threads are defined and monitored in spider.hoon.  This is a Gall App, with many state definitions loaded at the top.

#### Definitions in /sur/spider.hoon and state in the App File:

The entire state structure for Spider is as follows:

```
|%
:: shed is khan's rep of a thread.
+$  thread  $-(vase shed:khan)
+$  input   [=tid =cage]
::  thread id
+$  tid     tid:strand
::  all threads have access to bowl stuff.
+$  bowl    bowl:strand
+$  http-error
  $?  %bad-request   :: 400
      %forbidden     :: 403
      %nonexistent   :: 404
      %offline       :: 504
  ==
::  The file arg is the name of the thread file in /ted
::  This structure is used when we launch from dojo.

+$  start-args
  $:  parent=(unit tid)
      use=(unit tid)
      =beak
      file=term
      =vase
  ==
::  For Inline threads: launched from Gall or other threads.
+$  inline-args
  $:  parent=(unit tid)
      use=(unit tid)
      =beak
      =shed:khan
  ==
--
```

Spider has a state that manages all current threads, as seen below:

```
+$  clean-slate
  $:  %7
      starting=(map yarn [=trying =vase])
      running=(list yarn)
      tid=(map tid yarn)
      serving=(map tid [(unit [rid=@ta take=?(%json %noun)]) =mark =desk])
      scrying=(jug tid [wire ship path])
  ==
```

For reference, a yarn is a `(list tid)`.

### How the Spider App works:

- Spider is a Gall App, wiht 10 arms and a lot of state definitions, listed above.
- Because it deals with strands, it imports libstrand structures.  These are in the global namespace (lull.hoon)
- Spider has been greatly revamped, to utilize Khan to do the work of starting and managing threads. It is more of a wrapper and a shell of what it once was.  
- For illustrative purposes, lets walk through what the old Spider used to do:

####  Old Spider: Starting a thread[]:

- Old %Spider resembles new %spider in its Gall Arms.  There are lots of @tas tags to track and respond to different Arvo cards, and state changes.
- What is different, is all the thread gates are implemented in the file. Lets look at a few of them:

```
++  start-thread
  ~/  %start-thread
  :: yarn is a list of tids.
  |=  [=yarn =thread]
  ^-  (quip card ^state)
  =/  =vase  vase:(~(got by starting.state) yarn)
  ?<  (has-yarn running.state yarn)
   =/  m  (strand ,^vase)
  =/  res  (mule |.((thread vase)))
  ?:  ?=(%| -.res)
    (thread-fail-not-running (yarn-to-tid yarn) %false-start p.res)
  =/  =eval-form:eval:m
    (from-form:eval:m p.res)
  =:  starting.state  (~(del by starting.state) yarn)
      running.state   (put-yarn running.state yarn eval-form)
    ==
  (take-input yarn ~)
```
- Here we register a jet to start the thread. There is lots of functionality to check if the thread is erroneous. We also invoke the eval function, which is not mentioned in modern documentation.
- A thread structure here is an `+$-(vase _*form:(strand ,vase))`.  In other words, it is a gate that pins a vase as a sample, and has


```
++  handle-build
  ~/  %handle-build
  |=  [=tid =sign-arvo]
  ^-  (quip card ^state)
  =/  =yarn  (~(got by tid.state) tid)
  =.  starting.state
    (~(jab by starting.state) yarn |=([=trying =vase] [%none vase]))
  ~|  sign+[- +<]:sign-arvo
  ?>  ?=([?(%behn %clay) %writ *] sign-arvo)
  =/  =riot:clay  p.sign-arvo
  ?~  riot
    (thread-fail-not-running tid %build-thread-error *tang)
  ?.  ?=(%vase p.r.u.riot)
    (thread-fail-not-running tid %build-thread-strange >[p q]:u.riot< ~)
  =/  maybe-thread  (mule |.(!<(thread !<(vase q.r.u.riot))))
  ?:  ?=(%| -.maybe-thread)
    (thread-fail-not-running tid %thread-not-thread ~)
  (slam-thread yarn p.maybe-thread)

```

- This gate interacts with clay, to actually build a thread file (that would be found in /ted). %handle-build is also a registered jet (strangely? why?).

```
++  thread-fail
  |=  [=yarn =term =tang]
  ^-  (quip card ^state)
  ::%-  (slog leaf+"strand {<yarn>} failed" leaf+<term> tang)
  =/  =tid  (yarn-to-tid yarn)
  =/  fail-cards  (thread-say-fail tid term tang)
  =^  http-cards  state  (thread-http-fail tid term tang)
  =^  scry-card   state  (cancel-scry tid silent=%.n)
  =^  cards       state  (thread-clean yarn)
  :_  state
  :(weld fail-cards cards http-cards scry-card)

```

- thread-fail shows the card aspect of threading.  Many stacks of internal cards are sent in running different threads. The different stacks can be seen above, which are welded together and all sent out when a thread failure has been detected; various parts of Arvo need to be involved when this happens.

- In summary, old %spider is much more massive. All the work is done in file, and clay/jet registration is involved to actually run the threads.


### New Spider Code:

- this is much more reliant on %khan, to hand things off. It ends up being a smaller file. Lets compare the three gates we showcased above:

- Firstly, note that `start-thread` no longer exists. We have two types of threads now compilable, and inline threads.  See the relevent code:

```
++  handle-start-thread
  ~/  %handle-start-thread
  |=  [parent-tid=(unit tid) use=(unit tid) =beak file=term =vase]
  (prep-thread parent-tid use beak %| file vase)

++  handle-inline-thread
  ~/  %handle-inline-thread
  |=  [parent-tid=(unit tid) use=(unit tid) =beak =shed:khan]
  (prep-thread parent-tid use beak %& shed)
```
- In the first gate, most of the work is handed of to prep-thread.  In the second, we see a khan reference added for the inline type.  Lets look at prep thread:

```
++  prep-thread
  :: use buc rune as a cell is unwieldy.
  |=  $:  parent-tid=(unit tid)  use=(unit tid)  =beak
          source=(each shed:khan [file=term =vase])
      ==
  :: Everything is encoded in a card, but to where?
  ^-  (quip card ^state)
  =/  parent-yarn=yarn
    ?~  parent-tid
      /
    :: Not sig case, produce item from key u.parent-tid
    (~(got by tid.state) u.parent-tid)
  =/  new-tid
    ?^  use
      u.use
    %-  new-thread-id
    ?-  -.source
      ::  What are these short forms again?
      %&  (cat 3 'inline-' q.beak)
      %|  file.p.source
    ==
  ::  A lot of this is to check error states
  =/  =yarn  (snoc parent-yarn new-tid)
  ::
  ?:  (~(has of running.state) yarn)
    ~|  [%already-started yarn]
    !!
  ?:  (~(has by starting.state) yarn)
    ~|  [%already-starting yarn]
    !!
  ::
  =?  serving.state  !(~(has by serving.state) new-tid)
    (~(put by serving.state) new-tid [~ %noun q.beak])
  ::
  =.  tid.state       (~(put by tid.state) new-tid yarn)
  ?-    -.source
      %&  (begin-shed yarn p.source)
      %|
    =.  starting.state  (~(put by starting.state) yarn [%build vase.p.source])
    =/  pax=path
      ~|  no-file-for-thread+file.p.source
      (need (get-fit:clay beak %ted file.p.source))
    :_  state
    :_  ~
    :+  %pass  /build/[new-tid]
    [%arvo %c %warp p.beak q.beak ~ %sing %a r.beak pax]
  ==

```
Prep thread processes all of the input arguments and paths needed to run 
the thread.  There is some corner case checking, and error checking. The important part is at the end. **A thread is dispatched using a %pass card, with a `/build/t-id` wire, and an %arvo-note that is addressed to %clay.  The thread is started in clay!**

- Clay compiles files and does state management, but it doesn't say anywhere that it runs compiled files.

- Also for reference, lets take a look at the Gall Arm tags that track different system pokes and messages:

```


```


- So how are our threads actually run (???)

- Note that `handle-build` and `thread-fail are largely unchanged.

## Strands:

- Although Spider and the idea of threads are how computations are handled, much of the low-level work in chaining together a computation is done with **Strands**. These have their own structures and non-trivial functionality, which we will go into.



## References and Footnotes:

[] Urbit Docs: Threads Tutorial (Basics): https://docs.urbit.org/userspace/threads/tutorials/basics

[] Urbit Docs: Thread Examples.

[] Core Academy: ca12: Vanes II, Khan and Lick.

[] Urbit Code Base: Base Desk (manual grep/ctrl+F searches in VS Code).

[] Old Spider Code: https://github.com/urbit/urbit/blob/571649c6313e0f9d906b092b9c532b543e9510e5/pkg/arvo/app/spider.hoon
Note:  You might need to look at historical revisions to see the code.