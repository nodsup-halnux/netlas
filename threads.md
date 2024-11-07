## Threads:

We First start by realizing that threads are a necessary **pain-in-the-ass**, that allow us to perform complex IO and segment unsafe computations for our Gall Apps.

I lack the functional programming experience with notions such as Monadic functions and IO, so this will be a bottom-up grind of a notes/tutorial document.

For the purposes of these notes, we will ignore Khan as it involves computations and threads outside of Vere (in the OS userspace).  All computations we consider will be within Arvo (minus fetching JSON or using some Eyre/Iris functionality). Yes, the new %spider references %khan in its implementation - we will try to keep the %khan details to a minimum.

Its time to begin:

## Goals:

By the end of this document, we should:

1) Understand threads at a deep level. By this, I mean knowing how Spider works, a deep grasp of all relevant structures and what **exactly** micgal is doing, with all syntactic sugar removed.

2) Launch basic threads on Dojo.

3) Launch medium complexity threads on Dojo - ones that spawn a few children, or children of children for modest computational work.

4) Launching a thread from a Gall App, with:

    i) A JSON thread, where we parse with de-json (essential).

    ii) An internal thread, that launches a few children and consolidates results.


##  What are Threads:

###  Uses:
- Threads are used to allow for Complex IO that may or may not return an output.
    - Recall that a Gall App acts like a transition function, with a deterministic output.
    - Putting non-blocking/async requests into a Gall Arm can easily invalidate this requirement. So we use threads, which return a %fail if anything goes wrong, allowing Gall to transition gracefully.
- For doing batteries of tests.

### Basic Facts:
- Threads live in the /ted folder.
- Calling a Thread in Dojo:  `-deskname!threadname`. In `%base` you can just do `-threadname`.
- Thread management is done by %spider, which lives in /app space.
- Libraries+Files involved:
    - **spider.hoon:** high-level management, and imports libstrand.hoon
    - **strand.hoon:** **Empty!** Main functions and definitions for strand are in lull.hoon, and in spider.hoon, referenced in the `++rand` arm.  These are in the global namespace, so imported.
    - **strandio.hoon:**  Contains lots of high-level gates that package common thread/strand operations. Useful.
- A thread contains a number of strands, which are the basic child unit of compilation.

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
- Here we register a jet to start the thread. There is lots of functionality to check if the thread is erroneous. We also invoke the `eval` function, which is not mentioned in modern documentation.
- A thread structure here is an `+$-(vase _*form:(strand ,vase))`.  In other words, it is a gate that pins a vase as a sample, and has a body that produces the form of a strand.  See the sample image below:

<<strand image>>


- Note that the thread will always return a vase. Strands (later) can be specialized to other types (@ud, maps, etc).

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

- This gate interacts with Clay, to actually build a thread file (that would be found in /ted). %handle-build is also a registered jet (strangely? why?).

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

- Firstly, note that `start-thread` no longer exists. We have two types of threads now: *compilable, and inline threads.*  See the relevent code:

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
- In the first gate, most of the work is handed off to prep-thread.  In the second, we see a %khan reference added for the inline type.  Lets look at prep thread:

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
- Prep thread processes all of the input arguments and paths needed to run the thread.  There is some corner case checking, and error checking. The important part is at the end. **A thread is dispatched using a %pass card, with a `/build/t-id` wire, and an %arvo-note that is addressed to %clay.  The thread is started in clay!**

- Clay compiles files and does state management, but it doesn't say anywhere that it runs compiled files.

- Also for reference, lets take a look at the Gall Arm tags that track different system pokes and messages:

```


```

- So how are our threads actually run (???)

- Note that `handle-build` and `thread-fail are **largely unchanged**.

### Understanding Monadic IO:

| **Monadic IO Concept** | **Description** | **Generic Pseudocode**               | **JavaScript Promise Equivalent**    |
|------------------------|-----------------|--------------------------------------|--------------------------------------|
| **Monadic Type (IO)**  | A structure representing a computation that will eventually produce a value, but has not done so yet. | `IO<T>` represents a deferred value of type `T` | `Promise<T>` represents a deferred value of type `T` |
| **`pure`**             | Wraps a regular value into the monadic structure, representing it as a computation that produces that value. | `IO.pure(value)`                    | `Promise.resolve(value)`              |
| **`bind`**             | Chains computations, taking a monadic value and a function that transforms the contained value, producing a new monadic value. | `ioValue.bind(fn)`                  | `promise.then(fn)`                    |
| **Sequential Execution** | Runs each operation in sequence, allowing the result of each to pass to the next. | `ioA.bind(ioB).bind(ioC)`           | `promiseA.then(promiseB).then(promiseC)` |
| **Side Effect Isolation** | Encapsulates side effects (e.g., IO actions) in a monadic context, allowing controlled execution flow. | `IO.perform(effect)`                | `Promise` chains keep effects isolated in `.then` callbacks |
| **Error Handling**     | Catches and handles errors that occur within the monadic chain. | `ioValue.catch(handleError)`        | `promise.catch(handleError)`          |


## Strands:

- Although Spider and the idea of threads are how computations are handled, much of the low-level work in chaining together a computation is done with **Strands**. These have their own structures and non-trivial functionality, which we will go into.

- Once again, all strand definitions are in the **rand arm of lull.hoon.**

- Conceptually, a strand is a function of strand-input to strand-output.

- Strands have four important arms:
    - **form**:  Which is the mold of a strand.
    - **pure**:  Produces an instance of a strand that just returns a value.
    - **bind**: Produces a monadic bind (read: kind of Promise), that returns an output, or branches on an error.
    - **eval**: This is more behind the scenes arm. It works with take to get inputs, and progress the strand's computation.

### Structures:

```
    $+  input
    $%  [%poke =cage]
        [%sign =wire =sign-arvo]
        [%agent =wire =sign:agent:gall]
        [%watch =path]
    ==
```

- An `input` is just a tagged cell, that has Gall agent head tags. For the poke, agent, arvo and watch arms, respectively.  The inputs to the strand are just kept in cages, or other structures.

```
  +$  strand-input
    $+  strand-input
    [=bowl in=(unit input)]

```

- Strand are given our (unit input), and the usual Gall Bowl to do work with.

### Gates and Calls:

```
  ++  strand-output-raw
    |*  a=mold
    $+  strand-output-raw
    $~  [~ %done *a]
    $:  cards=(list card)
        $=  next
        $%  [%wait ~]
            [%skip ~]
            [%cont self=(strand-form-raw a)]
            [%fail err=error]
            [%done value=a]
        ==
    ==
```

- A wet gate, with a mold input. 
- We redefine a strand-output-raw structure inside the gate.
- $~ defines a custom default value, with a structured tail. Our default value is `[~ %done *a]`, where our mold is bunted for an instance.
- The tail is a cell, with the first cell typed for cards. The inner tail is tagged with next, and contains signal cells
- Notice the **non-triviality** of %cont: we reference strand-form-raw to begin again.


```
  ++  strand-form-raw
    |*  a=mold
    $+  strand-form-raw
    $-(strand-input (strand-output-raw a))

```

- Note the appearance of a wet gate here - one that is taking in a structure (mold), and can take in and return various types.
- We see the gate name repated in a $+ structure definition. Here, strand-form raw is being specialized.
- The following line `$-(strand-input (strand-output-raw a))` can be interpreted as follows:
    - $- is a wide form of %-, we have a gate with one input and a body.
    - strand input is our pinned sample, which is a bowl and some input tag + binary storage stucture (like cages)
    -  the code body of our gate is another gate which is a strand-output-raw and our mold a that was our input.
    - So we have a mold gate, that has a specific structured input, and produces a specific structured output (strand-output-raw). This is not an instance!


####  The Strand Gate:

- this is a wet gate with a large core nested inside. It is more complex in its struturing.
- a is again a type of mold we pin as the sample head of the gate.

```
++  output  $+(output (strand-output-raw a))
```

- First we have a tall form of $+, which is used for pretty printing. The alias is output, and our inner %- produces a 2-cell (as seen above).  Note that we have wrapped out structure and bound a name, it can still be used, but has pretty printer information bound to it (for debugging, likely)

```
    ++  form  $+(form (strand-form-raw a))
```

- Same explanation as above.

```
    ++  pure
      |=  arg=a
      ^-  form
      |=  strand-input
      [~ %done arg]
```

- Notice a non-wet gate here. Generic arg a, and a return type of form is given as a compiler hint. We return a gate that maps a strand-input to the default output. We don't care about a, its not even used.


```
    ++  bind
      |*  b=mold
      |=  [m-b=(strand-form-raw b) fun=$-(b form)]
      ^-  form
      |=  input=strand-input
      =/  b-res=(strand-output-raw b)
        (m-b input)
      ^-  output
      :-  cards.b-res
      ?-    -.next.b-res
        %wait  [%wait ~]
        %skip  [%skip ~]
        %cont  [%cont ..$(m-b self.next.b-res)]
        %fail  [%fail err.next.b-res]
        %done  [%cont (fun value.next.b-res)]
      ==

```

- Another wet gate, with an input cell consisting of the following:
    - `m-b=(strand-form-raw b)` recall: this is a mold gate, that has a specific structured input, and produces a specific structured output (strand-output-raw). This is not an instance!
    - `fun=$-(b form)` $- normalizes [???]
- we again return something of type form.
- We define a gate that takes a `strand-input`, which is ~ or one of our Gall tagged cells for arms, and a cage/storage structure.
- pinned to the subject is b-res, which is a gate that takes `mold` to `[card next ...]` cell.
- we compute (m-b input) [???] which is necessarily a gate.
- for nested gate, we have an `output`, which is really a `strand-output-raw`
- the code below this then processes the next and signal stack of elements in the output, and returns a cell accordingly.

- **Question:** Which gates call `++ bind`?  Answer: **None in lull.hoon**.  This is an externally facing function.


- `Eval` just repackages `form`, so we won't worry about it here.

- Now we move onto take, where is where the real work is done!  Comments are included in the code.


## References and Footnotes:

[] Urbit Docs: Threads Tutorial (Basics): https://docs.urbit.org/userspace/threads/tutorials/basics

[] Urbit Docs: Thread Examples.

[] Core Academy: ca12: Vanes II, Khan and Lick.

[] Urbit Code Base: Base Desk (manual grep/ctrl+F searches in VS Code).

[] Old Spider Code: https://github.com/urbit/urbit/blob/571649c6313e0f9d906b092b9c532b543e9510e5/pkg/arvo/app/spider.hoon
Note:  You might need to look at historical revisions to see the code.

[] https://chatgpt.com/share/672d011c-94a8-8007-a502-511044acf11d