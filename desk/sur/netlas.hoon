|%
  +$  action
    $%  [%testaction do=@ud]
    ==
  ::these are respones sent by BE to FE client.
  +$  update  
    ::  Doing a GET request in Browser will always load 
    ::  the entire page.
    $%  [%testupdate ack=@ud]
    ==
  +$  netlas-state  
    $:  
      %0 
      int=@ud
    ==
--