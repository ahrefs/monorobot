  $ dune exec text_cleanup -- log | sed 's/\$/%/g' | sed 's/\\n/\n/g' 
  "~~~ Preparing working directory
  % cd /home/user/mydir
  Pseudo-terminal will not be allocated because stdin is not a terminal.
  
  # Host \"github.com\" already in list of known hosts at \"/home/user/.ssh/known_hosts\"
  % git clean -ffxdq
  % git fetch -v --prune -- origin 404671053ebacf0245dc5b60566eb85e19465860
  From github.com:Account/repo
  
   * branch            404671053ebacf0245dc5b60566eb85e19465860 -> FETCH_HEAD
  
  % git checkout -f 404671053ebacf0245dc5b60566eb85e19465860
  HEAD is now at 4046710 test change
  
  # Cleaning again to catch any post-checkout changes
  % git clean -ffxdq
  # Checking to see if git commit information needs to be sent to Buildkite...
  % buildkite-agent meta-data exists buildkite:git:commit
  # Git commit information has already been sent to Buildkite
  ~~~ Running commands
  % echo \"Test the rocket\"
  dune test
  exit 0
  
  Test the rocket
  
  Done: 60% (3/5, 2 left) (jobs: 0)
  
  Done: 55% (5/9, 4 left) (jobs: 0)
  
  Done: 66% (6/9, 3 left) (jobs: 1)
  
  Done: 77% (7/9, 2 left) (jobs: 1)
  
  Done: 90% (9/10, 1 left) (jobs: 1)
  
  File \"tests/test.t\", line 1, characters 0-0:
  
  /usr/bin/git --no-pager diff --no-index --color=always -u _build/.sandbox/b2994389d75f031a56c7ed865141cfe7/default/tests/test.t _build/.sandbox/b2994389d75f031a56c7ed865141cfe7/default/tests/test.t.corrected
  
  diff --git a/_build/.sandbox/b2994389d75f031a56c7ed865141cfe7/default/tests/test.t b/_build/.sandbox/b2994389d75f031a56c7ed865141cfe7/default/tests/test.t.corrected
  
  index 88404ca..a9e2880 100644
  
  --- a/_build/.sandbox/b2994389d75f031a56c7ed865141cfe7/default/tests/test.t
  
  +++ b/_build/.sandbox/b2994389d75f031a56c7ed865141cfe7/default/tests/test.t.corrected
  
  @@ -1,2 +1,2 @@
  
     % dune exec backend
  
  -  aaabvaaa
  
  +  aaaa
  
  Done: 90% (9/10, 1 left) (jobs: 1)
  
  ^^^ +++
  \240\159\154\168 Error: The command exited with status 1
  ^^^ +++
  user command error: exit status 1
  "
