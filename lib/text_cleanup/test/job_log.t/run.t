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
  $ dune exec text_cleanup -- log2 | sed 's/\$/%/g' | sed 's/\\n/\n/g'
  "~~~ Preparing working directory
  
  % cd /opt/homebrew/var/buildkite-agent/builds/Joses-MacBook-Pro-local-5/monorobot-tests/buildkite-tests
  
  Pseudo-terminal will not be allocated because stdin is not a terminal.
  
  
  # Host \"github.com\" already in list of known hosts at \"/Users/ze/.ssh/known_hosts\"
  
  % git clean -ffxdq
  
  % git fetch -v --prune -- origin 47033fe29b3aeb78f118a84a7512e28cac504901
  
  remote: Enumerating objects: 612, done.
  
  
  remote: Counting objects:   1% (1/81)
  remote: Counting objects:   2% (2/81)
  remote: Counting objects:   3% (3/81)
  remote: Counting objects:   4% (4/81)
  remote: Counting objects:   6% (5/81)
  remote: Counting objects:   7% (6/81)
  remote: Counting objects:   8% (7/81)
  remote: Counting objects:   9% (8/81)
  remote: Counting objects:  11% (9/81)
  remote: Counting objects:  12% (10/81)
  remote: Counting objects:  13% (11/81)
  remote: Counting objects:  14% (12/81)
  remote: Counting objects:  16% (13/81)
  remote: Counting objects:  17% (14/81)
  remote: Counting objects:  18% (15/81)
  remote: Counting objects:  19% (16/81)
  remote: Counting objects:  20% (17/81)
  remote: Counting objects:  22% (18/81)
  remote: Counting objects:  23% (19/81)
  remote: Counting objects:  24% (20/81)
  remote: Counting objects:  25% (21/81)
  remote: Counting objects:  27% (22/81)
  remote: Counting objects:  28% (23/81)
  remote: Counting objects:  29% (24/81)
  remote: Counting objects:  30% (25/81)
  remote: Counting objects:  32% (26/81)
  remote: Counting objects:  33% (27/81)
  remote: Counting objects:  34% (28/81)
  remote: Counting objects:  35% (29/81)
  remote: Counting objects:  37% (30/81)
  remote: Counting objects:  38% (31/81)
  remote: Counting objects:  39% (32/81)
  remote: Counting objects:  40% (33/81)
  remote: Counting objects:  41% (34/81)
  remote: Counting objects:  43% (35/81)
  remote: Counting objects:  44% (36/81)
  remote: Counting objects:  45% (37/81)
  remote: Counting objects:  46% (38/81)
  remote: Counting objects:  48% (39/81)
  remote: Counting objects:  49% (40/81)
  remote: Counting objects:  50% (41/81)
  remote: Counting objects:  51% (42/81)
  remote: Counting objects:  53% (43/81)
  remote: Counting objects:  54% (44/81)
  remote: Counting objects:  55% (45/81)
  remote: Counting objects:  56% (46/81)
  remote: Counting objects:  58% (47/81)
  remote: Counting objects:  59% (48/81)
  remote: Counting objects:  60% (49/81)
  remote: Counting objects:  61% (50/81)
  remote: Counting objects:  62% (51/81)
  remote: Counting objects:  64% (52/81)
  remote: Counting objects:  65% (53/81)
  remote: Counting objects:  66% (54/81)
  remote: Counting objects:  67% (55/81)
  remote: Counting objects:  69% (56/81)
  remote: Counting objects:  70% (57/81)
  remote: Counting objects:  71% (58/81)
  remote: Counting objects:  72% (59/81)
  remote: Counting objects:  74% (60/81)
  remote: Counting objects:  75% (61/81)
  remote: Counting objects:  76% (62/81)
  remote: Counting objects:  77% (63/81)
  remote: Counting objects:  79% (64/81)
  remote: Counting objects:  80% (65/81)
  remote: Counting objects:  81% (66/81)
  remote: Counting objects:  82% (67/81)
  remote: Counting objects:  83% (68/81)
  remote: Counting objects:  85% (69/81)
  remote: Counting objects:  86% (70/81)
  remote: Counting objects:  87% (71/81)
  remote: Counting objects:  88% (72/81)
  remote: Counting objects:  90% (73/81)
  remote: Counting objects:  91% (74/81)
  remote: Counting objects:  92% (75/81)
  remote: Counting objects:  93% (76/81)
  remote: Counting objects:  95% (77/81)
  remote: Counting objects:  96% (78/81)
  remote: Counting objects:  97% (79/81)
  remote: Counting objects:  98% (80/81)
  remote: Counting objects: 100% (81/81)
  remote: Counting objects: 100% (81/81), done.
  
  
  remote: Compressing objects:   3% (1/30)
  remote: Compressing objects:   6% (2/30)
  remote: Compressing objects:  10% (3/30)
  remote: Compressing objects:  13% (4/30)
  remote: Compressing objects:  16% (5/30)
  remote: Compressing objects:  20% (6/30)
  remote: Compressing objects:  23% (7/30)
  remote: Compressing objects:  26% (8/30)
  remote: Compressing objects:  30% (9/30)
  remote: Compressing objects:  33% (10/30)
  remote: Compressing objects:  36% (11/30)
  remote: Compressing objects:  40% (12/30)
  remote: Compressing objects:  43% (13/30)
  remote: Compressing objects:  46% (14/30)
  remote: Compressing objects:  50% (15/30)
  remote: Compressing objects:  53% (16/30)
  remote: Compressing objects:  56% (17/30)
  remote: Compressing objects:  60% (18/30)
  remote: Compressing objects:  63% (19/30)
  remote: Compressing objects:  66% (20/30)
  remote: Compressing objects:  70% (21/30)
  remote: Compressing objects:  73% (22/30)
  remote: Compressing objects:  76% (23/30)
  remote: Compressing objects:  80% (24/30)
  remote: Compressing objects:  83% (25/30)
  remote: Compressing objects:  86% (26/30)
  remote: Compressing objects:  90% (27/30)
  remote: Compressing objects:  93% (28/30)
  remote: Compressing objects:  96% (29/30)
  remote: Compressing objects: 100% (30/30)
  remote: Compressing objects: 100% (30/30), done.
  
  
  Receiving objects:   0% (1/612)
  Receiving objects:   1% (7/612)
  Receiving objects:   2% (13/612)
  Receiving objects:   3% (19/612)
  Receiving objects:   4% (25/612)
  Receiving objects:   5% (31/612)
  Receiving objects:   6% (37/612)
  Receiving objects:   7% (43/612)
  Receiving objects:   8% (49/612)
  Receiving objects:   9% (56/612)
  Receiving objects:  10% (62/612)
  Receiving objects:  11% (68/612)
  Receiving objects:  12% (74/612)
  Receiving objects:  13% (80/612)
  Receiving objects:  14% (86/612)
  Receiving objects:  15% (92/612)
  Receiving objects:  16% (98/612)
  Receiving objects:  17% (105/612)
  Receiving objects:  18% (111/612)
  Receiving objects:  19% (117/612)
  Receiving objects:  20% (123/612)
  Receiving objects:  21% (129/612)
  Receiving objects:  22% (135/612)
  Receiving objects:  23% (141/612)
  Receiving objects:  24% (147/612)
  Receiving objects:  25% (153/612)
  Receiving objects:  26% (160/612)
  Receiving objects:  27% (166/612)
  Receiving objects:  28% (172/612)
  Receiving objects:  29% (178/612)
  Receiving objects:  30% (184/612)
  Receiving objects:  31% (190/612)
  Receiving objects:  32% (196/612)
  Receiving objects:  33% (202/612)
  Receiving objects:  34% (209/612)
  Receiving objects:  35% (215/612)
  Receiving objects:  36% (221/612)
  remote: Total 612 (delta 54), reused 58 (delta 51), pack-reused 531 (from 2)
  
  
  Receiving objects:  37% (227/612)
  Receiving objects:  38% (233/612)
  Receiving objects:  39% (239/612)
  Receiving objects:  40% (245/612)
  Receiving objects:  41% (251/612)
  Receiving objects:  42% (258/612)
  Receiving objects:  43% (264/612)
  Receiving objects:  44% (270/612)
  Receiving objects:  45% (276/612)
  Receiving objects:  46% (282/612)
  Receiving objects:  47% (288/612)
  Receiving objects:  48% (294/612)
  Receiving objects:  49% (300/612)
  Receiving objects:  50% (306/612)
  Receiving objects:  51% (313/612)
  Receiving objects:  52% (319/612)
  Receiving objects:  53% (325/612)
  Receiving objects:  54% (331/612)
  Receiving objects:  55% (337/612)
  Receiving objects:  56% (343/612)
  Receiving objects:  57% (349/612)
  Receiving objects:  58% (355/612)
  Receiving objects:  59% (362/612)
  Receiving objects:  60% (368/612)
  Receiving objects:  61% (374/612)
  Receiving objects:  62% (380/612)
  Receiving objects:  63% (386/612)
  Receiving objects:  64% (392/612)
  Receiving objects:  65% (398/612)
  Receiving objects:  66% (404/612)
  Receiving objects:  67% (411/612)
  Receiving objects:  68% (417/612)
  Receiving objects:  69% (423/612)
  Receiving objects:  70% (429/612)
  Receiving objects:  71% (435/612)
  Receiving objects:  72% (441/612)
  Receiving objects:  73% (447/612)
  Receiving objects:  74% (453/612)
  Receiving objects:  75% (459/612)
  Receiving objects:  76% (466/612)
  Receiving objects:  77% (472/612)
  Receiving objects:  78% (478/612)
  Receiving objects:  79% (484/612)
  Receiving objects:  80% (490/612)
  Receiving objects:  81% (496/612)
  Receiving objects:  82% (502/612)
  Receiving objects:  83% (508/612)
  Receiving objects:  84% (515/612)
  Receiving objects:  85% (521/612)
  Receiving objects:  86% (527/612)
  Receiving objects:  87% (533/612)
  Receiving objects:  88% (539/612)
  Receiving objects:  89% (545/612)
  Receiving objects:  90% (551/612)
  Receiving objects:  91% (557/612)
  Receiving objects:  92% (564/612)
  Receiving objects:  93% (570/612)
  Receiving objects:  94% (576/612)
  Receiving objects:  95% (582/612)
  Receiving objects:  96% (588/612)
  Receiving objects:  97% (594/612)
  Receiving objects:  98% (600/612)
  Receiving objects:  99% (606/612)
  Receiving objects: 100% (612/612)
  Receiving objects: 100% (612/612), 67.95 KiB | 809.00 KiB/s, done.
  
  
  Resolving deltas:   0% (0/236)
  Resolving deltas:   1% (3/236)
  Resolving deltas:   2% (5/236)
  Resolving deltas:   3% (8/236)
  Resolving deltas:   4% (10/236)
  Resolving deltas:   5% (12/236)
  Resolving deltas:   6% (15/236)
  Resolving deltas:   8% (21/236)
  Resolving deltas:   9% (22/236)
  Resolving deltas:  10% (24/236)
  Resolving deltas:  11% (26/236)
  Resolving deltas:  12% (30/236)
  Resolving deltas:  13% (31/236)
  Resolving deltas:  14% (34/236)
  Resolving deltas:  15% (36/236)
  Resolving deltas:  16% (38/236)
  Resolving deltas:  17% (41/236)
  Resolving deltas:  18% (44/236)
  Resolving deltas:  19% (45/236)
  Resolving deltas:  20% (48/236)
  Resolving deltas:  21% (50/236)
  Resolving deltas:  22% (53/236)
  Resolving deltas:  23% (55/236)
  Resolving deltas:  24% (58/236)
  Resolving deltas:  25% (59/236)
  Resolving deltas:  26% (62/236)
  Resolving deltas:  27% (64/236)
  Resolving deltas:  28% (67/236)
  Resolving deltas:  29% (69/236)
  Resolving deltas:  30% (72/236)
  Resolving deltas:  31% (74/236)
  Resolving deltas:  32% (76/236)
  Resolving deltas:  33% (78/236)
  Resolving deltas:  34% (81/236)
  Resolving deltas:  35% (83/236)
  Resolving deltas:  36% (86/236)
  Resolving deltas:  37% (89/236)
  Resolving deltas:  38% (90/236)
  Resolving deltas:  39% (93/236)
  Resolving deltas:  40% (95/236)
  Resolving deltas:  41% (97/236)
  Resolving deltas:  42% (100/236)
  Resolving deltas:  43% (102/236)
  Resolving deltas:  44% (104/236)
  Resolving deltas:  45% (107/236)
  Resolving deltas:  46% (109/236)
  Resolving deltas:  47% (111/236)
  Resolving deltas:  48% (114/236)
  Resolving deltas:  49% (117/236)
  Resolving deltas:  50% (118/236)
  Resolving deltas:  51% (121/236)
  Resolving deltas:  52% (123/236)
  Resolving deltas:  53% (126/236)
  Resolving deltas:  54% (128/236)
  Resolving deltas:  55% (130/236)
  Resolving deltas:  56% (133/236)
  Resolving deltas:  57% (135/236)
  Resolving deltas:  58% (137/236)
  Resolving deltas:  59% (140/236)
  Resolving deltas:  60% (143/236)
  Resolving deltas:  61% (144/236)
  Resolving deltas:  62% (147/236)
  Resolving deltas:  63% (149/236)
  Resolving deltas:  64% (152/236)
  Resolving deltas:  65% (154/236)
  Resolving deltas:  66% (156/236)
  Resolving deltas:  67% (159/236)
  Resolving deltas:  68% (161/236)
  Resolving deltas:  69% (163/236)
  Resolving deltas:  70% (167/236)
  Resolving deltas:  71% (169/236)
  Resolving deltas:  72% (170/236)
  Resolving deltas:  73% (173/236)
  Resolving deltas:  74% (175/236)
  Resolving deltas:  75% (177/236)
  Resolving deltas:  76% (181/236)
  Resolving deltas:  77% (182/236)
  Resolving deltas:  78% (185/236)
  Resolving deltas:  79% (187/236)
  Resolving deltas:  80% (189/236)
  Resolving deltas:  81% (192/236)
  Resolving deltas:  82% (194/236)
  Resolving deltas:  83% (196/236)
  Resolving deltas:  84% (199/236)
  Resolving deltas:  85% (201/236)
  Resolving deltas:  86% (203/236)
  Resolving deltas:  87% (206/236)
  Resolving deltas:  88% (208/236)
  Resolving deltas:  89% (211/236)
  Resolving deltas:  90% (213/236)
  Resolving deltas:  91% (215/236)
  Resolving deltas:  92% (218/236)
  Resolving deltas:  93% (220/236)
  Resolving deltas:  94% (222/236)
  Resolving deltas:  95% (225/236)
  Resolving deltas:  96% (227/236)
  Resolving deltas:  97% (229/236)
  Resolving deltas:  98% (232/236)
  Resolving deltas:  99% (234/236)
  Resolving deltas: 100% (236/236)
  Resolving deltas: 100% (236/236), completed with 7 local objects.
  
  
  From github.com:thatportugueseguy/buildkite-starter
  
  
   * branch            47033fe29b3aeb78f118a84a7512e28cac504901 -> FETCH_HEAD
  
  
  % git checkout -f 47033fe29b3aeb78f118a84a7512e28cac504901
  
  Previous HEAD position was 8147cf8 update config
  
  
  HEAD is now at 47033fe fail 2 & notify
  
  
  # Cleaning again to catch any post-checkout changes
  
  % git clean -ffxdq
  
  # Checking to see if git commit information needs to be sent to Buildkite...
  
  % buildkite-agent meta-data exists buildkite:git:commit
  
  # Git commit information has already been sent to Buildkite
  
  ~~~ Running commands
  
  % echo \"Fail the rocket\"
  
  sleep 30
  
  exit 1
  
  
  
  Fail the rocket
  
  
  ^^^ +++
  
  \240\159\154\168 Error: The command exited with status 1
  
  ^^^ +++
  
  user command error: exit status 1
  
  "
