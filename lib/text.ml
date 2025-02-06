let log1 =
  ( "log1.txt",
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam bibendum sodales erat eu ornare. Suspendisse sed \
     dui laoreet, pellentesque metus nec, scelerisque urna. Phasellus ut erat leo. In maximus congue ante, vel \
     tincidunt dolor commodo nec. Proin pulvinar risus nisl, a posuere risus tincidunt sit amet. Mauris ut elit \
     fermentum, maximus urna eu, sagittis lorem. Maecenas faucibus tellus vitae lectus facilisis, eu fermentum risus \
     accumsan. Pellentesque ac elit posuere, gravida urna vel, placerat massa. Sed ac est felis. Vivamus luctus est \
     nec nunc lacinia finibus.\n\n\
     Vestibulum finibus mauris augue, vel tempus magna sollicitudin quis. Ut sem dui, egestas nec laoreet et, eleifend \
     eget eros. Interdum et malesuada fames ac ante ipsum primis in faucibus. Praesent pulvinar, neque ut lobortis \
     vehicula, diam augue suscipit purus, ac luctus ipsum tortor sodales tellus. Maecenas pretium dui et nibh commodo, \
     tristique sollicitudin quam molestie. Maecenas ligula mi, iaculis sit amet lectus vitae, luctus convallis magna. \
     Nam ultrices est nisi, eget posuere lorem vestibulum sodales. Donec vel felis vel felis luctus maximus eu id \
     eros.\n\n\
     Nam dui nunc, malesuada non metus cursus, lobortis aliquet massa. Etiam ut molestie magna, eu varius nibh. \
     Curabitur felis arcu, varius ac justo sed, auctor pharetra nulla. Donec quis lectus interdum, faucibus metus non, \
     sollicitudin nulla. Pellentesque sed posuere erat. Curabitur non porta libero. Nullam vitae eleifend eros. Fusce \
     in euismod tellus, ac sollicitudin ante. Nulla non ultricies quam. Vestibulum quis condimentum lectus. Aliquam a \
     eros turpis. Fusce suscipit iaculis erat, vel ultrices lorem tempor ut. Nullam non consequat felis.\n\n\
     Suspendisse potenti. Nullam purus mi, dictum in diam vel, bibendum tristique libero. Integer sit amet accumsan \
     lorem. Nulla facilisi. Sed feugiat magna ac ligula commodo, sit amet blandit leo tincidunt. Praesent facilisis, \
     diam eu tristique sagittis, risus risus mattis purus, eget aliquam lorem tellus non metus. Praesent rhoncus ipsum \
     non urna vulputate, nec fringilla lacus condimentum. Aenean tincidunt ultrices ante. Integer sed placerat purus, \
     non mollis lectus. Vestibulum luctus metus sit amet massa commodo suscipit.\n\n\
     Fusce dui dolor, ornare eu varius id, tristique in purus. Cras a blandit libero. Suspendisse vel dui convallis, \
     sodales ex sit amet, condimentum eros. Praesent id ligula quam. Etiam gravida velit id erat finibus, eu imperdiet \
     lectus ultricies. Aliquam erat volutpat. Donec quis sapien nisl. Praesent leo odio, efficitur et mi eu, vulputate \
     mattis urna.\n\n\
     Generated 5 paragraphs, 380 words, 2562 bytes of Lorem Ipsum" )

let log2 =
  ( "log2.txt",
    "_bk;t=1738172391664~~~ Running global environment hook\n\
     _bk;t=1738172391665[90m$[0m /etc/buildkite-agent/hooks/environment\n\
     _bk;t=1738172391711~~~ Preparing working directory\n\
     _bk;t=1738172391712[90m$[0m cd /home/user/builds/buildbot104/ahrefs/monorepo\n\
     _bk;t=1738172391780[90m# Host \"git.ahrefs.com\" already in list of known hosts at \
     \"/home/user/.ssh/known_hosts\"[0m\n\
     _bk;t=1738172391782[90m$[0m git submodule foreach --recursive \"git clean -ffdx -e _build -e _opam -e .opam \
     -e node_modules\"\n\
     _bk;t=1738172391823Entering 'backend/cdr/cachetools'\n\
     _bk;t=1738172391833[90m$[0m git clean -ffdx -e _build -e _opam -e .opam -e node_modules\n\
     _bk;t=1738172391979Removing typescript\n\
     _bk;t=1738172391980[90m$[0m git fetch -v --prune -- origin 61a85c041436cfd2fbc247978bf212383842fe69\n\
     _bk;t=1738172398066From git.ahrefs.com:ahrefs/monorepo\n\
     _bk;t=1738172398066 * branch                    61a85c041436cfd2fbc247978bf212383842fe69 -> FETCH_HEAD\n\
     _bk;t=1738172398099[90m$[0m git checkout -f 61a85c041436cfd2fbc247978bf212383842fe69\n\
     _bk;t=1738172399552Previous HEAD position was 19fcfa0cfaf infra: ch: mv `core::clickhouse::dashboard` to `devsg`\n\
     _bk;t=1738172399554HEAD is now at 61a85c04143 Revert \"infra: add aa_wc_admin to publish\"\n\
     _bk;t=1738172399569[90m# Git submodules detected[0m\n\
     _bk;t=1738172399569[90m$[0m git submodule sync --recursive\n\
     _bk;t=1738172399611Synchronizing submodule url for 'backend/cdr/cachetools'\n\
     _bk;t=1738172399618[90m$[0m git submodule update --init --recursive --force\n\
     _bk;t=1738172399669Submodule path 'backend/cdr/cachetools': checked out \
     'c9700a0b29a8d220c1afdb867e9eed9357cd3351'\n\
     _bk;t=1738172399674[90m$[0m git submodule foreach --recursive \"git reset --hard\"\n\
     _bk;t=1738172399713Entering 'backend/cdr/cachetools'\n\
     _bk;t=1738172399718HEAD is now at c9700a0 use long for internal counters\n\
     _bk;t=1738172399723[90m# Cleaning again to catch any post-checkout changes[0m\n\
     _bk;t=1738172399723[90m$[0m git clean -ffdx -e _build -e _opam -e .opam -e node_modules\n\
     _bk;t=1738172399865[90m$[0m git submodule foreach --recursive \"git clean -ffdx -e _build -e _opam -e .opam \
     -e node_modules\"\n\
     _bk;t=1738172399908Entering 'backend/cdr/cachetools'\n\
     _bk;t=1738172399917[90m# Checking to see if git commit information needs to be sent to Buildkite...[0m\n\
     _bk;t=1738172399917[90m$[0m buildkite-agent meta-data exists buildkite:git:commit\n\
     _bk;t=1738172400612[90m# Sending Git commit information back to Buildkite[0m\n\
     _bk;t=1738172400617[90m$[0m buildkite-agent meta-data set buildkite:git:commit < /dev/stdin\n\
     _bk;t=1738172400646[38;5;48m2025-01-29 17:40:00 INFO  [0m [0mReading meta-data value from STDIN[0m\n\
     _bk;t=1738172401344~~~ Running commands\n\
     _bk;t=1738172401344[90m$[0m /home/user/buildkite_pipeline.sh | buildkite-agent pipeline upload\n\
     _bk;t=1738172402024[38;5;48m2025-01-29 17:40:02 INFO  [0m [0mReading pipeline config from STDIN[0m\n\
     _bk;t=1738172402027[38;5;48m2025-01-29 17:40:02 INFO  [0m [0mUpdating BUILDKITE_COMMIT to \
     \"61a85c041436cfd2fbc247978bf212383842fe69\"[0m\n\
     _bk;t=1738172404182[38;5;48m2025-01-29 17:40:04 INFO  [0m [0mSuccessfully uploaded and parsed pipeline \
     config[0m\n" )
