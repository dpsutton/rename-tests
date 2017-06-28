# rename-tests

This is the code that allowed for the pull request on cider-nrepl to [rename tests](https://github.com/clojure-emacs/cider-nrepl/pull/366).

The gist of it was that there were tests that were of the form `test-fn`, `fn`, `fn-test` and to normalize these to `fn-tests`. This would open up all files in cider-nrepl, try to locate the tests, analyze whether the misnaming fit into a scheme, and then apply a fix.
