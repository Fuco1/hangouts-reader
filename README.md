# hangouts-reader

Little CLI utility for parsing Google Hanougts Takeout json data.

# Usage

Build with `stack install` then pipe the Hangouts file into the
compiled executable:

``` shell
cat Hangouts.json  | hangouts-reader-exe | less
```

The output format is "line-friendly" so you can use `grep`, `awk` or
whatever else for filtering.  It is reasonably fast to just parse
everything everytime.
