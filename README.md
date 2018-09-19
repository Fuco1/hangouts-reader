# hangouts-reader

Little CLI utility for parsing Google Hanougts Takeout json data.

# Installation

Build with `stack install`.  This will install a system-wide binary
`hangouts-reader` if your stack `.bin` directory is on the
`$PATH`.  Otherwise refer to stack documentation.

# Usage

The simplest way to use this is to pipe in the json data:

``` shell
cat Hangouts.json | hangouts-reader | less
```

You can also store the processed output in a file for faster access:

``` shell
cat Hangouts.json | hangouts-reader > hangouts-processed-data.txt
```

The application provides several output formats listed below.

## Line output

The output format is "line-friendly" so you can use `grep`, `awk` or
whatever else for filtering.  Fields are separated by tabs.  It is
reasonably fast to just parse everything everytime (the parsed output
is about 10 times smaller than the input file).

Output is prefixed with a list of all conversations and participants
with the format

    conversationId	participant1, participant2, ...

followed by an empty line.  Then follows the conversation data
(messages).  The messages printed are in the format:

    conversationId	time	author	message

They are grouped by conversation and ordered by message time from the
oldest message per conversation at the top.  The conversatiosn are in
no particular order.

# Helpers

The helper script `parse.sh` can be used to operate on the transformed
data.  The first argument is name of the person in the conversation
(first conversation with this name will be picked), the second is the
data file.  For example, if you want to print your conversation with
Emily, use it as so:

``` shell
./parse.sh Emily hangouts-line-data.txt
```

This assumes there is only one conversation where Emily is present,
otherwise it picks the first one.
