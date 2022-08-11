# fedora-composes

A small tool to list and check the status of Fedora composes.

## Examples

```shellsession
$ fedora-composes list rawhide
Fedora-Rawhide-20220809.n.0
$ fedora-composes list branched
Fedora-36-20220506.n.0
Fedora-37-20220811.n.0
$ fedora-composes status branched 37
2022-08-11 08:58:46 +08 -> 2022-08-11 08:58:48 +08 STARTED Fedora-37-20220811.n.0
```

## Usage

```shellsession
$ fedora-composes --version
0.1
$ fedora-composes --help
check status of fedora composes

Usage: fedora-composes [--version] COMMAND
  description here

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  list                     List dirs/composes (by default only last compose)
  status                   Show compose status
```

There is the notation of repos and composes.

For example `Fedora-36-updates` is a repo
and `Fedora-36-updates-20220810.0` is a compose for it.

Filtering is case insensitive.

### list

`fedora-composes list` lists releases and other subdirs (rawhide, updates, branched, etc)

`fedora-composes list updates` shows latest updates composes

`fedora-composes list updates fedora-36` shows latest F36 updates composes

`fedora-composes list branched` shows latest branched composes

```shellsession
$ fedora-composes list --help
Usage: fedora-composes list [-d|--debug] [(-a|--all-repos) | (-n|--num NOREPOS)]
                            [(-A|--all-composes) | (-l|--limit LIMIT)]
                            [-r|--repos] [DIR] [SUBSTR]
  List dirs/composes (by default only last compose)

Available options:
  -d,--debug               debug output
  -a,--all-repos           All repos
  -n,--num NOREPOS         Number of repos (default: 6)
  -A,--all-composes        All composes
  -l,--limit LIMIT         Number of composes (default: 1)
  -r,--repos               Only list target repos
  -h,--help                Show this help text
```

### status

`fedora-composes status rawhide` shows time and status of newest rawhide

`fedora-composes status updates fedora-36` shows time and status of updates push

`fedora-composes status branched 37` shows time and status of branched compose

```shellsession
$ fedora-composes status --help
Usage: fedora-composes status [-d|--debug]
                              [(-a|--all-repos) | (-n|--num NOREPOS)]
                              [(-A|--all-composes) | (-l|--limit LIMIT)] DIR
                              [SUBSTR]
  Show compose status

Available options:
  -d,--debug               debug output
  -a,--all-repos           All repos
  -n,--num NOREPOS         Number of repos (default: 6)
  -A,--all-composes        All composes
  -l,--limit LIMIT         Number of composes (default: 1)
  -h,--help                Show this help text
```

## Installation

stack/cabal/cabal-rpm install
