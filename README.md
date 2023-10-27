# fedora-composes

A small tool to list and check the status of Fedora composes.

## Examples

`$ fedora-composes list -l1 rawhide`
```
https://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20231027.n.0
```
`$ fedora-composes list -l2 branched`
```
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-39-20231027.n.0
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-39-20231026.n.0
```
`$ fedora-composes status branched 39`
```
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-39-20231027.n.0
2023-10-27 15:18:56 +08
2023-10-27 18:45:42 +08 FINISHED_INCOMPLETE

```

## Usage

`$ fedora-composes --version`
```
0.2
```
`$ fedora-composes --help`
```
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

There is the notion of repos and composes.

For example `Fedora-38-updates` is a repo
and `Fedora-38-updates-20230810.0` is a compose for it.

Filtering is case insensitive.

### list

`fedora-composes list` lists releases and other subdirs (rawhide, updates, branched, etc)

`fedora-composes list updates` shows latest updates composes

`fedora-composes list updates fedora-36` shows latest F36 updates composes

`fedora-composes list branched` shows latest branched composes

`$ fedora-composes list --help`
```
Usage: fedora-composes list [-d|--debug] 
                            [(-a|--all-composes) | (-l|--limit LIMIT)] 
                            [-r|--repos] [DIR] [SUBSTR]

  List dirs/composes (by default only last compose)

Available options:
  -d,--debug               debug output
  -a,--all-composes        All composes
  -l,--limit LIMIT         Max number of composes
  -r,--repos               Only list target repos
  -h,--help                Show this help text
```

### status

`fedora-composes status rawhide` shows time and status of newest rawhide

`fedora-composes status updates fedora-38` shows time and status of updates push

`fedora-composes status branched 39` shows time and status of branched compose

`$ fedora-composes status --help`
```
Usage: fedora-composes status [-d|--debug] 
                              [(-a|--all-composes) | (-l|--limit LIMIT)] 
                              [-m|--more] DIR [SUBSTR]

  Show compose status

Available options:
  -d,--debug               debug output
  -a,--all-composes        All composes
  -l,--limit LIMIT         Max number of composes
  -m,--more                Offer showing older composes
  -h,--help                Show this help text
```

## Installation
fedora-composes is available from Copr:
<https://copr.fedorainfracloud.org/coprs/petersen/fedora-composes/>

## Build from source
stack/cabal/cabal-rpm install
