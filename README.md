# fedora-composes

A small tool to list and check the status of Fedora composes.

## Examples

`$ fedora-composes list --latest rawhide`
```
https://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20240214.n.1
```
`$ fedora-composes list -l2 branched`
```
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-40-20240214.n.0
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-40-20240213.n.1
```
`$ fedora-composes status --no-more branched`
```
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-40-20240214.n.0
2024-02-14 15:19:10 +08
2024-02-14 15:31:52 +08 DOOMED

https://kojipkgs.fedoraproject.org/compose/branched/Fedora-40-20240213.n.1
2024-02-14 03:40:26 +08
2024-02-14 08:52:37 +08 FINISHED_INCOMPLETE

```

`$ fedora-composes status -n updates 39`
```
https://kojipkgs.fedoraproject.org/compose/updates/Fedora-39-updates-testing-20240214.0
2024-02-14 09:25:36 +08
2024-02-14 10:03:51 +08 FINISHED

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

For example `Fedora-39-updates` is a repo
and `Fedora-39-updates-20230810.0` is a compose for it.

Filtering is case insensitive.

### list

`fedora-composes list` lists releases and other subdirs (rawhide, updates, branched, etc)

`fedora-composes list updates` shows latest updates composes

`fedora-composes list updates fedora-36` shows latest F36 updates composes

`fedora-composes list branched` shows latest branched composes

`$ fedora-composes list --help`
```
Usage: fedora-composes list [-d|--debug]
                            [(-a|--all-composes) | (-L|--latest) |
                              (-l|--limit LIMIT)] [-r|--repos] [DIR] [SUBSTR]

  List dirs/composes (by default only last compose)

Available options:
  -d,--debug               debug output
  -a,--all-composes        All composes
  -L,--latest              Only latest compose
  -l,--limit LIMIT         Max number of composes
  -r,--repos               Only list target repos
  -h,--help                Show this help text
```

### status

`fedora-composes status rawhide` shows time and status of newest rawhide

`fedora-composes status updates fedora-39` shows time and status of updates push

`fedora-composes status branched 39` shows time and status of branched compose

`$ fedora-composes status --help`
```
Usage: fedora-composes status [-d|--debug]
                              [(-a|--all-composes) | (-L|--latest) |
                                (-l|--limit LIMIT)] [-n|--no-more] DIR [SUBSTR]

  Show compose status

Available options:
  -d,--debug               debug output
  -a,--all-composes        All composes
  -L,--latest              Only latest compose
  -l,--limit LIMIT         Max number of composes
  -n,--no-more             Do not prompt for more results
  -h,--help                Show this help text
```

## Installation
fedora-composes rpm builds are available from Copr:
<https://copr.fedorainfracloud.org/coprs/petersen/fedora-composes/>

## Build from source
stack/cabal/cabal-rpm install
