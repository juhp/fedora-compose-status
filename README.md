# fedora-composes

A small tool to list and check the status of Fedora composes.

## Examples

`$ fedora-composes list --latest rawhide`

```
https://kojipkgs.fedoraproject.org/compose/rawhide/Fedora-Rawhide-20240909.n.0
```
`$ fedora-composes list -l2 branched`

```
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-41-20240909.n.0
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-41-20240908.n.0
```
`$ fedora-composes status --no-more branched`

```
https://kojipkgs.fedoraproject.org/compose/branched/Fedora-41-20240909.n.0
2024-09-09 15:15:03 +08
2024-09-09 19:59:30 +08 FINISHED_INCOMPLETE

```

`$ fedora-composes status -n updates 40`

```
https://kojipkgs.fedoraproject.org/compose/updates/Fedora-40-updates-testing-20240909.0
2024-09-09 22:33:40 +08 STARTED

https://kojipkgs.fedoraproject.org/compose/updates/Fedora-40-updates-20240909.0
2024-09-09 08:15:35 +08
2024-09-09 09:13:03 +08 FINISHED

```

## Usage

`$ fedora-composes --version`

```
0.2.1
```
`$ fedora-composes --help`

```
check status of fedora composes

Usage: fedora-composes [--version] COMMAND

  https://github.com/juhp/fedora-composes#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  list                     List dirs/composes (by default only last compose)
  status                   Show compose status
```

There is the notion of repos and composes.

For example `Fedora-40-updates` is a repo
and `Fedora-40-updates-20240809.0` is a compose for it.

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

`fedora-composes status updates fedora-40` shows time and status of updates push

`fedora-composes status branched 41` shows time and status of branched compose

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
