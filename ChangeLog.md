# Version history for fedora-composes

## 0.2.1 (2024-09-09)
- status: for updates default to yes for more
- status: don't prompt for more with --limit
- ignore undated compose directories
- add readme url to help description

## 0.2 (2024-02-14)
- output compose urls and use multiple lines for status
- status: default to showing latest finished and prompt to show older
- if STARTED only print STATUS time
- handle missing STATUS and COMPOSE_ID
- default to up to 10 composes
- add --latest option
- append logs/global to url if doomed

## 0.1 (2022-08-11)
- initial release with list and status commands and filtering
