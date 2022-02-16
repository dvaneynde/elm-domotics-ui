# elm-domotica-ui

> Note: git log shows too many entries, repository was once an everything-combined repo, which was not a good idea.

## Development Environment

Use ELM version 0.18.

```bash
npm install elm@0.18
```

## Development Run

In domotic.elm, change urlBase to your host's IP, e.g. `192.168.0.10:8080` or `localhost:8080`.

Then:
```bash
elm reactor
```

## Installation

In domotic.elm, change urlBase to your host's IP, e.g. `192.168.0.10:8080` or `localhost:8080`.

Next:

```bash
elm make domotic.elm --output domotic.js
scp domotic.js domotica3:/home/dirk/domotic/static
```