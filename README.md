# elm-domotics-ui

> Note: git log shows too many entries, repository was once an everything-combined repo, which was not a good idea.

## Development Environment

Use ELM version 0.18.

```bash
npm install elm@0.18
```

## Development Run

In domotic.elm, change `fixBackendHostPort` to your backend's host and port, e.g. `192.168.0.10:80` or `localhost:80`.

Then:
```bash
elm reactor
```

Open your browser to http://localhost:8000

## Installation

In domotic.elm, set `fixBackendHostPort` to `Nothing`.

Next:

```bash
elm make domotic.elm --output domotic.js
scp domotic.js domotica3:/home/dirk/domotic/static
```