# elm-domotics-ui

> Note: git log shows too many entries, repository was once an everything-combined repo, which was not a good idea.

## Development Environment

Use Elm 0.19.1.

```bash
% elm --version
0.19.1
```

## Running Tests

```bash
elm-test "src-test/TestDecode.elm"
```

## Development Run with Production Backend

In [src/Domotic.elm](src/Domotic.elm), set `fixBackendHostPort` to your backend's host and port:

```elm
fixBackendHostPort : Maybe String
fixBackendHostPort =
    Just "192.168.0.10:80"
```

Compile and serve locally:

```bash
elm make src/Domotic.elm --output domotic.js
python3 -m http.server 8080
```

Open http://localhost:8080 in Safari.

Because the backend is on a different host, you need to disable CORS in Safari:
**Develop menu → Disable Cross-Origin Restrictions**

(The Develop menu can be enabled in Safari → Settings → Advanced → Show features for web developers.)

> Note: `elm reactor` does not work for this app because it serves the Elm file directly,
> bypassing `index.html` — which means the WebSocket port never gets wired up.

## Installation

In [src/Domotic.elm](src/Domotic.elm), set `fixBackendHostPort` to `Nothing` so the backend URL
is taken from the browser's address bar:

```elm
fixBackendHostPort : Maybe String
fixBackendHostPort =
    Nothing
```

Then compile and deploy:

```bash
elm make src/Domotic.elm --output domotic.js && \
scp domotic.js index.html domotica3:/home/dirk/domotic/static
```