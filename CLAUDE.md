# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

**Build:**
```bash
elm make src/Domotic.elm --output domotic.js
```

**Test:**
```bash
elm-test src-test/TestDecode.elm
```

**Run locally** (do not use `elm reactor` — it bypasses `index.html` and breaks WebSocket initialization):
```bash
python3 -m http.server 8080
# Open http://localhost:8080 in Safari with CORS disabled:
# Safari → Develop → Disable Cross-Origin Restrictions
```

## Architecture

Single-page Elm 0.19.1 home automation UI using The Elm Architecture (TEA). The entire application lives in [src/Domotic.elm](src/Domotic.elm).

**Communication:**
- **HTTP REST** — sends control commands to `/rest/actuators/{name}/{command}`
- **WebSocket** — receives real-time device status from `ws://{host}:{port}/status/`
- WebSocket is managed by JavaScript in [index.html](index.html) via Elm ports; Elm sends a connect request (`connectWebSocket` port out) and receives updates (`newStatusViaWs` port in)

**Backend URL configuration** — controlled by `fixBackendHostPort` in [src/Domotic.elm](src/Domotic.elm):
- `Just "192.168.0.10:80"` — targets a specific backend (use for development against the production system)
- `Nothing` — derives host/port from `window.location` (use for production deployment)

**Device types:** `Lamp`, `DimmedLamp`, `Screen`, `SunWindController`, `WindSensor`, `LightSensor`, `Thermostat`. Each has its own JSON decoder and view rendering. Device groups are hardcoded with Dutch names (`Beneden`, `Buiten`, `ScreensZ`, etc.).

**Data flow:** WebSocket message → JSON decoded into `Status` records with a polymorphic `ExtraStatus` union type → stored in `Model.groups` (ordered `Dict`) → rendered as collapsible groups.

## Deployment

Set `fixBackendHostPort = Nothing`, build, then SCP `index.html` and `domotic.js` to the server.
