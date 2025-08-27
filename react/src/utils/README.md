# React Dashboard Utilities

This directory contains utility functions for the Longleaf React dashboard.

## Files

### `oclFormat.js`
Utilities for converting between React and OCaml yojson formats.

**Key Functions:**
- `parseOCamlVariant(variant)` - Convert `["Backtest"]` to `"Backtest"`
- `toOCamlVariant(value)` - Convert `"Backtest"` to `["Backtest"]`
- `parseTarget(target)` - Parse OCaml Target.t to React format
- `toOCamlTarget(settings)` - Convert React target to OCaml format
- `getTargetDisplay(target)` - Get display string for target
- `getActiveDataFile(target)` - Extract file path from File target
- `parseOCamlCLI(cli)` - Parse complete CLI settings from OCaml
- `toOCamlCLI(cli)` - Convert complete CLI settings to OCaml
- `formatError(error, operation)` - Standardize error messages

### `api.js`
Reusable API functions for server communication.

**Key Functions:**
- `apiCall(method, url, data, timeout)` - Generic API wrapper
- `apiGet(url, timeout)` - GET request helper
- `apiPost(url, data, timeout)` - POST request helper
- `updateServerStatus(status)` - Update server status
- `updateCLI(cliSettings)` - Update CLI settings
- `updateTarget(targetData)` - Update target settings

## OCaml Variant Format

OCaml's yojson derivation converts variants to JSON arrays:
- Simple variants: `Backtest` → `["Backtest"]`
- Parameterized variants: `File "path"` → `["File", "path"]`

All parsing functions handle this format consistently.