# FIX Protocol data dictionaries

XML data dictionaries for the FIX (Financial Information eXchange) Protocol's
Market Data message family, maintained by the FIX Trading Community. See
`manifest.json` for per-file provenance (source URL, sha256, size) and
`methodology.txt` for how these files were located and downloaded.

The authoritative, browsable specification lives at
https://fiximate.fixtrading.org/ (FIX Trading Community). These particular
files are machine-readable XML data dictionaries redistributed by the
QuickFIX open-source engine project under a permissive BSD-style license
(see `LICENSE`), pinned to a specific commit for reproducibility.

## Contents

- `FIX44.xml` — FIX 4.4 data dictionary (historically dominant for market
  data and order flow).
- `FIX50SP2.xml` — FIX 5.0 Service Pack 2 data dictionary (the last
  numbered release before "FIX Latest").
- `LICENSE` — QuickFIX Software License, Version 1.0, governing this
  redistributed copy.

## Knowledge doc

See `doc/knowledge/domain/financial_information_exchange_protocol.org`
for what FIX is, its Market Data message family, symbology, and how it
compares to ORE Studio's own identifier schemes.
