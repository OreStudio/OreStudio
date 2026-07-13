# Federal Reserve H.10 FX Rates

This directory archives the raw Federal Reserve Board H.10 Foreign
Exchange Rates historical release pages used to source the
`marketdata.fx_driver_rates` DQ dataset -- real, individually-citable FX
spot rates, not values typed by hand or approximated from another
source.

## Quick Start

```bash
# Re-download every currency page, regenerate the archive + manifest,
# and print the parsed 2016-02-05 rates as a sanity check.
./tools/fetch_fed_h10_rates.py --currency eu,uk,sz,ja,sd,al,ca,nz,sf,mx,in \
    --dates 2016-02-05 --format table --archive-zip external/fed_h10/pages.zip --force

# Regenerate the full 25-vintage SQL VALUES rows consumed by
# projects/ores.sql/populate/marketdata/marketdata_fx_driver_rates_populate.sql
./tools/fetch_fed_h10_rates.py --currency eu,uk,sz,ja,sd,al,ca,nz,sf,mx,in \
    --dates 2016-01-04,2016-01-29,2016-02-01,2016-02-05,2016-02-29,2016-03-01,2016-03-31, \
2016-04-01,2016-04-29,2016-05-02,2016-05-31,2016-06-01,2016-06-30,2016-07-01,2016-07-29, \
2016-08-01,2016-08-31,2016-09-01,2016-09-30,2016-10-03,2016-10-31,2016-11-01,2016-11-30, \
2016-12-01,2016-12-30 \
    --format sql
```

## Data Files

| File | Description |
|------|--------------|
| `pages.zip` | Raw Fed H.10 historical HTML pages (one per currency, each holding the full multi-year daily series), plus `manifest.json`. |
| `pages.zip:manifest.json` | Per-page `code`, `pair`, `source_url`, `sha256`, `size_bytes`, and the archive's `retrieved_at` timestamp -- lets anyone verify the archived bytes match what the Fed actually published, independent of the Fed's site staying up or serving the same content later. |

## Currencies

| Fed code | Pair | Role |
|----------|------|------|
| `eu` | EUR/USD | major |
| `uk` | GBP/USD | major |
| `sz` | USD/CHF | major |
| `ja` | USD/JPY | major |
| `sd` | USD/SEK | major |
| `al` | AUD/USD | major |
| `ca` | USD/CAD | major |
| `nz` | NZD/USD | major |
| `sf` | USD/ZAR | exotic |
| `mx` | USD/MXN | exotic |
| `in` | USD/INR | exotic |

The 8 majors feed the CRM story's "majors" tier; the 3 exotics feed its
"exotics" tier (see `doc/agile/versions/v0/sprint_23/crm_implementation/`).

## Regenerating

Never hand-edit `pages.zip` or the SQL rows it produces. To add a
currency or a date, add the Fed code to `CURRENCY_NAMES` in
`tools/fetch_fed_h10_rates.py` if it's new, then re-run the Quick Start
commands above and paste the tool's `--format sql` output into
`marketdata_fx_driver_rates_populate.sql`'s artefact seed block.

## Data Source

Federal Reserve Board, H.10 Foreign Exchange Rates, historical release
pages: https://www.federalreserve.gov/releases/h10/hist/

## Related Files

- Fetch/parse/archive script: `tools/fetch_fed_h10_rates.py`
- Methodology: `external/fed_h10/methodology.txt`
- Consuming populate script: `projects/ores.sql/populate/marketdata/marketdata_fx_driver_rates_populate.sql`
