# IP to Country Mapping Data

This directory contains IP address to country mapping data from iptoasn.com.

## Source

- **Website**: https://iptoasn.com/
- **License**: PDDL v1.0 (Public Domain)
- **Format**: TSV (tab-separated values)

## Files

| File | Description |
|------|-------------|
| `ip2country-v4-u32.tsv` | IPv4 ranges with country codes |
| `manifest.txt` | Data manifest with format details |
| `methodology.txt` | Data sourcing methodology |

## Updating the Data

1. Download the latest `ip2country-v4-u32.tsv.gz` from https://iptoasn.com/
2. Extract and replace `ip2country-v4-u32.tsv`
3. Re-run the populate scripts to update the database

## Usage in ORE Studio

This data is used for:
- Geolocation services (mapping IP addresses to countries)
- Security analytics (identifying traffic origins)
- Compliance reporting (jurisdiction determination)
