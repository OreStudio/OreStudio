#!/usr/bin/env python3
"""Print one of ORE's market datum enums, one member per line, with the key
tokens its parser reads as that member: "<member> <token> [<token>...]".

The members come from the enum in marketdatum.hpp: InstrumentType or QuoteType.
The tokens come from the string map that marketdatumparser.cpp's parse function
for the enum holds, so FX_SPOT lists both FX and FX_SPOT. NONE is printed only
when the map gives it a token: no key names the NONE instrument type, but NULL
names the NONE quote type. Any other member the map gives no token is printed
with none, which the catalogue tests report.

Usage: extract_enum_tokens.py <ORE source dir> InstrumentType|QuoteType
"""

import re
import sys
from pathlib import Path


def main():
    source = Path(sys.argv[1]) / "OREData/ored/marketdata"
    enum_name = sys.argv[2]
    header = (source / "marketdatum.hpp").read_text()
    enum = re.search(rf"enum class {enum_name}\s*\{{(.*?)\}}", header, re.S).group(1)
    members = [m.strip() for m in enum.split(",") if m.strip()]

    parser = (source / "marketdatumparser.cpp").read_text()
    tokens = {}
    for token, member in re.findall(
            rf'^\s*\{{"(\w+)",\s*MarketDatum::{enum_name}::(\w+)\}}', parser, re.M):
        tokens.setdefault(member, []).append(token)

    for m in members:
        if m == "NONE" and m not in tokens:
            continue
        print(" ".join([m] + tokens.get(m, [])))


if __name__ == "__main__":
    main()
