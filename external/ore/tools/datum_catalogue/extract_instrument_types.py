#!/usr/bin/env python3
"""Print ORE's market datum instrument types, one per line, with the key tokens
its parser reads as each: "<type> <token> [<token>...]".

The types come from the InstrumentType enum in marketdatum.hpp, without NONE.
The tokens come from the string map parseInstrumentType() holds in
marketdatumparser.cpp, so FX_SPOT lists both FX and FX_SPOT. A type the map
gives no token is printed with none, which the catalogue tests report.

Usage: extract_instrument_types.py <ORE source dir>
"""

import re
import sys
from pathlib import Path


def main():
    source = Path(sys.argv[1]) / "OREData/ored/marketdata"
    header = (source / "marketdatum.hpp").read_text()
    enum = re.search(r"enum class InstrumentType\s*\{(.*?)\}", header, re.S).group(1)
    types = [t.strip() for t in enum.split(",") if t.strip() and t.strip() != "NONE"]

    parser = (source / "marketdatumparser.cpp").read_text()
    tokens = {}
    for token, member in re.findall(
            r'^\s*\{"(\w+)",\s*MarketDatum::InstrumentType::(\w+)\}', parser, re.M):
        tokens.setdefault(member, []).append(token)

    for t in types:
        print(" ".join([t] + tokens.get(t, [])))


if __name__ == "__main__":
    main()
