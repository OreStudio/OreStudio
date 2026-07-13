#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Download Federal Reserve H.10 historical FX rate pages and extract clean
(date, value) rows for a given set of dates or date range.

Each Fed H.10 historical page (dat00_<code>.htm) contains the full
multi-year daily series in one page, so a currency's page is downloaded
once (cached under --cache-dir) and reused for any number of requested
dates - no per-date network round-trip.

Used to source real, individually-citable FX driver rates for
ores.marketdata's curated reference-vintage datasets (see
projects/ores.sql/populate/marketdata/marketdata_fx_driver_rates_populate.sql)
rather than typing values by hand.

Examples:
    # Table view, explicit dates
    ./tools/fetch_fed_h10_rates.py --currency eu,uk --dates 4-JAN-16,29-JAN-16

    # Table view, every business day the Fed published in a date range
    ./tools/fetch_fed_h10_rates.py --currency eu --from 2016-01-01 --to 2016-01-31

    # SQL VALUES rows ready to paste into a *_populate.sql artefact seed,
    # citing each row's own Fed release page as source_url
    ./tools/fetch_fed_h10_rates.py --currency eu,uk,sz,ja,sd,al,ca,nz \\
        --from 2016-01-01 --to 2016-01-31 --format sql
"""
import argparse
import hashlib
import json
import re
import sys
import urllib.request
import zipfile
from datetime import date, datetime, timezone
from pathlib import Path

CURRENCY_NAMES = {
    "eu": "EUR/USD",
    "uk": "GBP/USD",
    "sz": "USD/CHF",
    "ja": "USD/JPY",
    "sd": "USD/SEK",
    "al": "AUD/USD",
    "ca": "USD/CAD",
    "nz": "NZD/USD",
    "sf": "USD/ZAR",
    "mx": "USD/MXN",
    "in": "USD/INR",
}

BASE_URL = "https://www.federalreserve.gov/releases/h10/hist/dat00_{code}.htm"
DEFAULT_CACHE_DIR = Path(__file__).parent / ".fed_h10_cache"

# Matches: <th id="r1" headers="a1">16-JAN-04 </th> ... <td ...>1.0803</td>
ROW_RE = re.compile(
    r'<th id="r1"[^>]*>\s*(\d{1,2}-[A-Z]{3}-\d{2})\s*</th>\s*'
    r'<td[^>]*>\s*([0-9.]+|ND|n/a)?\s*</td>',
    re.IGNORECASE,
)


def download(code: str, cache_dir: Path, force: bool) -> Path:
    cache_dir.mkdir(parents=True, exist_ok=True)
    out_path = cache_dir / f"dat00_{code}.htm"
    if out_path.exists() and not force:
        print(f"  {code}: using cached {out_path}", file=sys.stderr)
        return out_path
    url = BASE_URL.format(code=code)
    print(f"  {code}: downloading {url}", file=sys.stderr)
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req, timeout=30) as resp:
        data = resp.read()
    out_path.write_bytes(data)
    return out_path


def archive(html_paths: dict, zip_path: Path, invocation: str) -> None:
    """Write a reproducibility archive: a zip of the raw downloaded pages,
    plus a single manifest.json next to it (not inside it) describing how
    the archive was produced -- retrieval timestamp, the exact command
    that produced it, and per-page source URL/sha256/size -- so the exact
    bytes a dataset was generated from are independently verifiable
    without re-downloading from the Fed's site.
    """
    zip_path.parent.mkdir(parents=True, exist_ok=True)
    retrieved_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    manifest = {
        "retrieved_at": retrieved_at,
        "source": "https://www.federalreserve.gov/releases/h10/hist/",
        "command": invocation,
        "archive": zip_path.name,
        "pages": [],
    }

    with zipfile.ZipFile(zip_path, "w", zipfile.ZIP_DEFLATED) as zf:
        for code, html_path in sorted(html_paths.items()):
            data = html_path.read_bytes()
            sha256 = hashlib.sha256(data).hexdigest()
            zf.write(html_path, arcname=html_path.name)
            manifest["pages"].append({
                "code": code,
                "pair": CURRENCY_NAMES.get(code, code),
                "file": html_path.name,
                "source_url": BASE_URL.format(code=code),
                "sha256": sha256,
                "size_bytes": len(data),
            })

    manifest_path = zip_path.parent / "manifest.json"
    manifest_path.write_text(json.dumps(manifest, indent=2, sort_keys=False) + "\n")

    print(f"  archived {len(html_paths)} page(s) -> {zip_path}", file=sys.stderr)
    print(f"  wrote {manifest_path}", file=sys.stderr)


def parse_rows(html_path: Path) -> dict:
    """Returns {date_str_upper (DD-MON-YY): value_str}, parsed in file order."""
    text = html_path.read_text(errors="ignore")
    rows = {}
    for m in ROW_RE.finditer(text):
        date_str, value = m.group(1), m.group(2)
        if value and value not in ("ND", "n/a"):
            rows[date_str.upper()] = value
    return rows


def fed_date_to_iso(d: str) -> str:
    return datetime.strptime(d, "%d-%b-%y").strftime("%Y-%m-%d")


def iso_to_fed_date(iso: str) -> str:
    dt = datetime.strptime(iso, "%Y-%m-%d")
    return f"{dt.day}-{dt.strftime('%b').upper()}-{dt.strftime('%y')}"


def dates_in_range(rows: dict, start: date, end: date) -> list:
    """All dates present in `rows` whose ISO date falls within [start, end]."""
    result = []
    for d in rows:
        iso = fed_date_to_iso(d)
        dt = datetime.strptime(iso, "%Y-%m-%d").date()
        if start <= dt <= end:
            result.append(d)
    result.sort(key=fed_date_to_iso)
    return result


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--currency", required=True,
                    help="Comma-separated Fed H.10 codes, e.g. eu,uk,sz,ja,sd,al,ca,nz")
    ap.add_argument("--dates", help="Comma-separated dates, DD-MON-YY (e.g. 4-JAN-16) or YYYY-MM-DD")
    ap.add_argument("--from", dest="date_from", help="Range start, YYYY-MM-DD")
    ap.add_argument("--to", dest="date_to", help="Range end, YYYY-MM-DD")
    ap.add_argument("--format", choices=["table", "sql"], default="table")
    ap.add_argument("--cache-dir", default=str(DEFAULT_CACHE_DIR),
                    help=f"Where downloaded pages are cached (default: {DEFAULT_CACHE_DIR})")
    ap.add_argument("--force", action="store_true", help="Re-download even if cached")
    ap.add_argument("--archive-zip",
                    help="Also write a zip + manifest.json of the raw downloaded pages to this "
                         "path, for reproducibility independent of the Fed's site staying up "
                         "(e.g. external/fed_h10/pages.zip)")
    args = ap.parse_args()

    if not args.dates and not (args.date_from and args.date_to):
        ap.error("Provide either --dates or both --from/--to")

    cache_dir = Path(args.cache_dir)
    codes = [c.strip() for c in args.currency.split(",")]

    parsed_by_code = {}
    html_paths = {}
    for code in codes:
        html_path = download(code, cache_dir, args.force)
        html_paths[code] = html_path
        parsed_by_code[code] = parse_rows(html_path)

    if args.archive_zip:
        invocation = "./tools/fetch_fed_h10_rates.py " + " ".join(sys.argv[1:])
        archive(html_paths, Path(args.archive_zip), invocation)

    if args.dates:
        requested = []
        for d in args.dates.split(","):
            d = d.strip().upper()
            requested.append(iso_to_fed_date(d) if re.match(r"^\d{4}-\d{2}-\d{2}$", d) else d)
    else:
        start = datetime.strptime(args.date_from, "%Y-%m-%d").date()
        end = datetime.strptime(args.date_to, "%Y-%m-%d").date()
        # Union of dates any requested currency actually published, in range.
        seen = set()
        requested = []
        for code in codes:
            for d in dates_in_range(parsed_by_code[code], start, end):
                if d not in seen:
                    seen.add(d)
                    requested.append(d)
        requested.sort(key=fed_date_to_iso)

    if args.format == "table":
        print(f"{'Pair':<10}" + "".join(f"{d:<14}" for d in requested))
        for code in codes:
            pair = CURRENCY_NAMES.get(code, code)
            line = f"{pair:<10}"
            for d in requested:
                line += f"{parsed_by_code[code].get(d, 'MISSING'):<14}"
            print(line)
        return

    # sql: one VALUES row per (currency, date), sorted by date then pair,
    # citing this row's own Fed release page as source_url.
    rows = []
    for code in codes:
        qualifier = CURRENCY_NAMES.get(code, code)
        for d in requested:
            value = parsed_by_code[code].get(d)
            if value is None:
                print(f"MISSING: {code} {d}", file=sys.stderr)
                continue
            rows.append((qualifier, fed_date_to_iso(d), value, code))
    rows.sort(key=lambda r: (r[1], r[0]))
    for qualifier, iso, value, code in rows:
        print(f"        ('{qualifier}', date '{iso}', {value}, "
              f"'https://www.federalreserve.gov/releases/h10/hist/dat00_{code}.htm'),")


if __name__ == "__main__":
    main()
