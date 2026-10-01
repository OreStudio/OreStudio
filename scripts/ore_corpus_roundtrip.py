#!/usr/bin/env python3
"""Round trip the whole ORE bond corpus through the database.

The single-document recipe stages one file and classifies one pair of
directories. This drives the same three shell verbs over the corpus: it
stages every bond document, uploads them in one pack, imports them in one
go, exports once, then classifies the union of the sources against the one
export.

Why the union. ``ore export`` writes every trade the database holds, so a
database that has seen the corpus exports all of it in one document. The
classifier compares two multisets of (element path, value) pairs and is
order blind, so folding the sources into one Portfolio and comparing it
with the export is the same zero-loss question the per-document recipe
asks, over the whole corpus.

The classifier's two scopes still apply: the bond products and the trade
envelope are in scope, the other families are reported and not gated.

The corpus is not consistent reference data, so the sweep holds it to one
statement per security before it compares. ORE states a bond's terms on
the trade, inline, and the corpus reuses five security ids across ten
documents that disagree: about 117 trades name =SECURITY_1= and state a
different issuer, a different credit curve and a different leg schedule
each. Our model holds one issue per security -- that is the split's point
-- so no database can return more than one of those statements, and a
comparison against all of them reads every contradiction as a loss. The
sweep therefore folds the issue's statement the way the import folds it,
first stated wins, rewrites every later trade that restates the same
security, and reports what it overrode. The two =bondData= members the
instrument keeps for itself, the security id and =BondNotional=, are left
as each trade states them. =--raw-corpus= skips the fold and compares the
corpus exactly as it is written, which is how the contradiction is
measured.

Usage::

    python3 scripts/ore_corpus_roundtrip.py [--work-dir DIR] [--keep]
        [--raw-corpus]

The run needs a freshly recreated database and a provisioned tenant; see
the round-trip recipe for both. Exit code is the classifier's.
"""
import argparse
import re
import shutil
import subprocess
import sys
import uuid
import xml.etree.ElementTree as ET
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_CORPUS = REPO_ROOT / "external/ore/examples"
DEFAULT_WORK_DIR = REPO_ROOT / "build/ore-corpus"
CLASSIFIER = REPO_ROOT / "scripts/ore_mapper_roundtrip_diff.py"
LOGIN = "tenant_admin@acme_corporation Secure-Password-123"

# The bond family's products, as the documents spell them. A document that
# names none of them is not part of the bond sweep.
BOND_MARKERS = (
    "BondData",
    "BondReferenceData",
    "BondFutureData",
    "BondOptionData",
    "BondRepoData",
    "BondTRSData",
    "ConvertibleBond",
    "CallableBond",
)


def root_name(path: Path) -> str:
    """The document's root element, read the way the exporter reads it."""
    text = path.read_text(encoding="utf-8", errors="replace")[:4096]
    match = re.search(r"<([A-Za-z_][\w.-]*)[\s/>]", text)
    return match.group(1) if match else ""


def is_bond_portfolio(path: Path) -> bool:
    if root_name(path) != "Portfolio":
        return False
    text = path.read_text(encoding="utf-8", errors="replace")
    return any(marker in text for marker in BOND_MARKERS)


# The exporter writes four kinds of document. A portfolio needs the
# conventions, calendars and currency configuration its instruments name, so
# the sweep stages those beside it; the bond documents are the ones the
# source union is built from.
CONFIG_ROOTS = ("CurrencyConfig", "CalendarAdjustments", "Conventions")


def discover(corpus_dir: Path) -> tuple:
    """The bond portfolios to gate on, and the configuration they need."""
    portfolios = [p for p in sorted(corpus_dir.rglob("*.xml")) if is_bond_portfolio(p)]
    if not portfolios:
        raise SystemExit(f"no bond portfolio document under {corpus_dir}")
    config = [
        p
        for p in sorted(corpus_dir.rglob("*.xml"))
        if root_name(p) in CONFIG_ROOTS
    ]
    return portfolios, config


# The two bondData members the model keeps on the trade's own instrument,
# which the sweep must leave as each trade states them. Everything else
# under BondData is the bond issue's statement: the mapper reads it into
# the issue, the issue is keyed by the security id, and one security
# therefore holds one statement. See the module docstring.
TRADE_OWNED_BOND_MEMBERS = frozenset({"SecurityId", "BondNotional"})


def member_signature(element: ET.Element):
    """A member's shape and text, with the document's indentation removed.

    Two documents state the same member with different whitespace, so the
    comparison reads the element's own text and children and never its
    tail.
    """
    return (
        element.tag,
        (element.text or "").strip(),
        tuple(member_signature(child) for child in element),
    )


def issue_statement(bond: ET.Element) -> list:
    """The issue-level members of one trade's bondData, in document order.

    A member repeats: a bond with a fixed and a floating leg states two
    =LegData= elements, so the statement is the sequence, not a map from
    tag to value.
    """
    return [
        (child.tag, member_signature(child), ET.tostring(child, encoding="unicode"))
        for child in bond
        if child.tag not in TRADE_OWNED_BOND_MEMBERS
    ]


def hold_one_statement_per_security(root: ET.Element) -> dict:
    """Rewrite every restatement of a security to the first one stated.

    The import resolves a security by its id and keeps the first statement
    it reads, so this mirrors it: the first trade that names a security
    states the issue, and every later trade that names the same security
    must agree. A later statement is rewritten in place, so the document
    that is imported states the same issue on every trade and the export
    can match it pair for pair.

    Returns the report the caller prints: the distinct security ids, the
    trades that restated one, and how many trades stated each member
    differently.
    """
    canonical: dict = {}
    restated_trades = set()
    restatements: Counter = Counter()
    for trade in root.iter("Trade"):
        # A bond product does not always state its bondData directly: a
        # forward bond wraps it in ForwardBondData, and a callable or
        # convertible bond wraps it the same way. The security is the same
        # datum wherever the document puts it, so every nested statement is
        # folded too.
        for bond in trade.iter("BondData"):
            security_id = bond.findtext("SecurityId")
            if not security_id:
                continue
            statement = issue_statement(bond)
            if security_id not in canonical:
                canonical[security_id] = statement
                continue

            first = canonical[security_id]
            if [member[1] for member in statement] == [member[1] for member in first]:
                continue
            restated_trades.add(trade.get("id"))
            for tag in sorted({member[0] for member in statement + first}):
                stated = [member[1] for member in statement if member[0] == tag]
                held = [member[1] for member in first if member[0] == tag]
                if stated != held:
                    restatements[tag] += 1

            # The statement is rewritten in place: the trade keeps its own
            # security id and notional where they are, and the issue's
            # members are replaced by the ones the security's first
            # statement gave.
            owned = [child for child in bond if child.tag in TRADE_OWNED_BOND_MEMBERS]
            replaced = [child for child in bond if child.tag not in TRADE_OWNED_BOND_MEMBERS]
            at = list(bond).index(replaced[0]) if replaced else len(bond)
            for child in replaced:
                bond.remove(child)
            for offset, member in enumerate(first):
                bond.insert(at + offset, ET.fromstring(member[2]))

    return {
        "securities": len(canonical),
        "restated_trades": len(restated_trades),
        "restatements": restatements,
    }


def stage(documents: list, source_dir: Path) -> None:
    """Copy each document under a name unique within the pack.

    The importer names the book it builds after the file, so two documents
    that share a file name would collide in one database.
    """
    if source_dir.exists():
        shutil.rmtree(source_dir)
    source_dir.mkdir(parents=True)
    for index, document in enumerate(documents):
        shutil.copy2(document, source_dir / f"{index:03d}-{document.name}")


def merge_sources(source_dir: Path, merged: Path, portfolios: int, canonicalise: bool) -> tuple:
    """Fold the staged portfolios into one Portfolio.

    The classifier pairs element paths, not files, so the union of the
    corpus is one document whose children are every source's children. A
    trade id stated by more than one document is kept once, because the
    import keys on the id and saves it once. The configuration documents
    staged beside them are not portfolios and are left out. Staging numbers
    the files in order, so the first ``portfolios`` of them are the
    portfolios.

    Unless ``canonicalise`` is off, every security is then held to the one
    statement its first trade gives it. The report of that fold is
    returned beside the child count.
    """
    merged_root = ET.Element("Portfolio")
    seen_ids = set()
    for source in sorted(source_dir.glob("*.xml"))[:portfolios]:
        try:
            root = ET.parse(source).getroot()
        except ET.ParseError as error:
            raise SystemExit(f"cannot parse {source}: {error}")
        for child in root:
            # The import keys a trade on its id, so a corpus that states the
            # same id in more than one document saves it once. The union is
            # folded the same way, or the duplicates read as trades the
            # export lost when they were never imported twice.
            if child.tag == "Trade":
                trade_id = child.get("id")
                if trade_id in seen_ids:
                    continue
                seen_ids.add(trade_id)
            merged_root.append(child)
    report = (
        hold_one_statement_per_security(merged_root)
        if canonicalise
        else {"securities": 0, "restated_trades": 0, "restatements": Counter()}
    )
    merged.parent.mkdir(parents=True, exist_ok=True)
    ET.ElementTree(merged_root).write(merged, encoding="utf-8", xml_declaration=True)
    return len(merged_root), report


def print_corpus_report(report: dict, canonicalised: bool) -> None:
    """Print what the corpus states for each security, and what was overridden."""
    tag = "held to one statement per security" if canonicalised else "compared raw"
    print(f"Reference data ({tag}): {report['securities']} distinct security id(s)")
    if not canonicalised:
        return
    print(f"  trades restating a security: {report['restated_trades']}")
    if report["restatements"]:
        print("  overridden, by member:")
        for member, count in report["restatements"].most_common():
            print(f"    {count:6d}  {member}")
    else:
        print("  the corpus states one statement per security; nothing overridden")


def write_script(work_dir: Path, source_dir: Path, output: Path, request_id: str) -> Path:
    """One shell session: pack the sources, import them, export the result."""
    lines = [
        "connect $ORES_NATS_URL",
        f"login {LOGIN}",
        f"ore upload {source_dir} --request-id {request_id}",
        f"ore import {request_id} --parent-portfolio-name ore_corpus_{request_id}",
        f"ore export {output}",
        "logout",
        "exit",
    ]
    script = work_dir / "corpus-roundtrip.ores"
    script.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return script


def run_script(script: Path) -> int:
    completed = subprocess.run(
        [str(REPO_ROOT / "compass.sh"), "shell", "-l", str(script)],
        cwd=str(REPO_ROOT),
    )
    return completed.returncode


def classify(source_dir: Path, output_dir: Path, all_products: bool) -> int:
    command = [
        sys.executable,
        str(CLASSIFIER),
        "--source-dir",
        str(source_dir),
        "--output-dir",
        str(output_dir),
    ]
    if all_products:
        command.append("--all-products")
    return subprocess.run(command, cwd=str(REPO_ROOT)).returncode


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus-dir", type=Path, default=DEFAULT_CORPUS)
    parser.add_argument("--work-dir", type=Path, default=DEFAULT_WORK_DIR)
    parser.add_argument(
        "--all-products",
        action="store_true",
        help="gate on every product the corpus states, not only the bond scope",
    )
    parser.add_argument(
        "--raw-corpus",
        action="store_true",
        help="compare the corpus as written, without holding it to one statement per security",
    )
    parser.add_argument(
        "--keep",
        action="store_true",
        help="keep the staged pack after the run",
    )
    args = parser.parse_args()

    work_dir = args.work_dir
    pack_dir = work_dir / "pack"
    union_dir = work_dir / "union"
    output_dir = work_dir / "output"
    output_dir.mkdir(parents=True, exist_ok=True)

    portfolios, config = discover(args.corpus_dir)
    print(
        f"Bond corpus: {len(portfolios)} portfolio(s) and "
        f"{len(config)} configuration document(s) under {args.corpus_dir}"
    )
    # Fold the corpus into one document and import *that*, rather than the
    # documents it came from. The corpus states the same trade id in more
    # than one document and the import keys on the id, so importing every
    # document would leave whichever copy it saw last while the comparison
    # holds the one it was folded from. One document, one write, one
    # comparison.
    staging = work_dir / "staged"
    stage(portfolios + config, staging)
    union = union_dir / "portfolio_roundtrip.xml"
    children, report = merge_sources(
        staging, union, len(portfolios), canonicalise=not args.raw_corpus
    )
    print(f"Union of the sources: {children} top-level element(s)")
    print_corpus_report(report, canonicalised=not args.raw_corpus)

    # The pack holds the union and the configuration documents beside it,
    # and nothing else: the portfolios it was folded from would import a
    # second copy of every trade. The classifier sees the union alone,
    # because the export is one document and a configuration document has
    # no output of its own to pair with.
    #
    # The imported file's name becomes the book's name, and a book name is
    # unique in a tenant, so the pack's copy carries the run's request id.
    # Without it a second run against the same database fails at save_book
    # and the sweep can only be measured once per database.
    request_id = str(uuid.uuid4())
    if pack_dir.exists():
        shutil.rmtree(pack_dir)
    pack_dir.mkdir(parents=True)
    shutil.copy2(union, pack_dir / f"000-union-{request_id}.xml")
    for document in sorted(staging.glob("*.xml"))[len(portfolios):]:
        shutil.copy2(document, pack_dir / document.name)

    output = output_dir / "portfolio_roundtrip.xml"
    if output.exists():
        output.unlink()
    script = write_script(work_dir, pack_dir, output, request_id)
    print(f"Running the import and export ({script.name})...")
    if run_script(script) != 0:
        return 1
    if not output.exists():
        print(f"the exporter wrote no {output.name}", file=sys.stderr)
        return 1

    # The classifier reads the union and the export, so the work directory
    # goes only once it has run: a run that deleted it first classified a
    # directory that was gone.
    exit_code = classify(union_dir, output_dir, args.all_products)
    if not args.keep:
        shutil.rmtree(work_dir, ignore_errors=True)
    return exit_code


if __name__ == "__main__":
    sys.exit(main())
