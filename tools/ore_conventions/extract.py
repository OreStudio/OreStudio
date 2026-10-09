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
"""Extract the one canonical instrument-convention set from the ORE examples.

The ORE example corpus carries a number of documents whose root is
<Conventions>. Each direct child of that root is one entry; the child's tag
names the kind and its <Id> child names the entry. Across the corpus an Id
sometimes carries more than one definition. This tool groups the definitions,
applies the documented resolution rule, overlays the checked-in human
decisions, and writes the canonical set plus an audit trail.

Extraction is mechanical and rerunnable. Curation lives in decisions.tsv and
survives a re-extraction.

Resolution rule, in order:

  1. most non-empty fields (child elements besides Id whose content is not
     empty after whitespace normalisation);
  2. then most distinct files;
  3. then the non-legacy path (Legacy/, ORE-API/, ORE-Python/, ScriptedTrade/,
     MinimalSetup/);
  4. then lexicographic order of the normalised signature.

An Id is flagged for human review when the decision rests on rule 3 or rule 4,
or when the rule-1 winner (most non-empty fields) is not the plurality winner
(most files). A pinned decision from decisions.tsv wins over the rules; a pin
that no longer matches any corpus variant is reported as STALE and kept.

Ids in one kind that carry one identical definition but different spellings are
classified by the shape of the difference:

  * A — every member strips to a single stem with or without a trailing
    `-CONVENTIONS`. The group is one convention: the `-CONVENTIONS` spelling is
    canonical and the other spellings become aliases.
  * B — every member is `PREFIX:STEM` and only `PREFIX` differs. These are
    different feeds, so both stay canonical and the group is reported only.
  * C — anything else. The tool does not guess; every C group goes to the
    review file for a human.

A decision keyed by an alias Id is moved onto the canonical Id, so a pin made
against the old spelling still lands.

Run with the project venv:

    ./projects/ores.codegen/venv/bin/python tools/ore_conventions/extract.py

Usage: extract.py [--examples DIR] [--decisions FILE] [--out DIR]
"""

import argparse
import hashlib
import sys
import xml.etree.ElementTree as ET
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path

# A supporting file is legacy when its path contains any of these markers.
LEGACY_MARKERS = (
    "Legacy/",
    "ORE-API/",
    "ORE-Python/",
    "ScriptedTrade/",
    "MinimalSetup/",
)

DECISIONS_HEADER = ["kind", "id", "decision", "signature"]
CANONICAL_HEADER = [
    "kind",
    "id",
    "signature",
    "source_path",
    "variant_count",
    "decided_by",
    "aliases",
]
ALIASES_HEADER = ["kind", "alias_id", "canonical_id", "pattern"]
REVIEW_HEADER = [
    "kind",
    "id",
    "variants",
    "winner_signature",
    "runner_up_signature",
    "why_flagged",
]
STALE_SOURCE = "<stale-pin>"
TODO_VALUES = {"", "todo"}

# The suffix the newer corpus samples put on a convention Id. A group whose
# members all strip to one stem is a rename, not several conventions.
CONVENTIONS_SUFFIX = "-CONVENTIONS"

# Id-spelling group shapes. A is resolved automatically, B is reported only,
# and C is handed to a human.
PATTERN_SUFFIX = "A"
PATTERN_VENDOR_PREFIX = "B"
PATTERN_OTHER = "C"


class DecisionError(Exception):
    """decisions.tsv is missing or cannot be parsed."""


def local(tag: str) -> str:
    """The tag without any XML namespace."""
    return tag.rsplit("}", 1)[-1]


def norm(text: str | None) -> str:
    """Whitespace-normalised text, so formatting is never a difference."""
    return " ".join((text or "").split())


def canon(element: ET.Element) -> str:
    """The element's content, with nested fields sorted by tag name."""
    text = norm(element.text)
    parts = [text] if text else []
    for child in sorted(element, key=lambda c: local(c.tag)):
        parts.append(f"{local(child.tag)}={canon(child)}")
    return "/".join(parts)


def signature(entry: ET.Element) -> str:
    """Everything about an entry except its Id, normalised and comparable.

    Direct fields are sorted by tag name. A field present with an empty value
    stays present as `Tag=`, so the difference between "absent" and "present
    but empty" is never lost.
    """
    fields = [
        (local(child.tag), f"{local(child.tag)}={canon(child)}")
        for child in entry
        if local(child.tag) != "Id"
    ]
    fields.sort(key=lambda pair: pair[0])
    return " ; ".join(text for _, text in fields)


def digest_of(sig: str) -> str:
    """The stable key for a normalised signature."""
    return hashlib.md5(sig.encode("utf-8")).hexdigest()[:10]


def field_counts(entry: ET.Element) -> tuple[int, int]:
    """(present fields, non-empty fields) for the entry, excluding Id."""
    values = [
        canon(child) for child in entry if local(child.tag) != "Id"
    ]
    return len(values), sum(1 for value in values if value != "")


def convention_files(root: Path):
    """Yield (path, root-or-None); None marks a document that will not parse."""
    for path in sorted(root.rglob("*.xml")):
        try:
            tree = ET.parse(path)
        except ET.ParseError:
            yield path, None
            continue
        if local(tree.getroot().tag) == "Conventions":
            yield path, tree.getroot()


def looks_like_conventions(path: Path) -> bool:
    """Cheap test for a broken file that meant to be a conventions document."""
    try:
        return "<Conventions" in path.read_text(errors="replace")[:65536]
    except OSError:
        return False


def is_legacy(rel_path: str) -> bool:
    return any(marker in rel_path for marker in LEGACY_MARKERS)


def legacy_label(files: tuple[str, ...]) -> str:
    legacy = [f for f in files if is_legacy(f)]
    if not legacy:
        return "no"
    if len(legacy) == len(files):
        return "all"
    return "mixed"


@dataclass
class Variant:
    """One distinct normalised definition of a (kind, Id) pair."""

    signature: str
    files: tuple[str, ...]
    present_fields: int
    nonempty_fields: int
    count: int

    @property
    def digest(self) -> str:
        return digest_of(self.signature)

    @property
    def source(self) -> str:
        return self.files[0] if self.files else STALE_SOURCE


@dataclass
class Corpus:
    definitions: dict[tuple[str, str], list[Variant]]
    entries_per_kind: dict[str, int]
    files: int
    parse_failures: list[tuple[str, str]] = field(default_factory=list)
    missing_id: list[tuple[str, str]] = field(default_factory=list)
    duplicate_in_file: list[tuple[str, str, str, str]] = field(default_factory=list)


@dataclass
class Decision:
    """One row of decisions.tsv."""

    kind: str
    id: str
    decision: str
    signature: str
    line: int

    @property
    def is_todo(self) -> bool:
        return self.decision.strip().lower() in TODO_VALUES


@dataclass
class Resolution:
    """The chosen definition for one (kind, Id) pair, with its audit reasons."""

    kind: str
    id: str
    variants: list[Variant]
    winner: Variant | None
    decided_by: str
    reasons: list[str] = field(default_factory=list)
    stale: bool = False
    todo: bool = False
    pinned_signature: str | None = None

    @property
    def flagged(self) -> bool:
        return bool(self.reasons)

    @property
    def signature(self) -> str:
        if self.stale and self.pinned_signature is not None:
            return self.pinned_signature
        return self.winner.signature if self.winner else ""

    @property
    def source(self) -> str:
        if self.stale:
            return STALE_SOURCE
        return self.winner.source if self.winner else STALE_SOURCE

    def ordered_variants(self) -> list[Variant]:
        return sort_variants(self.variants)

    def runner_up(self) -> str:
        for variant in self.ordered_variants():
            if self.winner is None or variant.signature != self.winner.signature:
                return variant.signature
        return ""


def sort_variants(variants: list[Variant]) -> list[Variant]:
    """Display order: strongest first, then deterministic."""
    return sorted(
        variants,
        key=lambda v: (-v.nonempty_fields, -len(v.files), v.signature),
    )


def parse_corpus(root: Path) -> Corpus:
    """Read the corpus once into per-(kind, Id) variant groups plus defects."""
    # (kind, id) -> signature -> Variant
    grouped: dict[tuple[str, str], dict[str, Variant]] = defaultdict(dict)
    entries_per_kind: dict[str, int] = defaultdict(int)
    parse_failures: list[tuple[str, str]] = []
    missing_id: list[tuple[str, str]] = []
    duplicate_in_file: list[tuple[str, str, str, str]] = []
    files = 0

    for path, element in convention_files(root):
        rel = path.relative_to(root).as_posix()
        if element is None:
            if looks_like_conventions(path):
                parse_failures.append((rel, "XML does not parse"))
            continue
        files += 1
        seen_here: dict[tuple[str, str], dict[str, int]] = defaultdict(
            lambda: defaultdict(int)
        )
        for entry in element:
            kind = local(entry.tag)
            id_element = next(
                (c for c in entry if local(c.tag) == "Id"), None
            )
            if id_element is None:
                missing_id.append((rel, kind))
                continue
            ident = norm(id_element.text)
            sig = signature(entry)
            present, nonempty = field_counts(entry)
            entries_per_kind[kind] += 1
            seen_here[(kind, ident)][sig] += 1
            variant = grouped[(kind, ident)].get(sig)
            if variant is None:
                variant = Variant(
                    signature=sig,
                    files=(),
                    present_fields=present,
                    nonempty_fields=nonempty,
                    count=0,
                )
                grouped[(kind, ident)][sig] = variant
            variant.count += 1
            if rel not in variant.files:
                variant.files = variant.files + (rel,)
        for (kind, ident), sigs in sorted(seen_here.items()):
            total = sum(sigs.values())
            if total > 1:
                status = "identical" if len(sigs) == 1 else "conflicting"
                duplicate_in_file.append(
                    (rel, kind, f"{ident} x{total}", status)
                )

    definitions = {
        key: [grouped[key][sig] for sig in sorted(grouped[key])]
        for key in grouped
    }
    for variants in definitions.values():
        for variant in variants:
            variant.files = tuple(sorted(variant.files))
    return Corpus(
        definitions=definitions,
        entries_per_kind=dict(entries_per_kind),
        files=files,
        parse_failures=parse_failures,
        missing_id=missing_id,
        duplicate_in_file=duplicate_in_file,
    )


def load_decisions(path: Path) -> dict[tuple[str, str], Decision]:
    """Read decisions.tsv into a (kind, Id) -> Decision map.

    A malformed file raises DecisionError: an unreadable decision must never
    be silently ignored, because that would discard a human decision.
    """
    if not path.is_file():
        raise DecisionError(f"no such decisions file: {path}")
    try:
        text = path.read_text(encoding="utf-8")
    except OSError as error:
        raise DecisionError(f"cannot read decisions file {path}: {error}")

    decisions: dict[tuple[str, str], Decision] = {}
    header_seen = False
    for lineno, raw in enumerate(text.splitlines(), start=1):
        line = raw.rstrip("\r")
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        columns = line.split("\t")
        if not header_seen:
            if [c.strip() for c in columns] != DECISIONS_HEADER:
                raise DecisionError(
                    f"{path}:{lineno}: expected header "
                    f"{chr(9).join(DECISIONS_HEADER)!r}, got {line!r}"
                )
            header_seen = True
            continue
        if len(columns) != len(DECISIONS_HEADER):
            raise DecisionError(
                f"{path}:{lineno}: expected {len(DECISIONS_HEADER)} tab-separated "
                f"columns, got {len(columns)}"
            )
        kind, ident, decision, sig = (column.strip() for column in columns)
        if not kind or not ident:
            raise DecisionError(
                f"{path}:{lineno}: kind and id must not be empty"
            )
        key = (kind, ident)
        if key in decisions:
            raise DecisionError(
                f"{path}:{lineno}: duplicate decision for {kind} / {ident}"
            )
        decisions[key] = Decision(
            kind=kind,
            id=ident,
            decision=decision,
            signature=columns[3].strip(),
            line=lineno,
        )
    if not header_seen:
        raise DecisionError(f"{path}: no header row found")
    return decisions


def resolve_variants(variants: list[Variant]) -> tuple[Variant, str, list[str]]:
    """Apply the resolution rule; return (winner, decided_by, reasons)."""
    if len(variants) == 1:
        return variants[0], "only-variant", []
    rule1 = [
        v
        for v in variants
        if v.nonempty_fields
        == max(v.nonempty_fields for v in variants)
    ]
    max_files_all = max(len(v.files) for v in variants)
    plurality = {v.signature for v in variants if len(v.files) == max_files_all}
    disagree = not any(v.signature in plurality for v in rule1)

    if len(rule1) == 1:
        winner, decided_by = rule1[0], "rule-1"
    else:
        max_files = max(len(v.files) for v in rule1)
        rule2 = [v for v in rule1 if len(v.files) == max_files]
        if len(rule2) == 1:
            winner, decided_by = rule2[0], "rule-2"
        else:
            non_legacy = [
                v for v in rule2 if any(not is_legacy(f) for f in v.files)
            ]
            if len(non_legacy) == 1:
                winner, decided_by = non_legacy[0], "rule-3"
            else:
                pool = non_legacy if non_legacy else rule2
                winner = min(pool, key=lambda v: v.signature)
                decided_by = "rule-4"

    reasons: list[str] = []
    if disagree:
        reasons.append(
            "rule-1 field-count winner is not the rule-2 plurality (most-files) winner"
        )
    if decided_by == "rule-3":
        reasons.append("decided by rule-3 (non-legacy path)")
    if decided_by == "rule-4":
        reasons.append("decided by rule-4 (lexicographic signature)")
    return winner, decided_by, reasons


def match_pin(
    decision: Decision, variants: list[Variant]
) -> Variant | None:
    """Resolve a pin to a corpus variant by hash key or by full signature."""
    by_digest = [v for v in variants if v.digest == decision.decision]
    if len(by_digest) > 1:
        raise DecisionError(
            f"decisions.tsv:{decision.line}: hash {decision.decision} matches "
            f"{len(by_digest)} variants of {decision.kind} / {decision.id}"
        )
    if by_digest:
        return by_digest[0]
    by_signature = [v for v in variants if v.signature == decision.decision]
    if len(by_signature) > 1:
        raise DecisionError(
            f"decisions.tsv:{decision.line}: signature matches several variants "
            f"of {decision.kind} / {decision.id}"
        )
    if by_signature:
        return by_signature[0]
    return None


def remap_decisions(
    decisions: dict[tuple[str, str], Decision],
    renames: dict[str, dict[str, str]],
) -> dict[tuple[str, str], Decision]:
    """Move a decision made against an alias Id onto its canonical Id.

    A human who pinned `EUR-6M-FRA` pinned the convention that is now spelled
    `EUR-6M-FRA-CONVENTIONS`; without this the pin would be reported STALE.
    Two decisions that collapse onto one canonical Id must agree, otherwise
    the human has contradicted themselves and the run fails.
    """
    remapped: dict[tuple[str, str], Decision] = {}
    for (kind, ident), decision in sorted(decisions.items()):
        target = renames.get(kind, {}).get(ident, ident)
        key = (kind, target)
        moved = Decision(
            kind=kind,
            id=target,
            decision=decision.decision,
            signature=decision.signature,
            line=decision.line,
        )
        existing = remapped.get(key)
        if existing is None:
            remapped[key] = moved
            continue
        if (existing.decision, existing.signature) != (
            moved.decision,
            moved.signature,
        ):
            raise DecisionError(
                f"decisions.tsv:{decision.line}: the decision for {kind} / "
                f"{ident} renames onto {kind} / {target}, which already has a "
                f"different decision (line {existing.line})"
            )
    return remapped


def resolve_all(
    corpus: Corpus,
    decisions: dict[tuple[str, str], Decision],
    renames: dict[str, dict[str, str]] | None = None,
) -> list[Resolution]:
    """Resolve every (kind, Id) in the corpus, then any pin absent from it."""
    renames = renames or {}
    keys = sorted(corpus.definitions)
    known = set(keys)
    for key in sorted(decisions):
        if key not in known:
            keys.append(key)
    keys.sort()

    resolutions: list[Resolution] = []
    for kind, ident in keys:
        variants = corpus.definitions.get((kind, ident), [])
        decision = decisions.get((kind, ident))
        pinned = decision is not None and not decision.is_todo
        matched = match_pin(decision, variants) if pinned else None

        if pinned and matched is not None:
            resolutions.append(
                Resolution(
                    kind=kind,
                    id=ident,
                    variants=variants,
                    winner=matched,
                    decided_by="pinned",
                    pinned_signature=matched.signature,
                )
            )
            continue

        if variants:
            winner, decided_by, reasons = resolve_variants(variants)
        else:
            winner, decided_by, reasons = None, "rule-1", []
        # A rename decided the Id when it was the only decision to make. A
        # rule or a pin still owns the definition when the merged spellings
        # disagree, so `decided_by` stays honest there.
        if (
            decided_by == "only-variant"
            and ident in renames.get(kind, {}).values()
        ):
            decided_by = "renamed"
        resolution = Resolution(
            kind=kind,
            id=ident,
            variants=variants,
            winner=winner,
            decided_by=decided_by,
            reasons=reasons,
        )
        if pinned or (decision is not None and not variants):
            resolution.stale = True
            resolution.pinned_signature = decision.signature
            if variants:
                reason = (
                    "STALE pin: decision not found in the corpus, retained for a human"
                )
            else:
                reason = (
                    "STALE pin: Id is not present in the corpus, retained for a human"
                )
            resolution.reasons = [reason]
            resolution.decided_by = "pinned"
        elif decision is not None and decision.is_todo:
            resolution.todo = True
        resolutions.append(resolution)
    return resolutions


def conventions_stem(ident: str) -> str:
    """The Id with one trailing `-CONVENTIONS` removed, if it carries one."""
    if ident.endswith(CONVENTIONS_SUFFIX):
        return ident[: -len(CONVENTIONS_SUFFIX)]
    return ident


def stem_template(idents: list[str]) -> str:
    """The shared spelling of hyphen-separated tokens, varying tokens as <X>.

    Falls back to a character common prefix and suffix when the members carry
    different token counts, so every group gets a readable shape.
    """
    parts = [ident.split("-") for ident in idents]
    if len({len(part) for part in parts}) == 1:
        return "-".join(
            parts[0][index]
            if len({part[index] for part in parts}) == 1
            else "<X>"
            for index in range(len(parts[0]))
        )
    shortest = min(len(ident) for ident in idents)
    prefix = ""
    for index in range(shortest):
        if len({ident[index] for ident in idents}) == 1:
            prefix += idents[0][index]
        else:
            break
    suffix = ""
    for size in range(1, shortest + 1):
        if len({ident[-size] for ident in idents}) == 1:
            suffix = idents[0][-size] + suffix
        else:
            break
    return f"{prefix}<X>{suffix}"


def group_shape(idents: list[str]) -> str:
    """A readable template for the group, e.g. `USD-CMS-<X>` or `ICE:<X>`."""
    idents = sorted(idents)
    columns = [ident.split(":") for ident in idents]
    if all(len(column) == 2 for column in columns) and len(
        {column[0] for column in columns}
    ) == 1:
        return f"{columns[0][0]}:{stem_template([c[1] for c in columns])}"
    return stem_template(idents)


@dataclass
class SpellingGroup:
    """Ids in one kind that share one identical definition.

    `pattern` is A (a `-CONVENTIONS` rename), B (a vendor/exchange prefix
    twin), or C (everything else). Only A is resolved; B is reported and C is
    handed to a human.
    """

    kind: str
    signature: str
    ids: list[str]
    pattern: str
    canonical: str | None
    aliases: list[str]
    shape: str

    @property
    def reason(self) -> str:
        if self.pattern == PATTERN_SUFFIX:
            return ""
        if self.pattern == PATTERN_VENDOR_PREFIX:
            return (
                "vendor/exchange prefix twin: only the prefix before ':' "
                "differs, so these are different feeds, never one convention"
            )
        return (
            f"unrecognised shape `{self.shape}`: the Ids do not reduce to one "
            "`-CONVENTIONS` stem, so the tool cannot tell a rename from "
            "genuinely distinct instruments that share a definition"
        )


def classify_spelling_group(
    kind: str, signature: str, idents: list[str]
) -> SpellingGroup:
    """Classify one identical-definition group into pattern A, B, or C."""
    stems = {conventions_stem(ident) for ident in idents}
    if len(stems) == 1:
        stem = next(iter(stems))
        suffixed = sorted(i for i in idents if i.endswith(CONVENTIONS_SUFFIX))
        canonical = f"{stem}{CONVENTIONS_SUFFIX}"
        if len(suffixed) == 1 and suffixed[0] == canonical and all(
            conventions_stem(ident) == stem for ident in idents
        ):
            return SpellingGroup(
                kind=kind,
                signature=signature,
                ids=idents,
                pattern=PATTERN_SUFFIX,
                canonical=canonical,
                aliases=sorted(i for i in idents if i != canonical),
                shape=group_shape(idents),
            )
    if all(":" in ident for ident in idents):
        prefixes = {ident.split(":", 1)[0] for ident in idents}
        tails = {ident.split(":", 1)[1] for ident in idents}
        if len(tails) == 1 and len(prefixes) == len(idents):
            return SpellingGroup(
                kind=kind,
                signature=signature,
                ids=idents,
                pattern=PATTERN_VENDOR_PREFIX,
                canonical=None,
                aliases=[],
                shape=group_shape(idents),
            )
    return SpellingGroup(
        kind=kind,
        signature=signature,
        ids=idents,
        pattern=PATTERN_OTHER,
        canonical=None,
        aliases=[],
        shape=group_shape(idents),
    )


def spelling_groups(
    definitions: dict[tuple[str, str], list[Variant]]
) -> list[SpellingGroup]:
    """Group Ids in one kind that share an identical definition, classified."""
    by_kind_sig: dict[tuple[str, str], list[str]] = defaultdict(list)
    for (kind, ident), variants in definitions.items():
        for variant in variants:
            by_kind_sig[(kind, variant.signature)].append(ident)
    groups: list[SpellingGroup] = []
    for (kind, sig), idents in sorted(by_kind_sig.items()):
        unique = sorted(set(idents))
        if len(unique) < 2:
            continue
        groups.append(classify_spelling_group(kind, sig, unique))
    groups.sort(key=lambda g: (-len(g.ids), g.kind, g.signature))
    return groups


def rename_map(
    groups: list[SpellingGroup],
) -> dict[str, dict[str, str]]:
    """kind -> {alias_id: canonical_id} for every Pattern A group."""
    forward: dict[str, dict[str, str]] = defaultdict(dict)
    for group in groups:
        if group.pattern != PATTERN_SUFFIX or group.canonical is None:
            continue
        for alias in group.aliases:
            forward[group.kind][alias] = group.canonical
    return {kind: dict(aliases) for kind, aliases in forward.items()}


def aliases_by_canonical(
    renames: dict[str, dict[str, str]]
) -> dict[tuple[str, str], list[str]]:
    """(kind, canonical) -> the alias Ids that renamed onto it."""
    reverse: dict[tuple[str, str], list[str]] = defaultdict(list)
    for kind, mapping in renames.items():
        for alias, canonical in mapping.items():
            reverse[(kind, canonical)].append(alias)
    return {key: sorted(set(value)) for key, value in reverse.items()}


def apply_renames(
    definitions: dict[tuple[str, str], list[Variant]],
    renames: dict[str, dict[str, str]],
) -> dict[tuple[str, str], list[Variant]]:
    """Fold each Pattern A alias's variants into its canonical Id.

    The alias Id disappears as a key and its definitions become variants of
    the canonical Id, so a conflicting old spelling still reaches the rule
    instead of being dropped with the rename.
    """
    merged: dict[tuple[str, str], dict[str, Variant]] = defaultdict(dict)
    for (kind, ident), variants in definitions.items():
        target = renames.get(kind, {}).get(ident, ident)
        bucket = merged[(kind, target)]
        for variant in variants:
            existing = bucket.get(variant.signature)
            if existing is None:
                bucket[variant.signature] = Variant(
                    signature=variant.signature,
                    files=variant.files,
                    present_fields=variant.present_fields,
                    nonempty_fields=variant.nonempty_fields,
                    count=variant.count,
                )
            else:
                existing.files = tuple(
                    sorted(set(existing.files) | set(variant.files))
                )
                existing.count += variant.count
    return {
        key: [bucket[sig] for sig in sorted(bucket)]
        for key, bucket in merged.items()
    }


def write_canonical(
    path: Path,
    resolutions: list[Resolution],
    aliases: dict[tuple[str, str], list[str]],
) -> None:
    lines = ["\t".join(CANONICAL_HEADER)]
    for res in resolutions:
        lines.append(
            "\t".join(
                [
                    res.kind,
                    res.id,
                    res.signature,
                    res.source,
                    str(len(res.variants)),
                    res.decided_by,
                    ",".join(aliases.get((res.kind, res.id), [])),
                ]
            )
        )
    _write(path, lines)


def write_aliases(path: Path, groups: list[SpellingGroup]) -> None:
    """One row per resolved rename: the alias and the spelling it became."""
    lines = ["\t".join(ALIASES_HEADER)]
    rows: set[tuple[str, str, str, str]] = set()
    for group in groups:
        if group.pattern != PATTERN_SUFFIX or group.canonical is None:
            continue
        for alias in group.aliases:
            rows.add((group.kind, alias, group.canonical, group.pattern))
    for kind, alias, canonical, pattern in sorted(rows):
        lines.append("\t".join([kind, alias, canonical, pattern]))
    _write(path, lines)


def write_review(
    path: Path,
    resolutions: list[Resolution],
    groups: list[SpellingGroup],
) -> None:
    lines = ["\t".join(REVIEW_HEADER)]
    for res in resolutions:
        if not res.reasons:
            continue
        lines.append(
            "\t".join(
                [
                    res.kind,
                    res.id,
                    str(len(res.variants)),
                    res.signature,
                    res.runner_up(),
                    "; ".join(res.reasons),
                ]
            )
        )
    # Pattern C groups need a human: the tool will not guess whether the Ids
    # are one renamed convention or several instruments that share a shape.
    for group in groups:
        if group.pattern != PATTERN_OTHER:
            continue
        lines.append(
            "\t".join(
                [
                    group.kind,
                    ", ".join(group.ids),
                    str(len(group.ids)),
                    group.signature,
                    "",
                    f"pattern-C ({group.shape}): {group.reason}",
                ]
            )
        )
    _write(path, lines)


def write_resolutions(
    path: Path,
    corpus: Corpus,
    resolutions: list[Resolution],
    summary: list[dict],
    groups: list[SpellingGroup],
    renames: dict[str, dict[str, str]],
) -> None:
    flagged = [r for r in resolutions if r.reasons and not r.stale]
    stale = [r for r in resolutions if r.stale]
    todo = [r for r in resolutions if r.todo]
    conflicts = [r for r in resolutions if len(r.variants) > 1]
    conflicts.sort(key=lambda r: (-len(r.variants), r.kind, r.id))
    pair_count = sum(
        len(group.ids) * (len(group.ids) - 1) // 2 for group in groups
    )
    pattern_a = [g for g in groups if g.pattern == PATTERN_SUFFIX]
    pattern_b = [g for g in groups if g.pattern == PATTERN_VENDOR_PREFIX]
    pattern_c = [g for g in groups if g.pattern == PATTERN_OTHER]
    alias_count = sum(len(mapping) for mapping in renames.values())

    out: list[str] = []
    out.append("# Instrument conventions — resolution audit")
    out.append("")
    out.append("Canonical definition per (kind, Id), chosen by the documented rule")
    out.append("and overridden by `decisions.tsv` where a human has decided.")
    out.append("A definition is the normalised signature: whitespace-collapsed text,")
    out.append("fields sorted by tag name, a field present with an empty value kept")
    out.append("present. Values are never rewritten.")
    out.append("")
    out.append("## Stale pins")
    out.append("")
    if stale:
        out.append(
            "These pins no longer match any variant in the corpus. The pin is kept; "
            "a human must choose a replacement or withdraw it."
        )
        out.append("")
        out.append("| kind | id | pinned signature | decision |")
        out.append("|---|---|---|---|")
        for res in stale:
            out.append(
                f"| {res.kind} | `{res.id}` | `{res.pinned_signature}` | "
                f"`{res.decided_by}` |"
            )
    else:
        out.append("None. Every pin still matches a corpus variant.")
    out.append("")
    out.append("## Flagged for human review")
    out.append("")
    if flagged:
        out.append(
            "The decision rests on rule 3 (non-legacy path) or rule 4 "
            "(lexicographic signature), or the rule-1 field-count winner is not "
            "the rule-2 plurality winner."
        )
        out.append("")
        out.append("| kind | id | winner | why |")
        out.append("|---|---|---|---|")
        for res in flagged:
            out.append(
                f"| {res.kind} | `{res.id}` | `{res.signature}` | "
                f"{'; '.join(res.reasons)} |"
            )
    else:
        out.append("None. Every conflict had a clear field-count and file-count winner.")
    out.append("")
    out.append("## Awaiting a decision (TODO)")
    out.append("")
    if todo:
        out.append(
            "`decisions.tsv` carries a TODO row for these Ids: a human has marked "
            "them as needing a decision."
        )
        out.append("")
        for res in todo:
            out.append(f"- {res.kind} / `{res.id}`")
    else:
        out.append("None.")
    out.append("")
    out.append("## Summary")
    out.append("")
    out.append("| kind | entries | ids | conflicting ids | flagged ids | pinned ids |")
    out.append("|---|---:|---:|---:|---:|---:|")
    pinned_by_kind: dict[str, int] = defaultdict(int)
    for res in resolutions:
        if res.decided_by == "pinned" and not res.stale:
            pinned_by_kind[res.kind] += 1
    flagged_by_kind: dict[str, int] = defaultdict(int)
    for res in flagged:
        flagged_by_kind[res.kind] += 1
    for row in summary:
        out.append(
            f"| {row['kind']} | {row['entries']} | {row['ids']} | "
            f"{row['conflicting']} | {flagged_by_kind[row['kind']]} | "
            f"{pinned_by_kind[row['kind']]} |"
        )
    out.append(
        f"| **total** | **{sum(r['entries'] for r in summary)}** | "
        f"**{sum(r['ids'] for r in summary)}** | "
        f"**{sum(r['conflicting'] for r in summary)}** | "
        f"**{len(flagged)}** | **{sum(pinned_by_kind.values())}** |"
    )
    out.append("")
    out.append("## Corpus defects")
    out.append("")
    out.append(
        "Documents that look like conventions but do not parse, entries without "
        "an Id, and (kind, Id) pairs defined more than once inside one file."
    )
    out.append("")
    defects = corpus.parse_failures or corpus.missing_id or corpus.duplicate_in_file
    if not defects:
        out.append("None.")
    for rel, why in corpus.parse_failures:
        out.append(f"- `{rel}` — {why}")
    for rel, kind in corpus.missing_id:
        out.append(f"- `{rel}` — `<{kind}>` entry has no `<Id>`")
    for rel, kind, what, status in corpus.duplicate_in_file:
        out.append(f"- `{rel}` — {kind} `{what}` ({status} definition)")
    out.append("")
    out.append("## Conflicting Ids")
    out.append("")
    for res in conflicts:
        marks = []
        if res.reasons and not res.stale:
            marks.append("FLAGGED FOR REVIEW")
        if res.stale:
            marks.append("STALE PIN")
        if res.todo:
            marks.append("TODO")
        mark = f" — **{' / '.join(marks)}**" if marks else ""
        out.append(f"### {res.kind} / {res.id}{mark}")
        out.append("")
        out.append(
            f"Winner: `{res.signature}` from `{res.source}` "
            f"({len(res.winner.files) if res.winner else 0} file(s)) — "
            f"decided by {res.decided_by}."
        )
        out.append("")
        out.append(
            "| | non-empty fields | present fields | files | legacy paths | definition |"
        )
        out.append("|---|---:|---:|---:|---|---|")
        for index, variant in enumerate(res.ordered_variants(), start=1):
            chosen = "✓ " if res.winner and variant.signature == res.winner.signature else ""
            out.append(
                f"| {chosen}{index} | {variant.nonempty_fields} | "
                f"{variant.present_fields} | {len(variant.files)} | "
                f"{legacy_label(variant.files)} | `{variant.signature}` |"
            )
        out.append("")
        out.append("Supporting files per variant:")
        out.append("")
        for index, variant in enumerate(res.ordered_variants(), start=1):
            shown = ", ".join(f"`{f}`" for f in variant.files)
            out.append(f"- variant {index}: {shown}")
        out.append("")
    out.append("## Id-spelling groups")
    out.append("")
    out.append(
        "Ids in the same kind that carry an identical normalised definition but "
        "differ in spelling. Each group is classified by the shape of the "
        "difference and handled by its pattern: A is resolved, B is reported, "
        "C is handed to a human."
    )
    out.append("")
    out.append(
        f"Groups: {len(groups)}. Id pairs: {pair_count}. "
        f"Pattern A (resolved renames): {len(pattern_a)}. "
        f"Pattern B (vendor prefix, never merged): {len(pattern_b)}. "
        f"Pattern C (human): {len(pattern_c)}. "
        f"Aliases recorded: {alias_count}."
    )
    out.append("")
    out.append("### Pattern A — `-CONVENTIONS` renames, resolved")
    out.append("")
    out.append(
        "Every member strips to a single stem, so the group is one convention "
        "under two spellings. The `-CONVENTIONS` form is canonical and the "
        "other spelling is recorded as an alias, not as a second entry."
    )
    out.append("")
    if pattern_a:
        out.append("| kind | canonical | aliases | definition |")
        out.append("|---|---|---|---|")
        for group in pattern_a:
            alias_text = ", ".join(f"`{a}`" for a in group.aliases)
            out.append(
                f"| {group.kind} | `{group.canonical}` | {alias_text} | "
                f"`{digest_of(group.signature)}` |"
            )
    else:
        out.append("None.")
    out.append("")
    out.append("### Pattern B — vendor/exchange prefix twins (informational)")
    out.append("")
    out.append(
        "These share a stem after the first `:` and differ only in the prefix "
        "before it. They are different feeds, not a rename, so both spellings "
        "stay canonical and no human action is required."
    )
    out.append("")
    if pattern_b:
        out.append("| kind | ids | definition |")
        out.append("|---|---|---|")
        for group in pattern_b:
            ids = ", ".join(f"`{ident}`" for ident in group.ids)
            out.append(
                f"| {group.kind} | {ids} | `{digest_of(group.signature)}` |"
            )
    else:
        out.append("None.")
    out.append("")
    out.append("### Pattern C — unrecognised, flagged for a human")
    out.append("")
    out.append(
        "The Ids do not reduce to one `-CONVENTIONS` stem, so the tool cannot "
        "tell a rename from genuinely distinct instruments that share a "
        "definition. The shape column gives the common spelling with `<X>` at "
        "each varying token; these groups are also listed in "
        "`conventions-review.tsv`."
    )
    out.append("")
    if pattern_c:
        by_kind_c: dict[str, list[SpellingGroup]] = defaultdict(list)
        for group in pattern_c:
            by_kind_c[group.kind].append(group)
        out.append("| kind | ids | shape | definition |")
        out.append("|---|---|---|---|")
        for kind in sorted(by_kind_c):
            for group in by_kind_c[kind]:
                ids = ", ".join(f"`{ident}`" for ident in group.ids)
                out.append(
                    f"| {kind} | {ids} | `{group.shape}` | "
                    f"`{digest_of(group.signature)}` |"
                )
    else:
        out.append("None.")
    out.append("")
    _write(path, out)


def _write(path: Path, lines: list[str]) -> None:
    """Write text deterministically: UTF-8, LF, one trailing newline."""
    with path.open("w", encoding="utf-8", newline="\n") as handle:
        handle.write("\n".join(lines))
        handle.write("\n")


@dataclass
class RunResult:
    files: int
    kinds: int
    entries: int
    canonical_ids: int
    conflicting: int
    flagged: int
    stale: int
    todo: int
    pinned: int
    spelling_groups: int
    spelling_pairs: int
    pattern_a: int
    pattern_b: int
    pattern_c: int
    aliases: int
    resolutions: list[Resolution] = field(default_factory=list)


def run(examples: Path, decisions_path: Path, out_dir: Path) -> RunResult:
    """Extract, overlay, and write the outputs. Returns the counts."""
    if not examples.is_dir():
        raise DecisionError(f"no such examples directory: {examples}")
    decisions = load_decisions(decisions_path)
    corpus = parse_corpus(examples)
    groups = spelling_groups(corpus.definitions)
    renames = rename_map(groups)
    aliases = aliases_by_canonical(renames)
    corpus.definitions = apply_renames(corpus.definitions, renames)
    decisions = remap_decisions(decisions, renames)
    resolutions = resolve_all(corpus, decisions, renames)

    kinds = sorted(corpus.entries_per_kind)
    summary = []
    for kind in kinds:
        ids = [r for r in resolutions if r.kind == kind]
        summary.append(
            {
                "kind": kind,
                "entries": corpus.entries_per_kind[kind],
                "ids": len(ids),
                "conflicting": sum(1 for r in ids if len(r.variants) > 1),
            }
        )

    out_dir.mkdir(parents=True, exist_ok=True)
    write_canonical(
        out_dir / "conventions-canonical.tsv", resolutions, aliases
    )
    write_aliases(out_dir / "conventions-aliases.tsv", groups)
    write_review(out_dir / "conventions-review.tsv", resolutions, groups)
    write_resolutions(
        out_dir / "conventions-resolutions.md",
        corpus,
        resolutions,
        summary,
        groups,
        renames,
    )

    flagged = [r for r in resolutions if r.reasons and not r.stale]
    return RunResult(
        files=corpus.files,
        kinds=len(kinds),
        entries=sum(corpus.entries_per_kind.values()),
        canonical_ids=len(resolutions),
        conflicting=sum(1 for r in resolutions if len(r.variants) > 1),
        flagged=len(flagged),
        stale=sum(1 for r in resolutions if r.stale),
        todo=sum(1 for r in resolutions if r.todo),
        pinned=sum(
            1 for r in resolutions if r.decided_by == "pinned" and not r.stale
        ),
        spelling_groups=len(groups),
        spelling_pairs=sum(
            len(g.ids) * (len(g.ids) - 1) // 2 for g in groups
        ),
        pattern_a=sum(1 for g in groups if g.pattern == PATTERN_SUFFIX),
        pattern_b=sum(
            1 for g in groups if g.pattern == PATTERN_VENDOR_PREFIX
        ),
        pattern_c=sum(1 for g in groups if g.pattern == PATTERN_OTHER),
        aliases=sum(len(mapping) for mapping in renames.values()),
        resolutions=resolutions,
    )


def main(argv: list[str] | None = None) -> int:
    default_decisions = Path(__file__).resolve().parent / "decisions.tsv"
    parser = argparse.ArgumentParser(
        description=(
            "Extract the one canonical instrument-convention set from the ORE "
            "examples, overlaying the checked-in human decisions."
        )
    )
    parser.add_argument("--examples", default="external/ore/examples")
    parser.add_argument("--decisions", default=str(default_decisions))
    parser.add_argument("--out", default="tmp/ore_conventions/")
    args = parser.parse_args(argv)

    try:
        result = run(Path(args.examples), Path(args.decisions), Path(args.out))
    except DecisionError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2

    print(
        f"files={result.files} kinds={result.kinds} entries={result.entries} "
        f"canonical_ids={result.canonical_ids} conflicting={result.conflicting} "
        f"flagged={result.flagged} stale={result.stale} todo={result.todo} "
        f"pinned={result.pinned} spelling_groups={result.spelling_groups} "
        f"spelling_pairs={result.spelling_pairs} pattern_a={result.pattern_a} "
        f"pattern_b={result.pattern_b} pattern_c={result.pattern_c} "
        f"aliases={result.aliases}"
    )
    for res in result.resolutions:
        if res.stale:
            print(
                f"STALE pin: {res.kind} / {res.id} — decision "
                f"`{res.pinned_signature}` matches no corpus variant; retained."
            )
    return 0


if __name__ == "__main__":
    sys.exit(main())
