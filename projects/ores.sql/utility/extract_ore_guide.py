#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
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
"""Extract from the official ORE user guide what the file type seed needs.

The guide says, for every analytic, which files it reads and writes, gives the
canonical name of each in its XML listings, and explains the parameters in prose.
It also describes each configuration document. The seed script reads the result
from file_type_guide.json, so the seed does not depend on the guide being
present. Run this script again when the guide changes.

    extract_ore_guide.py [--guide /path/to/ORE/Engine]
"""

import argparse
import json
import os
import re
import subprocess

DEFAULT_GUIDE = "/home/marco/Vault/Development/ORE/Engine"
OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "file_type_guide.json")

# The guide chapter that describes each document, by the well known name of the file.
DOCUMENT_CHAPTERS = {
    "calendaradjustment.xml": "calendaradjustment.tex",
    "collateralbalances.xml": "collateralbalances.tex",
    "conventions.xml": "conventions.tex",
    "counterparty.xml": "counterpartyinformation.tex",
    "curveconfig.xml": "curveconfig.tex",
    "ibor_fallback.xml": "iborfallbackconfig.tex",
    "referencedata.xml": "referencedata.tex",
    "sensitivity.xml": "sensitivity.tex",
    "simulation.xml": "simulation.tex",
    "stresstest.xml": "stressconfig.tex",
    "todaysmarket.xml": "todaysmarket.tex",
    "historicalreturnconfig.xml": "historicalreturnconfiguration.tex",
    "simmcalibration.xml": "simmcalibration.tex",
}

# Chapters one level up in the guide.
TOP_CHAPTERS = {
    "netting.xml": "nettingdata.tex",
    "fixings.txt": "fixingdata.tex",
    "market.txt": "marketdata.tex",
}


def clean(text):
    """LaTeX to plain text, enough for a sentence of documentation."""
    text = re.sub(r"(?m)(?<!\\)%.*$", "", text)
    text = re.sub(r"\\todo\[[^\]]*\]\{[^}]*\}", "", text)
    text = re.sub(r"\\(?:begin|end)\{[^}]*\}", " ", text)
    text = re.sub(r"\\(label|index|cite|footnote)\{[^}]*\}", "", text)
    text = re.sub(r"\\ref\{[^}]*\}", "the guide", text)
    text = re.sub(r"\\(?:tt|textbf|emph|textit|texttt|bf|it)\s*\{([^{}]*)\}", r"\1", text)
    text = re.sub(r"\{\\(?:tt|bf|it|em)\s+([^{}]*)\}", r"\1", text)
    text = re.sub(r"\\(?:tt|bf|it|em)\s+", "", text)
    text = text.replace("\\_", "_").replace("\\&", "&").replace("\\%", "%").replace("\\#", "#")
    text = text.replace("``", '"').replace("''", '"').replace("~", " ")
    text = re.sub(r"\\\\", " ", text)
    text = re.sub(r"\$([^$]*)\$", r"\1", text)
    text = re.sub(r"\\[a-zA-Z]+\*?(\[[^\]]*\])?", "", text)
    text = text.replace("\\ ", " ").replace("{", "").replace("}", "")
    text = re.sub(r"\s[!|]\s?(?=[A-Za-z])", " ", text)
    return re.sub(r"\s+", " ", text).strip()


def sections_of(tex):
    """The sections of ore.tex: a title and the text up to the next section."""
    parts = re.split(r"(?m)^(\\subsection\{[^\n]*|\\subsubsection\*?\{[^\n]*)$", tex)
    out = [("preamble", parts[0])]
    for i in range(1, len(parts), 2):
        title = clean(re.sub(r"\\label\{[^}]*\}", "", parts[i]))
        title = re.sub(r"^(subsection|subsubsection)\*?", "", title).strip()
        out.append((title, parts[i + 1]))
    return out


def parameters_of(block):
    return re.findall(r'<Parameter name="(\w+)">([^<]*)</Parameter>', block)


def sentences_with(text, name):
    """The sentences of the prose that mention a parameter."""
    found = []
    for sentence in re.split(r"(?<=[.!?])\s+", text):
        if re.search(r"(?<![A-Za-z])" + re.escape(name) + r"(?![A-Za-z])", sentence, re.I):
            found.append(sentence.strip())
    return found


def extract_ore_tex(path):
    tex = open(path).read()
    examples = {}
    notes = {}
    for title, body in sections_of(tex):
        listings = re.findall(r"\\begin\{minted\}[^\n]*\n(.*?)\\end\{minted\}", body, re.S)
        prose = re.sub(r"\\begin\{minted\}.*?\\end\{minted\}", " ", body, flags=re.S)
        bullets = {}
        for m in re.finditer(r"\\item\s*\{\\tt\s+([A-Za-z0-9_]+)\s*:?\}\s*:?(.*?)(?=\\item|\\end\{itemize\})",
                             prose, re.S):
            bullets.setdefault(m.group(1), clean(m.group(2)))
        plain = clean(re.sub(r"\\item\s*\{\\tt\s+[A-Za-z0-9_]+\s*:?\}", " ", prose))
        for listing in listings:
            setup = re.search(r"<Setup>(.*?)</Setup>", listing, re.S)
            if setup:
                for name, value in parameters_of(setup.group(1)):
                    examples.setdefault(f"setup|{name}", {"value": value.strip(), "section": title})
            for analytic, inner in re.findall(r'<Analytic type="(\w+)">(.*?)</Analytic>', listing, re.S):
                for name, value in parameters_of(inner):
                    key = f"{analytic}|{name}"
                    if value.strip() and key not in examples:
                        examples[key] = {"value": value.strip(), "section": title}
        for key, example in list(examples.items()):
            if example["section"] != title:
                continue
            name = key.split("|", 1)[1]
            snippet = bullets.get(name)
            if snippet is None:
                snippet = " ".join(sentences_with(plain, name)[:3])
            if snippet:
                notes[key] = snippet
    return examples, notes


def first_paragraphs(path, count=3, limit=1400):
    if not os.path.exists(path):
        return None
    tex = open(path).read()
    tex = re.sub(r"\\begin\{(listing|minted|longtable|table|figure)\}.*?\\end\{\1\}", " ", tex, flags=re.S)
    paragraphs = []
    for block in re.split(r"\n\s*\n", tex):
        text = clean(block)
        if len(text) < 60 or text.startswith(("section", "subsection")):
            continue
        paragraphs.append(text)
        if len(paragraphs) == count:
            break
    text = " ".join(paragraphs)
    return text[:limit].rsplit(" ", 1)[0] if len(text) > limit else text


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--guide", default=DEFAULT_GUIDE)
    args = parser.parse_args()
    user_guide = os.path.join(args.guide, "Docs", "UserGuide")
    examples, notes = extract_ore_tex(os.path.join(user_guide, "parameterisation", "ore.tex"))

    documents = {}
    for name, chapter in DOCUMENT_CHAPTERS.items():
        text = first_paragraphs(os.path.join(user_guide, "parameterisation", chapter))
        if text:
            documents[name] = {"chapter": f"parameterisation/{chapter}", "text": text}
    for name, chapter in TOP_CHAPTERS.items():
        text = first_paragraphs(os.path.join(user_guide, chapter))
        if text:
            documents[name] = {"chapter": chapter, "text": text}

    commit = subprocess.run(["git", "-C", args.guide, "rev-parse", "HEAD"],
                            capture_output=True, text=True).stdout.strip()
    result = {
        "source": "ORE user guide, Docs/UserGuide in the OpenSourceRisk Engine",
        "guide_commit": commit,
        "examples": examples,
        "notes": notes,
        "documents": documents,
    }
    with open(OUT, "w") as f:
        json.dump(result, f, indent=2, ensure_ascii=False, sort_keys=True)
        f.write("\n")
    print(f"Wrote {OUT}: {len(examples)} parameter examples, {len(notes)} notes, "
          f"{len(documents)} documents")


if __name__ == "__main__":
    main()
