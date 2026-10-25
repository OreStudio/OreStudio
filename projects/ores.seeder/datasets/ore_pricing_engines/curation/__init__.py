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
"""The curated facts about the pricing engine data, read from the curation files.

Extraction reads what the ORE guide and code say. Curation records what a person
decided, and why: an authored text, a name the guide misspells, a product type's
instrument, a global parameter. One file per concern, each entry keyed by the row it
changes, each entry carrying a reason, a source, a source reference, a curator and a
date.

The files are the source of truth. A generator reads them; a person edits them. See
doc/knowledge/how-the-pricing-engine-data-is-sourced-and-curated.org.
"""

import json
import os

DIRECTORY = os.path.dirname(os.path.abspath(__file__))

PROVENANCE = ("reason", "source", "source_ref", "curator", "date")

SOURCE_KINDS = ("ore_guide", "ore_code", "authored", "seed")

NAME_SECTIONS = ("types", "models", "engines", "settings", "label_words")

SETTING_SECTIONS = ("text", "not_a_setting")


class CurationError(ValueError):
    pass


def _read(directory, name):
    path = os.path.join(directory, name)
    if not os.path.exists(path):
        raise CurationError(f"no curation file {path}")
    with open(path, encoding="utf-8") as handle:
        return json.load(handle)


def _require(entry, keys, where):
    bad = [key for key in keys + PROVENANCE
           if not isinstance(entry.get(key), str) or entry[key].strip() == ""]
    if bad:
        raise CurationError(f"{where} has no {', '.join(bad)}")
    if entry["source"] not in SOURCE_KINDS:
        raise CurationError(f"{where} has an unknown source {entry['source']!r}")
    return entry


def _entries(section, keys, where):
    for key, entry in section.items():
        _require(entry, keys, f"{where}: {key}")
    return section


class Curation:
    """The curation files, checked for the fields every entry must carry."""

    def __init__(self, directory=DIRECTORY):
        self.directory = directory
        self.types = self._load("types.json", ("description", "instrument"))
        names = self._sections("names.json", NAME_SECTIONS)
        self.names = {kind: _entries(section, ("value",), f"names.json {kind}")
                      for kind, section in names.items()}
        self.text_fixes = self._load("text-fixes.json", ("value",))
        settings = self._sections("settings.json", SETTING_SECTIONS)
        self.setting_texts = _entries(settings["text"], ("value",), "settings.json text")
        self.not_settings = _entries(settings["not_a_setting"], (),
                                     "settings.json not_a_setting")
        self.models = self._load("models.json", ("description", "detail"))
        self.engines = self._load("engines.json", ("description", "detail"))
        self.templates = self._load("templates.json",
                                    ("asset_class", "method", "description"))
        self.code_settings = self._load("settings-code.json",
                                        ("scope", "description", "source_ref"))
        for name, entry in self.code_settings.items():
            if entry["scope"] not in ("model", "engine"):
                raise CurationError(f"settings-code.json: {name} has an unknown scope")
        self.globals = self._load("globals.json",
                                  ("domain", "label", "help", "description"))
        for name, entry in self.globals.items():
            if "default_text" not in entry:
                raise CurationError(f"globals.json: {name} has no default_text")

    def _load(self, name, keys):
        return _entries(_read(self.directory, name), keys, name)

    def _sections(self, name, expected):
        data = _read(self.directory, name)
        unknown = set(data) - set(expected)
        if unknown:
            raise CurationError(f"{name} has an unknown section: {sorted(unknown)}")
        missing = set(expected) - set(data)
        if missing:
            raise CurationError(f"{name} has no section: {sorted(missing)}")
        return data

    def rename(self, kind, name):
        """The curated name of a thing, or the name it came in with."""
        entry = self.names.get(kind, {}).get(name)
        return entry["value"] if entry else name

    def label(self, name):
        """A curated label word, or None when the name needs no fixing."""
        entry = self.names.get("label_words", {}).get(name)
        return entry["value"] if entry else None

    def fix_text(self, text):
        for wrong, entry in self.text_fixes.items():
            text = text.replace(wrong, entry["value"])
        return text

    def setting_text(self, name):
        entry = self.setting_texts.get(name)
        return entry["value"] if entry else None

    def is_a_setting(self, name):
        return name not in self.not_settings

    def documentation(self, code):
        """The authored documentation of a product type, or None."""
        entry = self.types.get(code)
        return entry.get("documentation") if entry else None


CURATED = Curation()
