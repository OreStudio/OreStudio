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

# A group of engine run settings states the column its value travels under, the
# ORE name that documents it, and the value set that states its control. The
# set is the control, so a setting that states one as well contradicts it.
GROUP_SECTIONS = ("value_sets", "groups")

GROUP_SETTING_KEYS = ("column", "ore", "value_set")

CONTROL_KEYS = ("kind", "options", "minimum", "maximum")


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
        # A template states the class it belongs to and the method it is priced
        # by, as the codes the two lookup tables hold. The class is null on a
        # trade-level template, so the key is required to be present and its
        # value is not required to be a code.
        self.templates = self._load("templates.json",
                                    ("method_code", "description"))
        for code, entry in self.templates.items():
            if "asset_class_code" not in entry:
                raise CurationError(f"templates.json: {code} has no asset_class_code")
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
        self.engine_settings_groups = self._groups("engine-settings-groups.json")

    def _groups(self, name):
        """The engine run settings, whose sets state the control of a setting.

        A set is a curated fact like any other and carries the provenance every
        entry carries. A setting names its set and states no control of its own:
        the set is the control, and two statements of it would drift.
        """
        data = self._sections(name, GROUP_SECTIONS)
        for code, entry in data["value_sets"].items():
            _require(entry, ("name", "description", "source_kind"), f"{name} set {code}")
        for group in data["groups"]:
            where = f"{name} group {group.get('code', '')}"
            for key in ("code", "name", "description"):
                if not str(group.get(key) or "").strip():
                    raise CurationError(f"{where} has no {key}")
            for setting in group.get("settings", []):
                for key in GROUP_SETTING_KEYS:
                    if not str(setting.get(key) or "").strip():
                        raise CurationError(
                            f"{where}: the setting {setting.get('ore', '')} has no {key}")
                stated = [key for key in CONTROL_KEYS if setting.get(key) is not None]
                if stated:
                    raise CurationError(
                        f"{where}: the setting {setting['ore']} states the control "
                        f"{', '.join(stated)} beside its set")
                if setting["value_set"] not in data["value_sets"]:
                    raise CurationError(
                        f"{where}: the setting {setting['ore']} names the set "
                        f"{setting['value_set']!r}, which the file does not state")
        return data

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
