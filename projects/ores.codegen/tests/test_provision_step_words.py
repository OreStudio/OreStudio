"""The words a provisioning step is shown by, on both sides of the boundary.

The server declares a label and a description per step kind. A screen shows
them by looking the exact sentence up in its own catalogue, which is what lets
the text be translated without the browser holding a step catalogue and
without the server knowing about languages. Nothing in either language checks
that the two agree, so this does, in both directions:

- every sentence the kind table declares is a key in all three catalogues, so
  a step cannot reach a screen in English alone;
- no sentence sits in a catalogue that the table no longer declares, which is
  what a reworded sentence leaves behind, and which would silently stop being
  consulted.

The sentences are read out of the C++ table rather than restated here, because
a list copied into a test is a list that drifts from the thing it guards.
"""

import re
from pathlib import Path

import pytest

CHECKOUT = Path(__file__).resolve().parents[3]
WORDS_TABLE = (
    CHECKOUT / "projects/ores.iam/api/include/ores.iam.api/workflow/provision_tenant_workflow.hpp"
)
CATALOGUES = {
    "en": CHECKOUT / "projects/ores.web/packages/web/src/i18n/locales/en.ts",
    "fr": CHECKOUT / "projects/ores.web/packages/web/src/i18n/locales/fr.ts",
    "pt": CHECKOUT / "projects/ores.web/packages/web/src/i18n/locales/pt.ts",
}

# A C++ string literal without its quotes. Adjacent literals are one sentence:
# the table wraps a long sentence across lines.
_LITERAL = re.compile(r'"((?:[^"\\]|\\.)*)"')

# The catalogue group the server's own sentences live in, and the keys inside
# it. Prettier puts one key per line, quoted with either kind of quote. The
# markers start at the newline so a nested `server` key, which shares the
# name, cannot be taken for the group.
_GROUP_START = "\n    server: {"
_GROUP_END = "\n    },"
_KEY = re.compile(
    r"^\s+(?:'((?:[^'\\]|\\.)*)'|\"((?:[^\"\\]|\\.)*)\"|([A-Za-z_][A-Za-z0-9_]*)):",
    re.M,
)


def _unescape(raw: str) -> str:
    return raw.replace("\\'", "'").replace('\\"', '"')


def declared_sentences() -> list[str]:
    """Every sentence the kind table declares, in the order it declares them.

    Only the literals inside the function's `return {...}` blocks are sentences:
    the kind names it compares against are literals too, and they are not words
    a person reads.
    """
    text = WORDS_TABLE.read_text()
    start = text.index("inline step_kind_words words_for_step_kind")
    body = text[start : text.index("\n}\n", start)]

    sentences: list[str] = []
    for block in re.finditer(r"return \{([^}]*)\}", body):
        group: list[str] = []
        previous_end = 0
        for literal in _LITERAL.finditer(block.group(1)):
            if group and "," in block.group(1)[previous_end : literal.start()]:
                sentences.append("".join(group))
                group = []
            group.append(literal.group(1))
            previous_end = literal.end()
        if group:
            sentences.append("".join(group))
    return sentences


def catalogue_keys(language: str) -> set[str]:
    """The sentences one catalogue translates, which are the server's own."""
    text = CATALOGUES[language].read_text()
    # Past the group's own opening line: its key is the group's name, not one
    # of the sentences inside it.
    start = text.index(_GROUP_START) + len(_GROUP_START)
    group = text[start : text.index(_GROUP_END, start)]
    found: set[str] = set()
    for key in _KEY.finditer(group):
        found.add(_unescape(next(part for part in key.groups() if part is not None)))
    return found


def test_the_kind_table_declares_a_label_and_a_description_for_each_step():
    sentences = declared_sentences()

    assert sentences, "the kind table declares no words at all"
    assert len(sentences) % 2 == 0, sentences
    assert all(sentence != "" for sentence in sentences), sentences


@pytest.mark.parametrize("language", sorted(CATALOGUES))
def test_every_declared_sentence_is_translated(language: str):
    translated = catalogue_keys(language)
    missing = [sentence for sentence in declared_sentences() if sentence not in translated]

    assert missing == [], f"{language} carries no translation for: {missing}"


@pytest.mark.parametrize("language", sorted(CATALOGUES))
def test_no_translation_outlives_the_sentence_it_translates(language: str):
    declared = set(declared_sentences())
    stale = sorted(sentence for sentence in catalogue_keys(language) if sentence not in declared)

    assert stale == [], f"{language} translates sentences the server no longer sends: {stale}"
