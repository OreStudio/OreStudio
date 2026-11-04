#!/usr/bin/env python3
"""Break the wire codec of ores::dq::messaging::artefact_type_write one way.

Run from the repository root::

    python3 <this file> <mutation>     apply one mutation
    python3 <this file> revert         restore both files from git

Each mutation edits one generated file, so the test that must catch it is the
generated case ores::dq::messaging::artefact_type_write in ores.dq.api.tests.

Revert before you apply another mutation, or the two stack. The revert restores
both files from git, so it also discards any other uncommitted edit to them.
"""
import subprocess
import sys
from pathlib import Path

HEADER = Path("projects/ores.dq/api/include/ores.dq.api/messaging/artefact_type_protocol.hpp")
CODEC = Path("projects/ores.dq/api/src/messaging/artefact_type_protocol_codec.cpp")
T = "ores::dq::messaging::artefact_type_write"

READ_JSON = f"    return object::read_json<{T}>(text);\n"
READ_MSGPACK = f"    return object::read_msgpack<{T}>(data);\n"


def read_then(call, statement):
    return call.replace("    return ", "    auto r = ").removesuffix(";\n") + ";\n" + \
        f"    {statement}\n    return r;\n"


MUTATIONS = {
    "drop-member": (HEADER,
                    '        utility::codec::field{"target_subject", '
                    '&artefact_type_write::target_subject},\n',
                    ""),
    "json-loses-value": (CODEC, READ_JSON, read_then(READ_JSON, "r.description.reset();")),
    "msgpack-loses-value": (CODEC, READ_MSGPACK, read_then(READ_MSGPACK, "r.display_order = 0;")),
    "same-type-mixup": (CODEC, READ_JSON, read_then(READ_JSON, "r.name = r.code;")),
}


def main(which):
    if which == "revert":
        subprocess.run(["git", "checkout", "--", str(HEADER), str(CODEC)], check=True)
        return
    path, old, new = MUTATIONS[which]
    text = path.read_text(encoding="utf-8")
    if text.count(old) != 1:
        sys.exit(f"{which}: expected one match in {path}, found {text.count(old)}")
    path.write_text(text.replace(old, new), encoding="utf-8")
    print(f"{which}: applied to {path}")


if __name__ == "__main__":
    main(sys.argv[1])
