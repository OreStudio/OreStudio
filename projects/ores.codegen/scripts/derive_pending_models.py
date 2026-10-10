#!/usr/bin/env python3
"""Derive the pending-change model of every gated entity model.

Usage:
    derive_pending_models.py [--check] [MODEL ...]

With no model, every ``projects/*/modeling/ores.*.org`` marked gated is derived.
``--check`` writes nothing and fails when a derived model is stale. The
derivation lives in ``codegen.pending_change``.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "src"))

from codegen.pending_change import main  # noqa: E402

if __name__ == "__main__":
    sys.exit(main())
