#!/bin/bash
#
# DEPRECATED — superseded by ores.seeder.
#
# Use instead:
#   ./projects/ores.seeder/seeder.sh generate slovaris
#
# This script is retained only as a redirect to avoid breaking any
# existing invocations. It will be removed when the legacy-scripts
# decommission story is resolved.

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo "⚠️  generate_solvaris_refdata.sh is deprecated." >&2
echo "   Use: ./projects/ores.seeder/seeder.sh generate slovaris" >&2
exec "$SCRIPT_DIR/../ores.seeder/seeder.sh" generate slovaris
