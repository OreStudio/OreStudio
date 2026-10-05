#!/usr/bin/env bash
# Measures what remains of workspaces in the tree. Run from the repository
# root; every count is over tracked files at HEAD.
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

echo "commit $(git rev-parse --short HEAD)"

echo
echo "## Tables with a workspace_id column, by SQL directory"
git grep -l -E '^\s*"?workspace_id"?\s+uuid' -- 'projects/ores.sql/create/*' \
    | awk -F/ '{print $4}' | sort | uniq -c

echo
echo "## Models that set has_workspace_id true"
git grep -l ":has_workspace_id: true" -- 'projects/*/modeling/*.org'

echo
echo "## Models bound to a profile whose default sets has_workspace_id true"
git grep -l -E ":profile:.*(fk-scoped-child|fk_scoped_child|workspace-scoped-lookup|workspace_scoped_lookup)" \
    -- 'projects/*/modeling/*.org' | awk -F/ '{print $2}' | sort | uniq -c

echo
echo "## Files naming workspace_id, by component (excluding ores.workspace and doc)"
git grep -l "workspace_id" -- 'projects/*' ':!projects/ores.workspace' \
    | awk -F/ '{print $2}' | sort | uniq -c | sort -rn

echo
echo "## ores.workspace component files"
git ls-files projects/ores.workspace | wc -l

echo
echo "## ores.shell workspace part files"
git ls-files projects/ores.shell/workspace projects/ores.shell/scripts/library/workspaces | wc -l

echo
count() { git grep -n "$@" | wc -l || true; }

echo "## Callers that select a workspace other than Live"
echo "setWorkspace callers in ores.web (excluding the definition):"
count "setWorkspace(" -- projects/ores.web ':!*client.ts'
echo "with_workspace_id / with_workspace_resolution callers outside tests:"
count -E "\.with_workspace_(id|resolution)\(" -- 'projects/*.cpp' \
    ':!projects/ores.nats/src/service/nats_client.cpp' ':!projects/*/tests/*'
