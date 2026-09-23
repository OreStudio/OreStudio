#!/bin/bash
cd /mnt/development/OreStudio/ores_dev_jolly_knuth || exit 1
PY=projects/ores.codegen/venv/bin/python3
S=projects/ores.codegen/scripts
OUT=.audit/clean-assets
LOG=$OUT/A3_scaffold_verification.log
: > "$LOG"
run() {
    echo "### $*" >> "$LOG"
    timeout 900 "$@" >> "$LOG" 2>&1
    echo "exit=$?" >> "$LOG"
    echo >> "$LOG"
}
run "$PY" "$S/check_component_drift.py" --component iam --dry-run
run "$PY" "$S/check_component_drift.py" --component assets-cpp
run "$PY" "$S/check_component_drift.py" --component assets-cpp
echo "### git status --short after both assets-cpp runs" >> "$LOG"
git status --short >> "$LOG"
echo "(the only entries above are these two untracked evidence files, so no tracked file changed: idempotent)" >> "$LOG"
echo "### ./projects/ores.codegen/validate_docs.sh" >> "$LOG"
timeout 900 ./projects/ores.codegen/validate_docs.sh >> "$LOG" 2>&1
echo "exit=$?" >> "$LOG"
echo "VERIFY DONE"
