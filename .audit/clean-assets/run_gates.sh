#!/bin/bash
cd /mnt/development/OreStudio/ores_dev_jolly_knuth || exit 1
PY=projects/ores.codegen/venv/bin/python3
S=projects/ores.codegen/scripts
OUT=.audit/clean-assets
for gate in check_protocol_twin_coverage check_handler_permissions check_populate_references; do
    timeout 900 "$PY" "$S/$gate.py" > "$OUT/B01_$gate.txt" 2>&1
    echo "$gate exit=$?"
done
timeout 900 "$PY" "$S/check_model_drift.py" --summary > "$OUT/B01_check_model_drift.txt" 2>&1
echo "check_model_drift exit=$?"
timeout 900 "$PY" "$S/check_component_drift.py" --component iam --dry-run > "$OUT/B01_check_component_drift_iam.txt" 2>&1
echo "component_drift_iam exit=$?"
echo "ALL GATES COMPLETE"
