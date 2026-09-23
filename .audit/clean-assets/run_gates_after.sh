#!/bin/bash
cd /mnt/development/OreStudio/ores_dev_jolly_knuth || exit 1
PY=projects/ores.codegen/venv/bin/python3
S=projects/ores.codegen/scripts
OUT=.audit/clean-assets
for gate in check_protocol_twin_coverage check_handler_permissions check_populate_references; do
    timeout 900 "$PY" "$S/$gate.py" > "$OUT/A2_$gate.txt" 2>&1
    echo "$gate exit=$?"
done
timeout 900 "$PY" "$S/check_model_drift.py" --summary > "$OUT/A2_check_model_drift.txt" 2>&1
echo "check_model_drift exit=$?"
timeout 900 "$PY" "$S/regenerate_cmake_component_files.py" --all --check > "$OUT/A2_cmake_source_lists.txt" 2>&1
echo "cmake_source_lists exit=$?"
echo "GATES DONE"
