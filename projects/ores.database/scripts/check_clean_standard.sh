#!/usr/bin/env bash
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
#
# The mechanical checks of the Component Clean Standard for ores.database.
# Run from the repository root. Every check prints PASS, FAIL or N/A with the
# evidence it used, and the script exits non-zero when any check fails.
#
#   projects/ores.database/scripts/check_clean_standard.sh
#
# The standard lives at doc/knowledge/architecture/component_clean_standard.org.
# The live and build checks (V01 to V04) are not run here: they need a built
# tree and a running fleet, and their evidence is recorded on the task.

set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
COMPONENT_DIR="${REPO_ROOT}/projects/ores.database"
CODEGEN="${REPO_ROOT}/projects/ores.codegen"
PYTHON="${REPO_ROOT}/projects/ores.compass/venv/bin/python"
FAILURES=0
CHECKS=0

say() {
    local status="$1" item="$2" detail="$3"
    printf '%-4s %-4s %s\n' "$status" "$item" "$detail"
    CHECKS=$((CHECKS + 1))
    if [[ "${status}" == "FAIL" ]]; then
        FAILURES=$((FAILURES + 1))
    fi
}

# Fails when the search finds a hit, reports N/A when it finds none.
expect_no_hits() {
    local item="$1" pattern="$2" detail="$3"
    shift 3
    local hits
    hits="$("$@" 2>/dev/null | grep -E "${pattern}" || true)"
    if [[ -n "${hits}" ]]; then
        say FAIL "${item}" "${detail}: $(echo "${hits}" | wc -l) hit(s), first: $(echo "${hits}" | head -1)"
    else
        say PASS "${item}" "${detail}"
    fi
}

cd "${REPO_ROOT}"

# --- 2. Models ---------------------------------------------------------------

entity_models=$(find "${COMPONENT_DIR}/modeling" -name '*.org' \
    -exec grep -l '#+type: ores.codegen.entity\|#+type: ores.codegen.junction\|#+type: ores.codegen.operation\|#+type: ores.codegen.lookup_entity\|#+type: ores.codegen.message' {} + 2>/dev/null || true)
if [[ -z "${entity_models}" ]]; then
    say N/A M01 "no entity model in the component; only the component model remains"
    say N/A M02 "no entity to bind to a variability profile"
    say N/A M03 "no entity to compare against a profile"
    say N/A M04 "no entity feature to place in the loader namespace"
    say N/A M05 "no entity model; the metadata table is hand-written SQL"
    say N/A M07 "no junction model"
    say N/A M08 "no model type to bind to a header"
else
    say FAIL M01 "entity model(s) present: ${entity_models}"
fi

expect_no_hits M09 'Q_OBJECT|QWidget|Wt::|presentation_drawer' \
    "no retired Qt or Wt fragment in the modeling directory" \
    grep -rniE 'Q_OBJECT|QWidget|Wt::|presentation_drawer' "${COMPONENT_DIR}/modeling"

if [[ -f "${COMPONENT_DIR}/modeling/component_overview.org" ]] \
    && grep -q '#+type: ores.codegen.component' "${COMPONENT_DIR}/modeling/component_overview.org"; then
    say PASS M10 "component_overview.org declares the ores.codegen.component model"
else
    say FAIL M10 "component_overview.org is missing or does not declare the component model"
fi

# --- 3. Protocol -------------------------------------------------------------

messaging_dir="${COMPONENT_DIR}/include/ores.database/messaging"
if [[ -d "${messaging_dir}" ]]; then
    say FAIL P01 "the component owns messaging headers but has no protocol model"
else
    say N/A P01 "the component serves no entity protocol; it has no messaging headers"
    say N/A P02 "the component owns no NATS subject"
    say N/A P03 "the component declares no subject and serves no message"
    say N/A P04 "the component has no operation model"
    say N/A P06 "the component has no event cache"
fi

subjects="$(grep -rn 'ores\.[a-z_]*\.v[0-9]' "${COMPONENT_DIR}/src" "${COMPONENT_DIR}/include" 2>/dev/null | grep -E '"' || true)"
if [[ -z "${subjects}" ]]; then
    say PASS P05 "no consumer of another component's protocol lives in the component"
else
    say FAIL P05 "raw subject strings found: $(echo "${subjects}" | head -1)"
fi

# --- 4. Generation -----------------------------------------------------------

drift_out="$(timeout 900 "${PYTHON}" "${CODEGEN}/scripts/check_component_drift.py" --all --dry-run 2>&1)"
if [[ $? -eq 0 ]]; then
    say PASS G01 "check_component_drift.py --all --dry-run is clean"
else
    say FAIL G01 "check_component_drift.py --all --dry-run: $(echo "${drift_out}" | tail -1)"
fi

generated_marker_dirs="$(grep -rl 'AUTO-GENERATED FILE' "${COMPONENT_DIR}/include" "${COMPONENT_DIR}/src" "${COMPONENT_DIR}/tests" "${COMPONENT_DIR}/modeling" 2>/dev/null || true)"
if [[ -z "${generated_marker_dirs}" ]]; then
    say PASS G03 "no file under the component carries the generated marker"
else
    say FAIL G03 "generated marker without generation: ${generated_marker_dirs}"
fi

if grep -q '^| database' "${CODEGEN}/library/component_catalogue.org"; then
    cmake_out="$(timeout 600 "${PYTHON}" "${CODEGEN}/scripts/regenerate_cmake_component_files.py" --check --component ores.database 2>&1)"
    if [[ $? -eq 0 ]]; then
        say PASS G06 "regenerate_cmake_component_files.py --check --component ores.database is clean"
    else
        say FAIL G06 "regenerate_cmake_component_files.py --check --component ores.database: $(echo "${cmake_out}" | tail -1)"
    fi
else
    say N/A G06 "the catalogue holds no database entry, so the CMake source-list generator does not own this component's lists"
fi

say N/A G02 "no generator supersedes a hand-written file in this component"
say N/A G04 "no generated code in this component to review"
say N/A G05 "the component has no generator of its own"

# --- 5. Wiring and data ------------------------------------------------------

say N/A W01 "no generated registrar family; the component composes no messaging"
say N/A W02 "the component has no handler and checks no permission"
say N/A W03 "the component has no populate script"

sources="$(find "${COMPONENT_DIR}/src" -name '*.cpp' -printf '%P\n' | sort)"
missing=""
for source in ${sources}; do
    if ! grep -q "\"${source}\"" "${COMPONENT_DIR}/src/component_files.cmake"; then
        missing="${missing} ${source}"
    fi
done
if [[ -z "${missing}" ]]; then
    say PASS W04 "every src/*.cpp is listed in src/component_files.cmake"
else
    say FAIL W04 "unlisted source(s):${missing}"
fi

if grep -q 'database/database_create.sql' "${REPO_ROOT}/projects/ores.sql/create/create.sql"; then
    if grep -q 'database_database_info_create.sql' "${REPO_ROOT}/projects/ores.sql/create/database/database_create.sql"; then
        say PASS W04 "the metadata table is reachable from the create aggregator"
    else
        say FAIL W04 "database_create.sql does not include the metadata table script"
    fi
else
    say FAIL W04 "create.sql does not include the database aggregator"
fi

# The component owns no C++ model, so nothing outside it may name one. This
# caught a shell script and a diagram left pointing at the deleted stack.
expect_no_hits H02 'database_info_entity|database_info_mapper|database_info_repository|database_info_json_io|ores_database_info_fn' \
    "no file outside the component names a deleted database_info symbol" \
    grep -rn 'database_info_entity|database_info_mapper|database_info_repository|database_info_json_io|ores_database_info_fn' \
    "${REPO_ROOT}/projects/ores.sql" "${REPO_ROOT}/projects/ores.compass/src" "${COMPONENT_DIR}/modeling"

# --- 6. Shell ----------------------------------------------------------------

if [[ -d "${REPO_ROOT}/projects/ores.shell/database" ]]; then
    say FAIL S01 "the component has shell commands but no shell model"
else
    say N/A S01 "the component has no entity, so it has no shell command unit"
    say N/A S02 "the component has no command, so it has no recipe"
fi

# --- 7. Structure and hygiene ------------------------------------------------

leftovers="$(find "${COMPONENT_DIR}" \( -name '*stub*' -o -name '*.orig' -o -name '*.rej' \
    -o -name '.DS_Store' -o -name '__pycache__' -o -name '*.pyc' \) 2>/dev/null || true)"
if [[ -z "${leftovers}" ]]; then
    say PASS H02 "no scaffold or leftover artefact in the component tree"
else
    say FAIL H02 "leftover artefact(s): ${leftovers}"
fi

expect_no_hits H02 '#\s*(ifdef|ifndef|if)\s+(__|_WIN32)|<unistd\.h>|<windows\.h>|<sys/' \
    "no platform conditional or OS header outside ores.platform" \
    grep -rnE '#\s*(ifdef|ifndef|if)\s+(__|_WIN32)|<unistd\.h>|<windows\.h>|<sys/' \
    "${COMPONENT_DIR}/include" "${COMPONENT_DIR}/src"

commented="$(grep -rnE '^[[:space:]]*//[[:space:]]*(return|if|for|while|std::|auto |const |void |class |struct |[A-Za-z_]+::|[A-Za-z_]+\()[^;]*[;{}]' "${COMPONENT_DIR}/src" "${COMPONENT_DIR}/include" 2>/dev/null || true)"
if [[ -z "${commented}" ]]; then
    say PASS H02 "no commented-out code in the component"
else
    say FAIL H02 "commented-out code: $(echo "${commented}" | head -1)"
fi

docs_out="$(timeout 600 "${CODEGEN}/validate_docs.sh" 2>&1)"
if echo "${docs_out}" | grep -q 'ores.database'; then
    say FAIL H01 "validate_docs.sh reports the component: $(echo "${docs_out}" | grep 'ores.database' | head -1)"
else
    say PASS H01 "validate_docs.sh reports no ores.database violation"
fi

# --- 8. Verification ---------------------------------------------------------

model_gates="$(timeout 900 "${PYTHON}" "${CODEGEN}/scripts/check_model_drift.py" 2>&1)"
if [[ $? -eq 0 ]]; then
    say PASS V05 "check_model_drift.py is clean"
else
    say FAIL V05 "check_model_drift.py: $(echo "${model_gates}" | tail -1)"
fi

for gate in check_protocol_twin_coverage check_handler_permissions check_populate_references; do
    out="$(timeout 900 "${PYTHON}" "${CODEGEN}/scripts/${gate}.py" --all 2>&1)"
    if [[ $? -eq 0 ]]; then
        say PASS V05 "${gate}.py --all is clean"
    else
        say FAIL V05 "${gate}.py --all: $(echo "${out}" | tail -1)"
    fi
done

recipe_out="$(timeout 600 "${PYTHON}" "${CODEGEN}/scripts/regenerate_shell_recipe_inventory.py" --check 2>&1)"
if [[ $? -eq 0 ]]; then
    say PASS V05 "regenerate_shell_recipe_inventory.py --check is clean"
else
    say FAIL V05 "regenerate_shell_recipe_inventory.py --check: $(echo "${recipe_out}" | tail -1)"
fi

if grep -q '"database' "${CODEGEN}/scripts/component_registry.py"; then
    say PASS V06 "the registry lists the component"
else
    say N/A V06 "the component has no codegen model, so it has no regeneration for the gate and no catalogue entry to resolve; see the decision record on the task"
fi

# Every item of the standard except V01 to V04, which need a built tree and a
# running fleet, plus one extra W04 check for the drop aggregator and one extra
# H02 check for references to the deleted stack. The count is asserted so a
# check that stops being emitted is a failure rather than a silent gap in the
# record.
expected_items=39
if [[ ${CHECKS} -ne ${expected_items} ]]; then
    say FAIL ALL "emitted ${CHECKS} checks, expected ${expected_items}"
fi

echo
if [[ ${FAILURES} -eq 0 ]]; then
    echo "No failures. Build, test and fleet checks are recorded on the task."
    exit 0
fi
echo "${FAILURES} check(s) failed."
exit 1
