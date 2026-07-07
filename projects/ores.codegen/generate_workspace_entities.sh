#!/bin/bash
# Runs domain + repository codegen for all has_workspace_id: true entities.
# Usage: ./generate_workspace_entities.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/venv/bin/activate"
cd "$SCRIPT_DIR"

run() {
    echo "=== $1 ==="
    python3 src/generator.py "$1" --profile domain
    python3 src/generator.py "$1" --profile generator
    python3 src/generator.py "$1" --profile repository
}

# refdata (11 entities)
run models/refdata/book_domain_entity.json
run models/refdata/cds_convention_domain_entity.json
run models/refdata/deposit_convention_domain_entity.json
run models/refdata/fra_convention_domain_entity.json
run models/refdata/ibor_index_convention_domain_entity.json
run models/refdata/ois_convention_domain_entity.json
run models/refdata/overnight_index_convention_domain_entity.json
run models/refdata/portfolio_domain_entity.json
run models/refdata/swap_convention_domain_entity.json
run models/refdata/zero_convention_domain_entity.json

# trading (11 entities — trade_domain_entity is maintained manually due to composed sub-struct architecture)
run models/trading/balance_guaranteed_swap_instrument_domain_entity.json
run models/trading/callable_swap_instrument_domain_entity.json
run models/trading/cap_floor_instrument_domain_entity.json
run models/trading/fra_instrument_domain_entity.json
run models/trading/inflation_swap_instrument_domain_entity.json
run models/trading/knock_out_swap_instrument_domain_entity.json
run models/trading/lifecycle_event_domain_entity.json
run models/trading/rpa_instrument_domain_entity.json
run models/trading/swaption_instrument_domain_entity.json
run models/trading/trade_identifier_domain_entity.json
run models/trading/vanilla_swap_instrument_domain_entity.json

# reporting (1 entity)
run models/reporting/report_definition_domain_entity.json

echo ""
echo "All 23 workspace entities regenerated (trade maintained manually)."
