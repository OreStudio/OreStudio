#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

# Password Validation Feature Flag Management
# Component: ores.variability (used by ores.accounts)
# Purpose: Setup and toggle password validation feature flag
# Usage:
#   ./setup_password_validation_flag.sh setup    - Create the flag (disabled by default)
#   ./setup_password_validation_flag.sh disable  - Disable password validation (allow weak passwords)
#   ./setup_password_validation_flag.sh enable   - Enable password validation (require strong passwords)
#   ./setup_password_validation_flag.sh status   - Show current flag state

set -euo pipefail

# Database connection parameters (can be overridden via environment variables)
DB_USER="${ORES_DB_USER:-ores}"
DB_PASSWORD="${ORES_DB_PASSWORD:-ahV6aehuij6eingohsiajaiT0}"
DB_DATABASE="${ORES_DB_DATABASE:-oresdb}"
DB_HOST="${ORES_DB_HOST:-localhost}"
DB_PORT="${ORES_DB_PORT:-5432}"

# Export password for psql
export PGPASSWORD="$DB_PASSWORD"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

show_usage() {
    echo "Usage: $0 {setup|enable|disable|status}"
    echo ""
    echo "Commands:"
    echo "  setup   - Create the password validation feature flag (validation enabled by default)"
    echo "  enable  - Enable password validation (require strong passwords - PRODUCTION)"
    echo "  disable - Disable password validation (allow weak passwords - TESTING ONLY)"
    echo "  status  - Show current password validation state"
    echo ""
    echo "Environment variables:"
    echo "  ORES_DB_USER     - Database user (default: ores)"
    echo "  ORES_DB_PASSWORD - Database password (default: ahV6aehuij6eingohsiajaiT0)"
    echo "  ORES_DB_DATABASE - Database name (default: oresdb)"
    echo "  ORES_DB_HOST     - Database host (default: localhost)"
    echo "  ORES_DB_PORT     - Database port (default: 5432)"
    exit 1
}

run_sql_file() {
    local sql_file="$1"
    psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_DATABASE" -f "$sql_file"
}

run_sql_command() {
    local sql_command="$1"
    psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_DATABASE" -t -c "$sql_command"
}

setup_flag() {
    echo "Setting up password validation feature flag..."
    run_sql_file "projects/sql/disable_password_validation_setup.sql"
    echo -e "${GREEN}Setup complete!${NC}"
}

enable_validation() {
    echo "Enabling password validation (strong passwords required)..."
    run_sql_command "INSERT INTO oresdb.feature_flags (name, enabled, description, modified_by, valid_from, valid_to) VALUES ('system.disable_password_validation', 0, 'When enabled (1), disables strict password validation. FOR TESTING/DEVELOPMENT ONLY.', current_user, current_timestamp, '9999-12-31 23:59:59'::timestamptz);"
    echo -e "${GREEN}Password validation is now ENABLED${NC}"
    echo -e "${GREEN}Strong passwords are required (12+ chars, uppercase, lowercase, digit, special char)${NC}"
}

disable_validation() {
    echo -e "${YELLOW}WARNING: Disabling password validation!${NC}"
    echo -e "${YELLOW}This should ONLY be used for TESTING/DEVELOPMENT${NC}"
    run_sql_command "INSERT INTO oresdb.feature_flags (name, enabled, description, modified_by, valid_from, valid_to) VALUES ('system.disable_password_validation', 1, 'When enabled (1), disables strict password validation. FOR TESTING/DEVELOPMENT ONLY.', current_user, current_timestamp, '9999-12-31 23:59:59'::timestamptz);"
    echo -e "${RED}Password validation is now DISABLED${NC}"
    echo -e "${RED}Weak passwords are allowed!${NC}"
}

show_status() {
    echo "Current password validation state:"
    echo ""
    result=$(run_sql_command "SELECT enabled FROM oresdb.feature_flags WHERE name = 'system.disable_password_validation' AND valid_to = '9999-12-31 23:59:59'::timestamptz;")

    if [ -z "$result" ]; then
        echo -e "${YELLOW}Feature flag not found. Run '$0 setup' first.${NC}"
    else
        enabled=$(echo "$result" | tr -d '[:space:]')
        if [ "$enabled" = "0" ]; then
            echo -e "${GREEN}Password validation: ENABLED${NC}"
            echo -e "${GREEN}Strong passwords are required${NC}"
        else
            echo -e "${RED}Password validation: DISABLED${NC}"
            echo -e "${RED}Weak passwords are allowed (TESTING ONLY)${NC}"
        fi
    fi
}

# Main script
case "${1:-}" in
    setup)
        setup_flag
        ;;
    enable)
        enable_validation
        show_status
        ;;
    disable)
        disable_validation
        show_status
        ;;
    status)
        show_status
        ;;
    *)
        show_usage
        ;;
esac
