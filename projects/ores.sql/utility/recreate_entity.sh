#!/bin/bash
# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# Drop and recreate an entity's SQL schema.
#
# Environment variables:
#   ORES_TEST_DB_HOST     - Database host (default: localhost)
#   ORES_TEST_DB_PORT     - Database port (default: 5432)
#   ORES_TEST_DB_DATABASE - Database name (required)
#   ORES_TEST_DB_USER     - Database user (default: postgres)
#
# Usage:
#   ./recreate_entity.sh <component>_<entity>
#   ./recreate_entity.sh refdata_currencies
#   ./recreate_entity.sh iam_accounts
#   ./recreate_entity.sh dq_change_reasons
#
# The script will:
#   1. Drop the entity (including notify trigger if present)
#   2. Recreate the entity
#   3. Recreate the notify trigger if present
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Database connection defaults
DB_HOST="${ORES_TEST_DB_HOST:-localhost}"
DB_PORT="${ORES_TEST_DB_PORT:-5432}"
DB_USER="${ORES_TEST_DB_USER:-postgres}"
DB_NAME="${ORES_TEST_DB_DATABASE:-}"

usage() {
    echo "Usage: $0 <component>_<entity>"
    echo ""
    echo "Examples:"
    echo "  $0 refdata_currencies"
    echo "  $0 iam_accounts"
    echo "  $0 dq_change_reasons"
    echo ""
    echo "Environment variables:"
    echo "  ORES_TEST_DB_DATABASE  Database name (required)"
    echo "  ORES_TEST_DB_HOST      Database host (default: localhost)"
    echo "  ORES_TEST_DB_PORT      Database port (default: 5432)"
    echo "  ORES_TEST_DB_USER      Database user (default: postgres)"
    exit 1
}

if [ $# -ne 1 ]; then
    usage
fi

ENTITY="$1"

if [ -z "$DB_NAME" ]; then
    echo "Error: ORES_TEST_DB_DATABASE environment variable is required"
    echo ""
    usage
fi

# Parse component from entity name (first part before underscore)
# e.g., refdata_currencies -> refdata
#       iam_accounts -> iam
#       dq_change_reasons -> dq
COMPONENT="${ENTITY%%_*}"

# Validate component exists
if [ ! -d "$SQL_DIR/create/$COMPONENT" ]; then
    echo "Error: Component '$COMPONENT' not found in $SQL_DIR/create/"
    echo ""
    echo "Available components:"
    ls -1 "$SQL_DIR/create/" | grep -v '\.sql$' | sed 's/^/  /'
    exit 1
fi

# Build file paths
DROP_DIR="$SQL_DIR/drop/$COMPONENT"
CREATE_DIR="$SQL_DIR/create/$COMPONENT"

DROP_FILE="$DROP_DIR/${ENTITY}_drop.sql"
CREATE_FILE="$CREATE_DIR/${ENTITY}_create.sql"
NOTIFY_DROP_FILE="$DROP_DIR/${ENTITY}_notify_trigger_drop.sql"
NOTIFY_CREATE_FILE="$CREATE_DIR/${ENTITY}_notify_trigger_create.sql"

# Check required files exist
if [ ! -f "$DROP_FILE" ]; then
    echo "Error: Drop file not found: $DROP_FILE"
    echo ""
    echo "Available entities in $COMPONENT:"
    ls -1 "$DROP_DIR/"*_drop.sql 2>/dev/null | xargs -n1 basename | sed 's/_drop\.sql$//' | sed 's/^/  /' || echo "  (none)"
    exit 1
fi

if [ ! -f "$CREATE_FILE" ]; then
    echo "Error: Create file not found: $CREATE_FILE"
    exit 1
fi

# Build psql command
PSQL_CMD="psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME"

echo "=============================================="
echo "Recreating entity: $ENTITY"
echo "=============================================="
echo "Database: $DB_NAME@$DB_HOST:$DB_PORT"
echo "User: $DB_USER"
echo ""

# Step 1: Drop notify trigger if exists
if [ -f "$NOTIFY_DROP_FILE" ]; then
    echo "Step 1a: Dropping notify trigger..."
    echo "  -> $NOTIFY_DROP_FILE"
    $PSQL_CMD -f "$NOTIFY_DROP_FILE"
fi

# Step 2: Drop entity
echo "Step 1: Dropping entity..."
echo "  -> $DROP_FILE"
$PSQL_CMD -f "$DROP_FILE"

# Step 3: Create entity
echo "Step 2: Creating entity..."
echo "  -> $CREATE_FILE"
$PSQL_CMD -f "$CREATE_FILE"

# Step 4: Create notify trigger if exists
if [ -f "$NOTIFY_CREATE_FILE" ]; then
    echo "Step 3: Creating notify trigger..."
    echo "  -> $NOTIFY_CREATE_FILE"
    $PSQL_CMD -f "$NOTIFY_CREATE_FILE"
fi

echo ""
echo "=============================================="
echo "Entity '$ENTITY' recreated successfully!"
echo "=============================================="
