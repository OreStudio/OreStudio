#!/bin/bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
# Template: shell_service_vars.mustache
#
# To add a new domain service:
#   1. Add an entry to:
#        projects/ores.codegen/models/services/ores_services_service_registry.json
#   2. Run the generator:
#        cd projects/ores.codegen
#        ./run_generator.sh models/services/ores_services_service_registry.json \
#            --profile service-registry
#   3. Commit all generated files.
#   4. Re-run recreate_database.sh to provision the new service in the database.
#
# Naming conventions (N = service name, e.g. "iam"):
#   DB user env var:      ORES_{N_UPPER}_SERVICE_DB_USER
#   DB password env var:  ORES_{N_UPPER}_SERVICE_DB_PASSWORD
#   psql variable user:   {n}_service_user
#   psql variable pw:     {n}_service_password
#   IAM role name:        {NPascal}Service
#
# shellcheck disable=SC2034

# Registry of all NATS domain services.
SERVICE_NAMES=(
    iam
    refdata
    dq
    variability
    assets
    scheduler
    reporting
    telemetry
    trading
    compute
    synthetic
    workflow
)
