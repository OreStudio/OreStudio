/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

-- =============================================================================
-- Workspace Tables
-- =============================================================================
-- Each workspace is a named, isolated data context. The Live workspace uses
-- the sentinel UUID ores_utility_live_workspace_id_fn() and cannot be
-- archived. parent_workspace_id enables Docker-layer-style inheritance.
--
-- This file is the component's aggregator. The table, its triggers and its
-- hierarchy read are generated from ores.workspace.workspace.org; the
-- resolution-order function, the workspace FK validation function and the
-- trade-scope whitelist are hand-written, because the generator has no way to
-- express them.
--
-- scope_portfolio_id is a soft FK to ores_refdata_portfolios_tbl(id): no hard
-- DB constraint, because workspace is created before refdata so refdata can
-- hold a hard FK back to this table.

\ir ./workspace_workspaces_create.sql
\ir ./workspace_workspaces_notify_trigger_create.sql
\ir ./workspace_functions_create.sql
\ir ./workspace_trade_scope_create.sql
