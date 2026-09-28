/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

-- =============================================================================
-- Workspace functions the entity generator cannot express.
-- =============================================================================

drop trigger if exists ores_workspaces_require_live_trg on ores_workspaces_tbl;
drop function if exists ores_workspaces_require_live_fn() cascade;
drop function if exists ores_workspace_validate_fn(uuid) cascade;
drop function if exists ores_workspace_resolution_order_fn(uuid, uuid) cascade;
