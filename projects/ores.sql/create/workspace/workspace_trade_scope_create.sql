/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

-- =============================================================================
-- Trade scope whitelist (references workspace UUID, not integer).
-- =============================================================================

create table if not exists ores_workspace_trade_scope_tbl (
    workspace_id  uuid  not null,
    trade_id      uuid  not null,
    primary key (workspace_id, trade_id)
);
