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

drop table if exists ores_workspace_trade_scope_tbl cascade;
