/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * Licensed under the GNU General Public License, version 3 or later.
 */

drop function if exists ores_scheduler_sample_relation_sizes_fn();
drop table if exists ores_scheduler_relation_sizes_tbl cascade;
