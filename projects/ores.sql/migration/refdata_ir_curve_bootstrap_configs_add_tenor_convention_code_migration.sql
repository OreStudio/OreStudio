/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * IR Curve Bootstrap Configs: the tenor convention the pillars resolve under
 *
 * The convention used to be guessed from the source series' qualifier -- a
 * '-FOMC' suffix meant RATES_SPOT_FOMC -- so how a curve was built depended on
 * how one of its series happened to be spelled, and the re-key that gives the
 * series their ORE identities removed the suffix. The config that owns the
 * pillars now names the convention.
 *
 * The schema is applied by recreation, so this migration is for a database
 * built before the column: a rebuild has it, and the populate's upsert writes
 * the value for every config either way.
 */

\echo '--- IR curve bootstrap configs: tenor convention ---'

alter table ores_refdata_ir_curve_bootstrap_configs_tbl
    add column if not exists tenor_convention_code text;

-- Every row needs a convention before the constraint lands, history included:
-- the FOMC short end is the only config that steps through a schedule, so every
-- other config takes the forward default and the seeded one is corrected below.
update ores_refdata_ir_curve_bootstrap_configs_tbl
set tenor_convention_code = 'RATES_SPOT_FORWARD'
where tenor_convention_code is null;

update ores_refdata_ir_curve_bootstrap_configs_tbl
set tenor_convention_code = 'RATES_SPOT_FOMC'
where id = 'd0b1e2f3-4a5b-4c6d-8e7f-9a0b1c2d3e4f';

alter table ores_refdata_ir_curve_bootstrap_configs_tbl
    alter column tenor_convention_code set not null;
