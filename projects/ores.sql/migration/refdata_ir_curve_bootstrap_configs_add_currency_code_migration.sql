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
 * IR Curve Bootstrap Configs: the currency the pillars are quoted in
 *
 * A pillar carries tenor codes and a role, so a reader resolving a pillar to
 * the market series its ORE key projects to has to be told the currency the key
 * is written in. The config that owns the pillars names it, exactly as it
 * already names the tenor convention its pillars resolve their dates under.
 *
 * The schema is applied by recreation, so this migration is for a database
 * built before the column: a rebuild has it, and the populate's upsert writes
 * the value either way.
 */

\echo '--- IR curve bootstrap configs: currency ---'

alter table ores_refdata_ir_curve_bootstrap_configs_tbl
    add column if not exists currency_code text;

-- The only curve seeded today is the USD SOFR short end. A database carrying
-- another currency has to correct its own row before the constraint lands.
update ores_refdata_ir_curve_bootstrap_configs_tbl
set currency_code = 'USD'
where currency_code is null;

alter table ores_refdata_ir_curve_bootstrap_configs_tbl
    alter column currency_code set not null;
