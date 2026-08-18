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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * One-shot migration: relax the index_family/tenor combo check for the
 * FOMC-dated USD-SOFR segment
 *
 * The 2026 Realistic theme seeds a second USD-SOFR configuration keyed
 * by the special tenor 'FOMC': the meeting-dated short end consumed by
 * the FOMC bootstrap configuration. The tenor-combo check predates that
 * feature and required RFR families to carry an empty tenor, so
 * publishing the theme failed with a check violation on
 * ores_synthetic_ir_curve_generation_configs_tbl_check1. The create
 * script now permits 'FOMC'; this migration applies the same relaxation
 * to databases created before the fix.
 *
 * On a freshly recreated database the check already has the relaxed
 * definition and this script is a guarded no-op.
 *
 * No tool executes migration scripts; run this by hand against
 * persistent environments that predate the fix (see ores.sql's schema
 * upgrade policy in modeling/component_overview.org).
 *
 * This script is idempotent.
 */

\echo '--- Relaxing the IR curve generation config tenor-combo check ---'

do $$
declare
    v_constraint_name text;
begin
    -- The check was declared inline and therefore auto-named; match by
    -- definition text (the old form permits only an empty tenor for RFR
    -- families) rather than by guessing the generated name.
    select conname into v_constraint_name
    from pg_constraint
    where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
      and contype = 'c'
      and pg_get_constraintdef(oid) like '%tenor = ''''::text))%'
      and pg_get_constraintdef(oid) not like '%FOMC%';

    if v_constraint_name is null then
        raise notice 'tenor-combo check already relaxed; nothing to do';
        return;
    end if;

    execute format(
        'alter table ores_synthetic_ir_curve_generation_configs_tbl drop constraint %I',
        v_constraint_name);

    alter table ores_synthetic_ir_curve_generation_configs_tbl
        add constraint ores_synthetic_ir_curve_generation_configs_tbl_check1
        check (("index_family" in ('libor', 'euribor') and "tenor" <> '') or ("index_family" in ('sofr', 'estr', 'sonia', 'tona', 'saron', 'aonia', 'corra', 'honia', 'sora', 'swestr', 'nowa', 'kofr', 'mibor', 'zaronia', 'destr', 'polonia', 'nzonia', 'shibor', 'tiie', 'taibor') and ("tenor" = '' or "tenor" = 'FOMC')));
end $$;

-- Summary
select 'ores_synthetic_ir_curve_generation_configs_tbl_check1' as constraint_name,
       pg_get_constraintdef(oid) as definition
from pg_constraint
where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
  and conname = 'ores_synthetic_ir_curve_generation_configs_tbl_check1';
