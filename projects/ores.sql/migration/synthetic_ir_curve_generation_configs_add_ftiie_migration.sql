/* -*- mode: sql; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * One-shot migration: add F-TIIE to the index_family vocabulary
 *
 * ORE's own examples quote curves on F-TIIE -- the TIIE de Fondeo, Mexico's
 * overnight funding rate -- and the corpus carries 46 such keys:
 * IR_SWAP/RATE/MXN/FTIIE/1D/1D/130M and its siblings, plus
 * MM/RATE/MXN/FTIIE/0D/1D. F-TIIE is a second Mexican benchmark rather than a
 * spelling of TIIE, so oresmd's index_family needs a member for it, and the
 * family list is mirrored by two CHECK constraints on
 * ores_synthetic_ir_curve_generation_configs_tbl. The create script now lists
 * 'ftiie'; this migration applies the same change to databases created before
 * it.
 *
 * F-TIIE is overnight-style, so it joins the families that require an empty
 * tenor rather than the term families.
 *
 * On a freshly recreated database both checks already list 'ftiie' and this
 * script is a guarded no-op.
 *
 * No tool executes migration scripts; run this by hand against persistent
 * environments that predate the fix (see ores.sql's schema upgrade policy in
 * modeling/component_overview.org).
 *
 * This script is idempotent.
 */

\echo '--- Adding ftiie to the IR curve generation config family checks ---'

do $$
declare
    v_constraint_name text;
begin
    -- Both checks were declared inline and are therefore auto-named, so they are
    -- matched by definition text rather than by guessing the name. The family
    -- check's own name is the truncated, column-shaped one Postgres generated
    -- for it; it cannot be re-added as ..._tbl_check, which is a different
    -- check on the same table.
    --
    -- Each check is dropped only if it is still the old definition, and created
    -- only if it is absent, so the script both upgrades an old database and
    -- repairs one where a previous run dropped a check and failed to re-add it.
    select conname into v_constraint_name
    from pg_constraint
    where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
      and contype = 'c'
      and pg_get_constraintdef(oid) like '%libor%'
      and pg_get_constraintdef(oid) not like '%tenor%'
      and pg_get_constraintdef(oid) not like '%ftiie%';

    if v_constraint_name is not null then
        execute format(
            'alter table ores_synthetic_ir_curve_generation_configs_tbl drop constraint %I',
            v_constraint_name);
    end if;

    if not exists (
        select 1 from pg_constraint
        where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
          and contype = 'c'
          and pg_get_constraintdef(oid) like '%libor%'
          and pg_get_constraintdef(oid) not like '%tenor%'
          and pg_get_constraintdef(oid) like '%ftiie%'
    ) then
        raise notice 'adding ftiie to the family membership check';
        alter table ores_synthetic_ir_curve_generation_configs_tbl
            add constraint ores_synthetic_ir_curve_generation_configs_t_index_family_check
            check (("index_family" in ('libor', 'euribor', 'sofr', 'estr', 'sonia', 'tona', 'saron', 'aonia', 'corra', 'honia', 'sora', 'swestr', 'nowa', 'kofr', 'mibor', 'zaronia', 'destr', 'polonia', 'nzonia', 'shibor', 'tiie', 'ftiie', 'taibor')));
    else
        raise notice 'family membership check already lists ftiie; nothing to do';
    end if;

    -- The tenor-combo check splits the term families from the overnight ones.
    select conname into v_constraint_name
    from pg_constraint
    where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
      and contype = 'c'
      and pg_get_constraintdef(oid) like '%libor%'
      and pg_get_constraintdef(oid) like '%tenor%'
      and pg_get_constraintdef(oid) not like '%ftiie%';

    if v_constraint_name is not null then
        execute format(
            'alter table ores_synthetic_ir_curve_generation_configs_tbl drop constraint %I',
            v_constraint_name);
    end if;

    if not exists (
        select 1 from pg_constraint
        where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
          and contype = 'c'
          and pg_get_constraintdef(oid) like '%libor%'
          and pg_get_constraintdef(oid) like '%tenor%'
          and pg_get_constraintdef(oid) like '%ftiie%'
    ) then
        raise notice 'adding ftiie to the tenor-combo check';
        alter table ores_synthetic_ir_curve_generation_configs_tbl
            add constraint ores_synthetic_ir_curve_generation_configs_tbl_check1
            check (("index_family" in ('libor', 'euribor') and "tenor" <> '') or ("index_family" in ('estr', 'sonia', 'tona', 'saron', 'aonia', 'corra', 'honia', 'sora', 'swestr', 'nowa', 'kofr', 'mibor', 'zaronia', 'destr', 'polonia', 'nzonia', 'shibor', 'tiie', 'ftiie', 'taibor') and "tenor" = '') or ("index_family" = 'sofr' and "tenor" in ('', 'FOMC')));
    else
        raise notice 'tenor-combo check already lists ftiie; nothing to do';
    end if;
end $$;

-- Summary
select conname as constraint_name, pg_get_constraintdef(oid) as definition
from pg_constraint
where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
  and contype = 'c'
  and pg_get_constraintdef(oid) like '%libor%'
order by conname;
