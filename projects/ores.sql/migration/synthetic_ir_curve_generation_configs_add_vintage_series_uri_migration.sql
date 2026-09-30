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
 * Synthetic IR curve configs: the vintage series identity
 *
 * The config named the source and the date of the vintage it seeds from but not
 * the series, which the feed found through the registry's decomposition of its
 * key. The series is keyed by its oresmd identity now, so the config names it.
 *
 * The schema is applied by recreation, so this migration is for a database built
 * before the column: a rebuild has it already. Postgres names the table's
 * unnamed checks itself, so the vintage check that has to gain the new column is
 * found by its own definition rather than by a guessed name.
 */

\echo '--- Synthetic IR curve configs: vintage series identity ---'

alter table ores_synthetic_ir_curve_generation_configs_tbl
    add column if not exists vintage_series_uri text not null default '';

-- A vintage row cannot be left empty: the check below requires the column when
-- the price source is vintage, so the rows are named before it lands. The
-- dataset that publishes a config names the series it reads -- the deposit
-- grid's own ORE key projects to it -- so the artefact answers first, and only
-- a row no dataset publishes falls back to the spelling a currency's deposit
-- grid has: the currency and the two-day spot lag almost every market uses.
update ores_synthetic_ir_curve_generation_configs_tbl c
set vintage_series_uri = a.vintage_series_uri
from ores_dq_synthetic_ir_curve_configs_artefact_tbl a
where c.price_source = 'vintage'
  and a.vintage_series_uri is not null
  and a.vintage_series_uri <> ''
  and a.currency_code = c.currency_code
  and a.index_family = c.index_family
  and a.tenor = c.tenor;

update ores_synthetic_ir_curve_generation_configs_tbl
set vintage_series_uri =
    'oresmd://ir/' || lower(currency_code) || '?tenor=2d&type=quote&metric=rate&quote=mm'
where price_source = 'vintage'
  and vintage_series_uri = '';

alter table ores_synthetic_ir_curve_generation_configs_tbl
    alter column vintage_series_uri drop default;

do $$
declare
    v_constraint text;
begin
    select conname into v_constraint
    from pg_constraint
    where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
      and contype = 'c'
      and pg_get_constraintdef(oid) like '%vintage_date%'
      and pg_get_constraintdef(oid) not like '%vintage_series_uri%';

    if v_constraint is not null then
        execute format(
            'alter table ores_synthetic_ir_curve_generation_configs_tbl drop constraint %I',
            v_constraint);
    end if;
end $$;

alter table ores_synthetic_ir_curve_generation_configs_tbl
    drop constraint if exists ores_synthetic_ir_curve_generation_configs_vintage_check;

alter table ores_synthetic_ir_curve_generation_configs_tbl
    add constraint ores_synthetic_ir_curve_generation_configs_vintage_check
    check (("price_source" = 'fixed' and "vintage_source" = '' and "vintage_date" = '' and "vintage_series_uri" = '') or ("price_source" = 'vintage' and "vintage_source" <> '' and "vintage_date" <> '' and "vintage_series_uri" <> ''));
