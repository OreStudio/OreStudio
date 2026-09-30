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
 * Marketdata series classification rules: the index_name source
 *
 * The FIXING rule classified every index fixing as interest_rates. It now
 * reads the class from the index name, which is a third value for
 * asset_class_source, so a database built before this change has both the old
 * check constraint and the old FIXING row.
 *
 * The schema is applied by recreation, so this migration is for a database
 * built before the change: a rebuild has both already. Postgres names the
 * table's unnamed check itself, so the old one is found by its own definition
 * rather than by a guessed name, and the seed's insert does nothing on
 * conflict, so the existing FIXING row is moved here.
 */

\echo '--- Marketdata series classification rules: the index_name source ---'

do $$
declare
    v_constraint text;
begin
    select conname into v_constraint
    from pg_constraint
    where conrelid = 'ores_marketdata_series_classification_rules_tbl'::regclass
      and contype = 'c'
      and pg_get_constraintdef(oid) like '%asset_class_source%'
      and pg_get_constraintdef(oid) like '%correlation_operands%'
      and pg_get_constraintdef(oid) not like '%index_name%';

    if v_constraint is not null then
        execute format(
            'alter table ores_marketdata_series_classification_rules_tbl '
            'drop constraint %I',
            v_constraint);
    end if;
end $$;

alter table ores_marketdata_series_classification_rules_tbl
    drop constraint if exists ores_marketdata_series_classification_rules_source_check;

alter table ores_marketdata_series_classification_rules_tbl
    add constraint ores_marketdata_series_classification_rules_source_check
    check ("asset_class_source" in ('literal', 'correlation_operands', 'index_name'));

-- The FIXING row moves with the constraint. The classification rules are a
-- catalogue rather than an entity with history a reader asks for, so the
-- current row is updated in place; the seed's insert skips it on the next run.
update ores_marketdata_series_classification_rules_tbl
set asset_class_source = 'index_name',
    asset_class_code = null,
    description = 'A synthetic key the import builds for an index fixing. The class is the '
                  'refdata class the index name projects to, so a power fixing is commodity '
                  'and an equity fixing is equity; the subclass records the fixing.'
where series_type = 'FIXING'
  and metric = ''
  and valid_to = ores_utility_infinity_timestamp_fn()
  and asset_class_source = 'literal';
