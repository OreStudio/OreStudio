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
 * One-shot migration: the observation's coordinate becomes the datum's URI
 *
 * The observation used to store the registry's ORE-form point in a column named
 * point_id. It now stores the datum's canonical oresmd URI: the series' URI with
 * the observation's coordinate keys left in. The rename below is the whole of
 * what this file does to the schema.
 *
 * The data is not rewritten here, and the reason is worth stating. The
 * conversion needs the oresmd grammar -- an ORE key's point becomes a set of
 * named query keys, and which keys depends on the quote type the key names --
 * and SQL has no way to call it. The honest source of the new value is therefore
 * the producer's own key, which every imported row already carries in the key
 * column, and re-importing the corpus rewrites both the series' identity and the
 * datum URI from it. A rebuild does the same from the create script. A row with
 * no key cannot be re-derived from anything, so it is reported below rather than
 * guessed at.
 *
 * On a freshly recreated database the create script already emits the column
 * under its new name and this migration is unnecessary. It exists for databases
 * created before the change.
 */

\echo '--- market observations: point_id becomes the datum URI ---'

do $$
begin
    if exists (
        select 1 from information_schema.columns
        where table_name = 'ores_marketdata_market_observations_tbl'
          and column_name = 'point_id'
    ) then
        alter table ores_marketdata_market_observations_tbl
            rename column "point_id" to "oresmd_uri";

        -- The index that leads with the coordinate follows its column.
        alter index if exists market_observations_observations_series_point_datetime_idx
            rename to market_observations_observations_series_coordinate_datetime_idx;
    end if;

    if exists (
        select 1 from information_schema.columns
        where table_name = 'ores_marketdata_observation_lineages_tbl'
          and column_name = 'point_id'
    ) then
        alter table ores_marketdata_observation_lineages_tbl
            rename column "point_id" to "oresmd_uri";
    end if;
end $$;

-- The soft-update and soft-delete triggers name the column, so they are replaced
-- with the new spelling rather than left to fail on the next write.
create or replace function ores_marketdata_market_observations_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update "ores_marketdata_market_observations_tbl"
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
      and series_id = new.series_id
      and observation_datetime = new.observation_datetime
      and oresmd_uri = new.oresmd_uri
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_marketdata_market_observations_delete_fn()
returns trigger as $$
begin
    update "ores_marketdata_market_observations_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and series_id = old.series_id
      and observation_datetime = old.observation_datetime
      and oresmd_uri = old.oresmd_uri
      and valid_to = ores_utility_infinity_timestamp_fn();
    return null;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- Re-derivation report. Rows that still spell the point are the ones the next
-- import rewrites; a row with no producer key is the population the import can
-- never rewrite, and it is named rather than counted so an operator can see it.
\echo '--- observations still carrying an ORE-form point, pending re-import ---'
select count(*) as observations_pending_reimport
from ores_marketdata_market_observations_tbl
where key is not null
  and oresmd_uri not like 'oresmd://%';

\echo '--- observations with no producer key, which no re-import can re-derive ---'
select count(*) as observations_without_a_key
from ores_marketdata_market_observations_tbl
where key is null;
