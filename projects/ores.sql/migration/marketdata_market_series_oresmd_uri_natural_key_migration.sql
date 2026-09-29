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
 * One-shot migration: the oresmd URI becomes the series' natural key
 *
 * projects/ores.marketdata/modeling/ores.marketdata.market_series.org now keys
 * the series by oresmd_uri and party_id, and series_type, metric and qualifier
 * no longer key the row. A row written before that carries an identity when its
 * key was nameable and nothing when it was not, so a row with no identity is
 * given one derived from its own uuid -- the same way the trading migration
 * backfills a key that was never written. Such a name is honest about being
 * synthetic: the row is readable by it, and the ORE key the row arrived under is
 * not recoverable from SQL.
 *
 * The backfill covers closed history rows as well as current ones: not null
 * applies to every row in the table, and the GIST exclusion keeps each version
 * as a row of its own.
 *
 * The triple's unique index goes, because the triple no longer keys the row; the
 * columns stay until the readers that still ask by them are migrated. If an old
 * database holds two rows with one identity the create index fails, which is the
 * honest outcome: those rows need reconciling by hand rather than at random.
 *
 * series_type, metric and qualifier keep their non-empty checks; the identity
 * gains one so a caller cannot store a row the reader can never find.
 *
 * On a freshly recreated database the create script already emits the identity's
 * index, the not null and the check, so this migration is unnecessary. It exists
 * for databases created before the change.
 *
 * The whole file is one transaction: a database whose rows share an identity must
 * not be left with the triple's index dropped and the not null set. The rows that
 * would fail the index are named before it is created, so the operator sees which
 * ones need reconciling instead of Postgres' own message.
 */

begin;

update ores_marketdata_market_series_tbl
set oresmd_uri = 'oresmd://generic/migrated-' || replace(id::text, '-', '') || '?type=fixing'
where oresmd_uri is null
   or oresmd_uri = '';

do $$
declare
    offenders text;
begin
    select string_agg(identity || ' for party ' || party_id::text || ' (' || n || ' rows)', '; ')
    into offenders
    from (
        select oresmd_uri as identity, tenant_id, party_id, count(*) as n
        from ores_marketdata_market_series_tbl
        where valid_to = ores_utility_infinity_timestamp_fn()
        group by oresmd_uri, tenant_id, party_id
        having count(*) > 1
    ) shared;
    if offenders is not null then
        raise exception
            'market series rows share an identity and need reconciling before this '
            'migration: %', offenders;
    end if;
end $$;

alter table ores_marketdata_market_series_tbl
    alter column oresmd_uri set not null;

alter table ores_marketdata_market_series_tbl
    drop constraint if exists market_series_oresmd_uri_not_empty_ck;

alter table ores_marketdata_market_series_tbl
    add constraint market_series_oresmd_uri_not_empty_ck check (oresmd_uri <> '');

drop index if exists market_series_party_id_series_type_metric_qualifier_uniq_idx;

create unique index if not exists market_series_party_id_oresmd_uri_uniq_idx
on ores_marketdata_market_series_tbl (tenant_id, party_id, oresmd_uri)
where valid_to = ores_utility_infinity_timestamp_fn();

commit;
