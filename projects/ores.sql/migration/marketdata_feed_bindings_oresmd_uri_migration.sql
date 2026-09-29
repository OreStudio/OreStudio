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
 * One-shot migration: the feed binding names its series by identity
 *
 * projects/ores.marketdata/modeling/ores.marketdata.feed_binding.org keyed a
 * binding by the ORE key of the series it binds. That key is a spelling of the
 * series, and the product of the cutover is the series' oresmd identity, so the
 * column becomes oresmd_uri and the natural key moves with it.
 *
 * The projection from a key to its identity is the C++ grammar's, not SQL's, so
 * an existing binding cannot be converted here. A binding is a runtime artifact,
 * re-created from the feed it was made for -- the synthetic service's feed
 * controller makes one whenever a bound feed starts -- so the migration clears
 * the table and drops the column rather than keeping keys in a column that no
 * longer means what it says. A database that held bindings must start its bound
 * feeds again to re-create them.
 *
 * On a freshly recreated database the create script already emits the identity,
 * its check and its index, so this migration is unnecessary. It exists for
 * databases created before the change.
 */

begin;

delete from ores_marketdata_feed_bindings_tbl;

alter table ores_marketdata_feed_bindings_tbl
    drop column if exists "ore_key";

alter table ores_marketdata_feed_bindings_tbl
    add column if not exists "oresmd_uri" text;

alter table ores_marketdata_feed_bindings_tbl
    alter column "oresmd_uri" set not null;

alter table ores_marketdata_feed_bindings_tbl
    drop constraint if exists feed_bindings_oresmd_uri_not_empty_ck;

alter table ores_marketdata_feed_bindings_tbl
    add constraint feed_bindings_oresmd_uri_not_empty_ck check (oresmd_uri <> '');

drop index if exists feed_bindings_party_id_ore_key_source_name_uniq_idx;

create unique index if not exists feed_bindings_party_id_oresmd_uri_source_name_uniq_idx
on ores_marketdata_feed_bindings_tbl (tenant_id, party_id, oresmd_uri, source_name)
where valid_to = ores_utility_infinity_timestamp_fn();

commit;
