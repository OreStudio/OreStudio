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
 * One-shot migration: seed the today's market collection kinds
 *
 * A collection and a configuration binding named their kind as free text, and
 * every entry repeated its kind's key attribute. The twenty-four kinds are now
 * a seeded lookup read out of todaysmarket.xsd, holding each kind's entry
 * element and key attributes; both kind columns refer to it, and the entry's
 * key_attribute column is dropped.
 *
 * The migration refuses to start while a current collection or binding names
 * a kind the lookup will not hold, because the next version of that row would
 * be refused. The today's market mapper writes only schema kinds, so a database
 * it filled migrates in place.
 *
 * On a freshly recreated database the create and populate scripts already do
 * this, and this migration is unnecessary. It exists for databases created
 * before the change.
 */

begin;

\ir ../create/analytics/analytics_todays_market_collection_kinds_create.sql
\ir ../create/analytics/analytics_todays_market_collection_kinds_notify_trigger_create.sql
\ir ../populate/analytics/analytics_todays_market_collection_kinds_populate.sql

do $$
declare
    v_unknown text;
begin
    select string_agg(distinct kind, ', ')
    into v_unknown
    from (
        select collection as kind from ores_analytics_todays_market_collections_tbl
        where valid_to = ores_utility_infinity_timestamp_fn()
        union
        select collection from ores_analytics_todays_market_configuration_bindings_tbl
        where valid_to = ores_utility_infinity_timestamp_fn()
    ) k
    where kind not in (
        select code from ores_analytics_todays_market_collection_kinds_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()
    );

    if v_unknown is not null then
        raise exception 'Today''s market rows name collection kinds the schema does not define: %',
            v_unknown;
    end if;
end;
$$;

alter table ores_analytics_todays_market_entries_tbl drop column if exists key_attribute;

\ir ../create/analytics/analytics_todays_market_entries_create.sql
\ir ../create/analytics/analytics_todays_market_collections_create.sql
\ir ../create/analytics/analytics_todays_market_configuration_bindings_create.sql
\ir ../create/analytics/analytics_rls_policies_create.sql
\ir ../populate/iam/iam_permissions_populate.sql

commit;
