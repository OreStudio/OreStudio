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

-- =============================================================================
-- ORE Sample Counterparties Publication
--
-- Publishes the ORE sample banks as counterparties of the system tenant, beside
-- ACME's own, and publishes the ORE aliases that map every counterparty name
-- the ORE examples use onto one of them. An ORE import resolves an envelope's
-- CounterParty through these aliases, so the examples import against real
-- GLEIF banks. A provisioned tenant gets the aliases through the base bundle,
-- against the GLEIF counterparties it already holds.
--
-- Runs after the ACME publication, which publishes the business centres a
-- counterparty is validated against. Idempotent.
-- =============================================================================

\echo '--- ORE Sample Counterparties Publication ---'

do $$
declare
    v_tenant_id uuid := ores_utility_system_tenant_id_fn();
    v_dataset_id uuid;
    v_result record;
begin
    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.sample_counterparties'
      and valid_to = ores_utility_infinity_timestamp_fn();
    if v_dataset_id is null then
        raise warning 'ore.sample_counterparties dataset absent; ORE sample counterparties not published.';
        return;
    end if;

    for v_result in
        select * from ores_refdata_publish_lei_counterparties_from_dq_fn(v_dataset_id, v_tenant_id)
    loop
        raise notice 'ore sample counterparties: % %', v_result.action, v_result.record_count;
    end loop;

    select id into v_dataset_id from ores_dq_datasets_tbl
    where code = 'ore.counterparty_aliases'
      and valid_to = ores_utility_infinity_timestamp_fn();
    for v_result in
        select * from ores_refdata_publish_counterparty_aliases_from_dq_fn(v_dataset_id, v_tenant_id)
    loop
        raise notice 'ore sample counterparty aliases: % %', v_result.action, v_result.record_count;
    end loop;
end $$;
