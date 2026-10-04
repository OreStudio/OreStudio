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
-- ORE Sample Netting Publication
--
-- Publishes the ORE sample netting to the system tenant's root party, Acme
-- Corporation Plc, so an ORE import into the system tenant resolves every
-- netting set id the samples use. A provisioned tenant gets the same data per
-- party through the ore_sample_netting bundle. Runs after the ORE sample
-- counterparties publication, whose banks the agreements are with. Idempotent.
-- =============================================================================

\echo '--- ORE Sample Netting Publication ---'

do $$
declare
    v_result record;
begin
    for v_result in
        select * from ores_refdata_publish_netting_agreements_from_dq_fn(
            (select id from ores_dq_datasets_tbl
             where code = 'ore.sample_netting_agreements'
               and valid_to = ores_utility_infinity_timestamp_fn()),
            ores_utility_system_tenant_id_fn())
    loop
        raise notice 'ore sample netting agreements: % %', v_result.action, v_result.record_count;
    end loop;

    for v_result in
        select * from ores_refdata_publish_netting_sets_from_dq_fn(
            (select id from ores_dq_datasets_tbl
             where code = 'ore.sample_netting_sets'
               and valid_to = ores_utility_infinity_timestamp_fn()),
            ores_utility_system_tenant_id_fn())
    loop
        raise notice 'ore sample netting sets: % %', v_result.action, v_result.record_count;
    end loop;

    for v_result in
        select * from ores_refdata_publish_csas_from_dq_fn(
            (select id from ores_dq_datasets_tbl
             where code = 'ore.sample_csas'
               and valid_to = ores_utility_infinity_timestamp_fn()),
            ores_utility_system_tenant_id_fn())
    loop
        raise notice 'ore sample netting CSAs: % %', v_result.action, v_result.record_count;
    end loop;

    for v_result in
        select * from ores_refdata_publish_netting_set_aliases_from_dq_fn(
            (select id from ores_dq_datasets_tbl
             where code = 'ore.netting_set_aliases'
               and valid_to = ores_utility_infinity_timestamp_fn()),
            ores_utility_system_tenant_id_fn())
    loop
        raise notice 'ore sample netting aliases: % %', v_result.action, v_result.record_count;
    end loop;
end $$;
