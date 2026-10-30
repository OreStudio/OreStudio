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
 * Market Data Suppliers DQ Dataset
 *
 * Registers the marketdata.suppliers dataset and stages the delivery parties a
 * session connects to. A supplier is not the publisher: Frankfurter delivers
 * the ECB's data, and the ECB is the publisher. The dataset is a member of the
 * system_core bundle, so the system provisioning step publishes it into the
 * system tenant through ores_marketdata_publish_suppliers_from_dq_fn. Every
 * tenant reads the rows and only the system tenant writes them.
 *
 * This script is idempotent: it replaces the dataset's artefact rows.
 */

\echo '--- DQ Dataset: Market Data Suppliers ---'

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.suppliers',
        'Market Data',
        'Connectivity',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Market Data Suppliers',
        'The delivery parties a session connects to. The release holds two: Frankfurter, an aggregator that delivers many publishers'' daily rates, and the platform''s own simulator. A supplier is where the data comes from, not who owns it.',
        'ORESTUDIO',
        'The supplier catalogue a session is configured against',
        current_date,
        'Internal Use Only',
        'suppliers'
    );
END $$;

\echo '--- DQ Artefact: Market Data Suppliers ---'

do $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'marketdata.suppliers'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset marketdata.suppliers not found.';
    end if;

    delete from ores_dq_suppliers_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_suppliers_artefact_tbl (
        dataset_id, tenant_id, code, version, name, kind, description,
        contract_reference, support, display_order
    ) values
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'frankfurter', 0,
         'Frankfurter', 'aggregator',
         'An open service that republishes many publishers'' daily rates over one endpoint.',
         '', 'community', 10),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'synthetic', 0,
         'Synthetic simulator', 'internal',
         'The simulator the synthetic runner drives.', '', 'platform team', 20);
end $$;

-- Summary
select 'dq_suppliers_artefact' as entity, count(*) as count
from ores_dq_suppliers_artefact_tbl;
