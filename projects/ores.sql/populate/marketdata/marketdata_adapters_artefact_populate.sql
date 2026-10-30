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
 * Market Data Adapters DQ Dataset
 *
 * Registers the marketdata.adapters dataset and stages the adapter modules the
 * release provides. An adapter is code; a person configures a session of it and
 * then the feeds that come off the session. The dataset is a member of the
 * system_core bundle, so the system provisioning step publishes it into the
 * system tenant through ores_marketdata_publish_adapters_from_dq_fn. Every
 * tenant reads the rows and only the system tenant writes them.
 *
 * This script is idempotent: it replaces the dataset's artefact rows.
 */

\echo '--- DQ Dataset: Market Data Adapters ---'

DO $$
BEGIN
    PERFORM ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'marketdata.adapters',
        'Market Data',
        'Connectivity',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'OreStudio Code Generation Methodology',
        'Market Data Adapters',
        'The adapter modules the release defines: the code that reads a supplier service, the asset classes and services it can serve, and the configuration a session of it must state. A row is a definition; whether this installation has the module is what its announce confirms. The release defines two: Frankfurter for public daily rates, and the synthetic simulator.',
        'ORESTUDIO',
        'The adapter catalogue a session is configured from',
        current_date,
        'Internal Use Only',
        'adapters'
    );
END $$;

\echo '--- DQ Artefact: Market Data Adapters ---'

do $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'marketdata.adapters'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset marketdata.adapters not found.';
    end if;

    delete from ores_dq_adapters_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_adapters_artefact_tbl (
        dataset_id, tenant_id, code, version, name, module_version, description,
        process, asset_classes, services, configuration_schema, display_order
    ) values
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'frankfurter', 0,
         'Frankfurter', '0.0.29',
         'Reads the daily rates Frankfurter serves and sends them as fixings.',
         'ores.marketdata.official', 'fx', 'end_of_day,reference',
         '{"url":{"type":"url","required":false,"default":"https://api.frankfurter.dev"},"backfill_days":{"type":"int","required":false,"default":400},"poll_minutes":{"type":"int","required":false,"default":60}}',
         10),
        (v_dataset_id, ores_utility_system_tenant_id_fn(), 'synthetic', 0,
         'Synthetic simulator', '0.0.29',
         'Runs the synthetic generators the runner authors and sends their ticks.',
         'ores.synthetic.service', 'fx,ir', 'real_time', '{}',
         20);
end $$;

-- Summary
select 'dq_adapters_artefact' as entity, count(*) as count
from ores_dq_adapters_artefact_tbl;
