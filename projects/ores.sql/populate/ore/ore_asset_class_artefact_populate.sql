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
-- DQ Artefact ORE Asset Class
--
-- Seeds the eight ORE asset class codes used by market data series and trades.
-- The description field matches the display label used in the Qt UI so that
-- the data-driven combo filter aligns with asset_class_label() in the model.
-- =============================================================================

\echo '--- DQ Artefact ORE Asset Class ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'ore.asset_class'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset ore.asset_class not found. Run dataset population first.';
    end if;

    delete from ores_dq_asset_classes_artefact_tbl
    where dataset_id = v_dataset_id;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'fx', 1, 'ORE_ASSET_CLASS', 'ORE', 'FX');
    v_count := v_count + 1;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'rates', 1, 'ORE_ASSET_CLASS', 'ORE', 'Rates');
    v_count := v_count + 1;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'credit', 1, 'ORE_ASSET_CLASS', 'ORE', 'Credit');
    v_count := v_count + 1;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'equity', 1, 'ORE_ASSET_CLASS', 'ORE', 'Equity');
    v_count := v_count + 1;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'commodity', 1, 'ORE_ASSET_CLASS', 'ORE', 'Commodity');
    v_count := v_count + 1;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'inflation', 1, 'ORE_ASSET_CLASS', 'ORE', 'Inflation');
    v_count := v_count + 1;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'bond', 1, 'ORE_ASSET_CLASS', 'ORE', 'Bond');
    v_count := v_count + 1;

    insert into ores_dq_asset_classes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (v_dataset_id, ores_iam_system_tenant_id_fn(),
        'cross_asset', 1, 'ORE_ASSET_CLASS', 'ORE', 'Cross Asset');
    v_count := v_count + 1;

    raise notice 'Populated % ORE asset class records into dq_asset_classes_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- ORE Asset Class Artefact Summary ---'

select coding_scheme_code, count(*) as count
from ores_dq_asset_classes_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
