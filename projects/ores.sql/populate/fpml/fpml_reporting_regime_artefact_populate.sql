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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_populate_refdata.mustache
 * To modify, update the template and regenerate.
 *
 * DQ Artefact FpML Reporting Regime Population Script
 *
 * Populates the dq_reporting_regimes_artefact_tbl with reference data.
 * Dataset: fpml.reporting_regime
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_reporting_regimes() to publish to production.
 */


-- =============================================================================
-- DQ Artefact FpML Reporting Regime
-- =============================================================================

\echo '--- DQ Artefact FpML Reporting Regime ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.reporting_regime'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.reporting_regime not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_reporting_regimes_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ASIC',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Australian Securities and Investments Commission Derivative Transaction Rules (Reporting)'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CA.Rule.91-507',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Rule 91-507 Derivatives: Trade Repositories and Derivatives Data. Harmonized rule adopted by Canadian provinces and territories.'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DoddFrankAct',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Dodd-Frank Act (US)'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EMIR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'European Markets Infrastructure Regulation'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HKMA',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Hong Kong Monetary Authority'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'JFSA',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Japan Financial Services Authority'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MAS',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'The Monetary Authority of Singapore'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MiFID',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Markets in Financial Instruments Directive'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MiFIDII',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Markets in Financial Instruments Directive II'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MiFIR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Markets in Financial Instruments Regulation'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ODRF',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'OTC Derivatives Regulators Forum'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'RussianFederation',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Russian regulatory reporting'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SFTR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'Securities Financing Transactions Regulation'
    );
    v_count := v_count + 1;
    insert into ores_dq_reporting_regimes_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'UKEMIR',
        1,
        'FPML_REPORTING_REGIME',
        'FpML',
        'United Kingdom European Markets Infrastructure Regulation'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_reporting_regimes_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_reporting_regimes_artefact' as entity, count(*) as count
from ores_dq_reporting_regimes_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores_dq_reporting_regimes_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
