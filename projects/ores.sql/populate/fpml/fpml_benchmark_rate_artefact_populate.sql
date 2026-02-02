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
 * DQ Artefact FpML Benchmark Rate Population Script
 *
 * Populates the dq_benchmark_rates_artefact_tbl with reference data.
 * Dataset: fpml.benchmark_rate
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_benchmark_rates() to publish to production.
 */


-- =============================================================================
-- DQ Artefact FpML Benchmark Rate
-- =============================================================================

\echo '--- DQ Artefact FpML Benchmark Rate ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = 'fpml.benchmark_rate'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.benchmark_rate not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_benchmark_rates_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AMERIBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CORRA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CZEONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DESTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DKK OIS',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EFFR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        '"EFFR" or "Fed Funds" per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EuroSTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HUFONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'KOFR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MIBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MYOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NOWA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NZIONA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'POLONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'POLSTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'RUONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SARON',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SFXROD',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SHIR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SOFR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SONIA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SORA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SORR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'STIBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definition up to V3, Section 10.3 Overnight Rate Benchmarks. What is defined as "SEK OIS" in the 2006 ISDA Collateral Cash Price Matrix up to November 10, 2021 publication.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SWESTR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TELBOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'THOR',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TLREF',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TONA',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks. What is defined as "TONAR" in the 2006 ISDA Collateral Cash Price Matrix.'
    );
    v_count := v_count + 1;
    insert into ores_dq_benchmark_rates_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'WIRON',
        1,
        'FPML_BENCHMARK_RATE',
        'ISDA',
        'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_benchmark_rates_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_benchmark_rates_artefact' as entity, count(*) as count
from ores_dq_benchmark_rates_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores_dq_benchmark_rates_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;
