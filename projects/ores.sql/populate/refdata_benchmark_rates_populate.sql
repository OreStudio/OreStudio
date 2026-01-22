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
 * Reference Data benchmark_rates Population Script
 *
 * Populates the refdata_benchmark_rates_tbl with reference data.
 * Source: benchmark_rates_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data benchmark_rates
-- =============================================================================

\echo '--- Reference Data benchmark_rates ---'

insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AMERIBOR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'AMERIBOR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'AONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CORRA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'CORRA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CZEONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'CZEONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DESTR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'DESTR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DKK OIS',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'DKK OIS'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EFFR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    '"EFFR" or "Fed Funds" per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'EFFR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'EONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EuroSTR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'EuroSTR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'HONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HUFONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'HUFONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'KOFR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'KOFR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MIBOR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'MIBOR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MYOR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'MYOR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NOWA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'NOWA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NZIONA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'NZIONA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'POLONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'POLONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'POLSTR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'POLSTR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RUONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'RUONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SARON',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SARON'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SFXROD',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SFXROD'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SHIR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SHIR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SOFR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SOFR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SONIA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SONIA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SORA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SORA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SORR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SORR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'STIBOR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definition up to V3, Section 10.3 Overnight Rate Benchmarks. What is defined as "SEK OIS" in the 2006 ISDA Collateral Cash Price Matrix up to November 10, 2021 publication.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'STIBOR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SWESTR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'SWESTR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TELBOR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'TELBOR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'THOR',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'THOR'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TLREF',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'TLREF'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TONA',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks. What is defined as "TONAR" in the 2006 ISDA Collateral Cash Price Matrix.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'TONA'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_benchmark_rates_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'WIRON',
    0,
    'FPML_BENCHMARK_RATE',
    'ISDA',
    'Per 2021 ISDA Definitions, Section 10.3 Overnight Rate Benchmarks.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_benchmark_rates_tbl
    where code = 'WIRON'
    and coding_scheme_code = 'FPML_BENCHMARK_RATE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'benchmark_rates' as entity, count(*) as count
from ores.refdata_benchmark_rates_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_benchmark_rates_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
