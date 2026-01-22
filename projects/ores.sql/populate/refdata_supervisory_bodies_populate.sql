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
 * Reference Data supervisory_bodies Population Script
 *
 * Populates the refdata_supervisory_bodies_tbl with reference data.
 * Source: supervisory_bodies_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data supervisory_bodies
-- =============================================================================

\echo '--- Reference Data supervisory_bodies ---'

insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ASIC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Australian Securities and Investments Commission',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'ASIC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BankOfRussia',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Central Bank of the Russian Federation',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'BankOfRussia'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.AB.ASC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Alberta Securities Commission',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.AB.ASC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.BC.BCSC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'British Columbia Securities Commission',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.BC.BCSC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.MB.MSC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'The Manitoba Securities Commission',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.MB.MSC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.NB.FCSC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Financial and Consumer Services Commission',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.NB.FCSC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.NL.DSS',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Deputy Superintendent of Securities, Service Newfoundland and Labrador',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.NL.DSS'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.NS.NSSC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Nova Scotia Securities Commission',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.NS.NSSC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.NT.NTSO',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Northwest Territories Securities Office',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.NT.NTSO'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.NU.NSO',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Nunavut Securities Office, Government of Nunavut',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.NU.NSO'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.ON.OSC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Ontario Securities Commission',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.ON.OSC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.PEI.OSS',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Office of the Superintendent of Securities',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.PEI.OSS'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.QC.AMF',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Autorite des marches financiers',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.QC.AMF'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.SK.FCAA',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Financial and Consumer Affairs Authority of Saskatchewan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.SK.FCAA'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CA.YT.OSS',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Office of the Superintendent of Securities',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CA.YT.OSS'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CFTC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Commodity Futures Trading Commission (US)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'CFTC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ESMA',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'European Securities and Markets Authority (European Union)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'ESMA'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FCA',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Financial Conduct Authority (UK)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'FCA'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Fed',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Federal Reserve (US)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'Fed'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HKMA',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Hong Kong Monetary Authority (China)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'HKMA'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'JFSA',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Japan Financial Services Authority (Japan)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'JFSA'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MAS',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'The Monetary Authority of Singapore',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'MAS'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ODRF',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'OTC Derivatives Regulators Forum',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'ODRF'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SEC',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Securities and Exchange Commission (US)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'SEC'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_supervisory_bodies_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'UKFSA',
    0,
    'FPML_SUPERVISORY_BODY',
    'FpML',
    'Deprecated usage: FCA replaces UKFSA',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_supervisory_bodies_tbl
    where code = 'UKFSA'
    and coding_scheme_code = 'FPML_SUPERVISORY_BODY'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'supervisory_bodies' as entity, count(*) as count
from ores.refdata_supervisory_bodies_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_supervisory_bodies_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
