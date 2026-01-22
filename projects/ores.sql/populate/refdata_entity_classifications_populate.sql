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
 * Reference Data entity_classifications Population Script
 *
 * Populates the refdata_entity_classifications_tbl with reference data.
 * Source: entity_classifications_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data entity_classifications
-- =============================================================================

\echo '--- Reference Data entity_classifications ---'

insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CommodityPool',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'A commodity pool as defined in CFTC CEA § 2(h)(7)(C).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'CommodityPool'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EmployeeBenefitPlan',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'An employee benefit plan as defined in paragraphs (3) and (32) of section 1002 of title 29 of the Commodity Exchange Act (CEA).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'EmployeeBenefitPlan'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FinancialSectorPerson',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'A person predominantly engaged in activities that are in the business of banking, or in activities that are financial in nature, as defined in section 1843(k) of title 12 of the Commodity Exchange Act (CEA).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'FinancialSectorPerson'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MSBSP',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'A major security based swap participant as defined in CFTC CEA § 2(h)(7)(C).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'MSBSP'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MSP',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'A major swap participant as defined in CFTC CEA § 2(h)(7)(C).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'MSP'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'None',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'None of the listed in the scheme.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'None'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PrivateFund',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'A private fund as defined in section 80b-2(a) of title 15 of the Commodity Exchange Act (CEA).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'PrivateFund'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SBSD',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'A security-based swap dealer as defined in CFTC CEA § 2(h)(7)(C).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'SBSD'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SD',
    0,
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'FpML',
    'A swap dealer as defined in CFTC CEA § 2(h)(7)(C).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'SD'
    and coding_scheme_code = 'FPML_CFTC_ENTITY_CLASSIFICATION'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Agency',
    0,
    'FPML_CFTC_ORGANIZATION_TYPE',
    'FpML',
    'An agency as defined in 5 U.S.C. 551(1), a federal instrumentality, or a federal authority.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'Agency'
    and coding_scheme_code = 'FPML_CFTC_ORGANIZATION_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CharteredPursuantToFederalLaw',
    0,
    'FPML_CFTC_ORGANIZATION_TYPE',
    'FpML',
    'An entity chartered pursuant to federal law after formation (example: an organization listed in title 36 of the U.S. Code).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'CharteredPursuantToFederalLaw'
    and coding_scheme_code = 'FPML_CFTC_ORGANIZATION_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EstablishedByFederalEntity',
    0,
    'FPML_CFTC_ORGANIZATION_TYPE',
    'FpML',
    'An entity that was established by, or at the direction of, one or more of the entities listed in clause (1), or has an ultimate parent listed in its LEI reference data that is an entity listed in clause (1) or in the first part of this clause (2).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'EstablishedByFederalEntity'
    and coding_scheme_code = 'FPML_CFTC_ORGANIZATION_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FederallyFundedResearchAndDevelopmentCenter',
    0,
    'FPML_CFTC_ORGANIZATION_TYPE',
    'FpML',
    'A federally funded research and development center on the master list referenced in 48 CFR 35.017-6.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'FederallyFundedResearchAndDevelopmentCenter'
    and coding_scheme_code = 'FPML_CFTC_ORGANIZATION_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GovernmentCorporation',
    0,
    'FPML_CFTC_ORGANIZATION_TYPE',
    'FpML',
    'A government corporation (examples: as such term is defined in 5 U.S.C. 103(1) or in 31 U.S.C. 9101).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'GovernmentCorporation'
    and coding_scheme_code = 'FPML_CFTC_ORGANIZATION_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GovernmentSponsoredEnterprise',
    0,
    'FPML_CFTC_ORGANIZATION_TYPE',
    'FpML',
    'A government-sponsored enterprise (example: as such term is defined in 2 U.S.C. 622(8)).',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'GovernmentSponsoredEnterprise'
    and coding_scheme_code = 'FPML_CFTC_ORGANIZATION_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USCListedExecutiveDepartment',
    0,
    'FPML_CFTC_ORGANIZATION_TYPE',
    'FpML',
    'An executive department listed in 5 U.S.C. 101.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'USCListedExecutiveDepartment'
    and coding_scheme_code = 'FPML_CFTC_ORGANIZATION_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Asian',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of Asian.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'Asian'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AustralianAndNewZealand',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of Australian and New Zealand.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'AustralianAndNewZealand'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EuropeanEmergingMarkets',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of European Emerging Markets.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'EuropeanEmergingMarkets'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Japanese',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of Japanese.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'Japanese'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NorthAmericanHighYield',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of North American High Yield.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'NorthAmericanHighYield'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NorthAmericanInsurance',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of North American Insurance.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'NorthAmericanInsurance'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NorthAmericanInvestmentGrade',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of North American Investment Grade.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'NorthAmericanInvestmentGrade'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Singaporean',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of Singaporean.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'Singaporean'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'WesternEuropean',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of Western European.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'WesternEuropean'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_entity_classifications_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'WesternEuropeanInsurance',
    0,
    'FPML_ENTITY_TYPE',
    'FpML',
    'Entity Type of Western European Insurance.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_entity_classifications_tbl
    where code = 'WesternEuropeanInsurance'
    and coding_scheme_code = 'FPML_ENTITY_TYPE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'entity_classifications' as entity, count(*) as count
from ores.refdata_entity_classifications_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_entity_classifications_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
