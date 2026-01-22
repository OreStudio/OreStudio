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
 * Reference Data business_centres Population Script
 *
 * Populates the refdata_business_centres_tbl with reference data.
 * Source: business_centres_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data business_centres
-- =============================================================================

\echo '--- Reference Data business_centres ---'

insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AEAB',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Abu Dhabi, Business Day (as defined in 2021 ISDA Definitions Section 2.1.10 (ii))',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AEAB'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AEAD',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Abu Dhabi, Settlement Day (as defined in 2021 ISDA Definitions Section 2.1.10 (i))',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AEAD'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AEDU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Dubai, United Arab Emirates',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AEDU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AMYE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Yerevan, Armenia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AMYE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AOLU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Luanda, Angola',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AOLU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ARBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Buenos Aires, Argentina',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ARBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ATVI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Vienna, Austria',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ATVI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AUAD',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Adelaide, Australia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AUAD'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AUBR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Brisbane, Australia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AUBR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AUCA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Canberra, Australia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AUCA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AUDA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Darwin, Australia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AUDA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AUME',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Melbourne, Australia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AUME'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AUPE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Perth, Australia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AUPE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AUSY',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Sydney, Australia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AUSY'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AZBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Baku, Azerbaijan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'AZBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BBBR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Bridgetown, Barbados',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BBBR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BDDH',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Dhaka, Bangladesh',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BDDH'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BEBR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Brussels, Belgium',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BEBR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BGSO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Sofia, Bulgaria',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BGSO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BHMA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Manama, Bahrain',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BHMA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BMHA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Hamilton, Bermuda',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BMHA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BNBS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Bandar Seri Begawan, Brunei',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BNBS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BOLP',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'La Paz, Bolivia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BOLP'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BRBD',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Brazil Business Day.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BRBD'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BRBR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Brasilia, Brazil.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BRBR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BRRJ',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Rio de Janeiro, Brazil.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BRRJ'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BRSP',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Sao Paulo, Brazil.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BRSP'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BSNA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Nassau, Bahamas',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BSNA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BWGA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Gaborone, Botswana',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BWGA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BYMI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Minsk, Belarus',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'BYMI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CACL',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Calgary, Canada',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CACL'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CAFR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Fredericton, Canada.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CAFR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CAMO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Montreal, Canada',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CAMO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CAOT',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Ottawa, Canada',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CAOT'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CATO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Toronto, Canada',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CATO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CAVA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Vancouver, Canada',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CAVA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CAWI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Winnipeg, Canada',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CAWI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CHBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Basel, Switzerland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CHBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CHGE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Geneva, Switzerland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CHGE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CHZU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Zurich, Switzerland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CHZU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CIAB',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Abidjan, Cote d''Ivoire',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CIAB'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CLSA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Santiago, Chile',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CLSA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CMYA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Yaounde, Cameroon',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CMYA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CNBE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Beijing, China',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CNBE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CNSH',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Shanghai, China',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CNSH'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'COBO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Bogota, Colombia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'COBO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CRSJ',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'San Jose, Costa Rica',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CRSJ'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CWWI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Willemstad, Curacao',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CWWI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CYNI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Nicosia, Cyprus',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CYNI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CZPR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Prague, Czech Republic',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'CZPR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DECO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Cologne, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DECO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DEDU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Dusseldorf, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DEDU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DEFR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Frankfurt, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DEFR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DEHA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Hannover, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DEHA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DEHH',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Hamburg, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DEHH'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DELE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Leipzig, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DELE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DEMA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Mainz, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DEMA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DEMU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Munich, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DEMU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DEST',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Stuttgart, Germany',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DEST'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DKCO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Copenhagen, Denmark',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DKCO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DOSD',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Santo Domingo, Dominican Republic',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DOSD'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DZAL',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Algiers, Algeria',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'DZAL'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ECGU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Guayaquil, Ecuador',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ECGU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EETA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Tallinn, Estonia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'EETA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EGCA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Cairo, Egypt',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'EGCA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ESAS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'ESAS Settlement Day (as defined in 2006 ISDA Definitions Section 7.1 and Supplement Number 15 to the 2000 ISDA Definitions)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ESAS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ESBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Barcelona, Spain',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ESBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ESMA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Madrid, Spain',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ESMA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ESSS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'San Sebastian, Spain',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ESSS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ETAA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Addis Ababa, Ethiopia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ETAA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EUR-ICESWAP',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Publication dates for ICE Swap rates based on EUR-EURIBOR rates',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'EUR-ICESWAP'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'EUTA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'TARGET Settlement Day',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'EUTA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FIHE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Helsinki, Finland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'FIHE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'FRPA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Paris, France',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'FRPA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GBED',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Edinburgh, Scotland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GBED'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GBLO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'London, United Kingdom',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GBLO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GBP-ICESWAP',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Publication dates for GBP ICE Swap rates',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GBP-ICESWAP'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GETB',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Tbilisi, Georgia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GETB'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GGSP',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Saint Peter Port, Guernsey',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GGSP'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GHAC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Accra, Ghana',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GHAC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GIGI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Gibraltar, Gibraltar',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GIGI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GMBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Banjul, Gambia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GMBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GNCO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Conakry, Guinea',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GNCO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GRAT',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Athens, Greece',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GRAT'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GTGC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Guatemala City, Guatemala',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GTGC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'GUGC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Guatemala City, Guatemala [DEPRECATED, to be removed in 2024. Replaced by GTGC.]',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'GUGC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HKHK',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Hong Kong, Hong Kong',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'HKHK'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HNTE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Tegucigalpa, Honduras',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'HNTE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HRZA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Zagreb, Republic of Croatia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'HRZA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'HUBU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Budapest, Hungary',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'HUBU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'IDJA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Jakarta, Indonesia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'IDJA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'IEDU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Dublin, Ireland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'IEDU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ILJE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Jerusalem, Israel',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ILJE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ILS-SHIR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Publication dates of the ILS-SHIR index.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ILS-SHIR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ILS-TELBOR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Publication dates of the ILS-TELBOR index.',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ILS-TELBOR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ILTA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Tel Aviv, Israel',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ILTA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INAH',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Ahmedabad, India',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'INAH'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Bangalore, India',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'INBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INCH',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Chennai, India',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'INCH'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INHY',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Hyderabad, India',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'INHY'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INKO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Kolkata, India',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'INKO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INMU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Mumbai, India',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'INMU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'INND',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'New Delhi, India',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'INND'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'IQBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Baghdad, Iraq',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'IQBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'IRTE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Teheran, Iran',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'IRTE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ISRE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Reykjavik, Iceland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ISRE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ITMI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Milan, Italy',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ITMI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ITRO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Rome, Italy',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ITRO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ITTU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Turin, Italy',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ITTU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'JESH',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'St. Helier, Channel Islands, Jersey',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'JESH'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'JMKI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Kingston, Jamaica',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'JMKI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'JOAM',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Amman, Jordan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'JOAM'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'JPTO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Tokyo, Japan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'JPTO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'KENA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Nairobi, Kenya',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'KENA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'KHPP',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Phnom Penh, Cambodia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'KHPP'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'KRSE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Seoul, Republic of Korea',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'KRSE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'KWKC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Kuwait City, Kuwait',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'KWKC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'KYGE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'George Town, Cayman Islands',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'KYGE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'KZAL',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Almaty, Kazakhstan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'KZAL'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LAVI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Vientiane, Laos',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'LAVI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LBBE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Beirut, Lebanon',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'LBBE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LKCO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Colombo, Sri Lanka',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'LKCO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LULU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Luxembourg, Luxembourg',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'LULU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'LVRI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Riga, Latvia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'LVRI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MACA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Casablanca, Morocco',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MACA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MARA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Rabat, Morocco',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MARA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MCMO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Monaco, Monaco',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MCMO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MNUB',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Ulan Bator, Mongolia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MNUB'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MOMA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Macau, Macao',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MOMA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MTVA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Valletta, Malta',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MTVA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MUPL',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Port Louis, Mauritius',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MUPL'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MVMA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Male, Maldives',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MVMA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MWLI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Lilongwe, Malawi',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MWLI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MXMC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Mexico City, Mexico',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MXMC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MYKL',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Kuala Lumpur, Malaysia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MYKL'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MYLA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Labuan, Malaysia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MYLA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MZMA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Maputo, Mozambique',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'MZMA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NAWI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Windhoek, Namibia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NAWI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NGAB',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Abuja, Nigeria',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NGAB'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NGLA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Lagos, Nigeria',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NGLA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NLAM',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Amsterdam, Netherlands',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NLAM'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NLRO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Rotterdam, Netherlands',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NLRO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NOOS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Oslo, Norway',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NOOS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NPKA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Kathmandu, Nepal',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NPKA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NYFD',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'New York Fed Business Day (as defined in 2006 ISDA Definitions Section 1.9, 2000 ISDA Definitions Section 1.9, and 2021 ISDA Definitions Section 2.1.7)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NYFD'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NYSE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'New York Stock Exchange Business Day (as defined in 2006 ISDA Definitions Section 1.10, 2000 ISDA Definitions Section 1.10, and 2021 ISDA Definitions Section 2.1.8)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NYSE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NZAU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Auckland, New Zealand',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NZAU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NZBD',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'New Zealand Business Day (proposed effective date: 2025-10-06)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NZBD'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'NZWE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Wellington, New Zealand',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'NZWE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'OMMU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Muscat, Oman',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'OMMU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PAPC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Panama City, Panama',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PAPC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PELI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Lima, Peru',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PELI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PHMA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Manila, Philippines',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PHMA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PHMK',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Makati, Philippines',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PHMK'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PKKA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Karachi, Pakistan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PKKA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PLWA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Warsaw, Poland',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PLWA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PRSJ',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'San Juan, Puerto Rico',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PRSJ'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PTLI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Lisbon, Portugal',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'PTLI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'QADO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Doha, Qatar',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'QADO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ROBU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Bucharest, Romania',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ROBU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RSBE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Belgrade, Serbia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'RSBE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'RUMO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Moscow, Russian Federation',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'RUMO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SAAB',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Abha, Saudi Arabia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SAAB'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SAJE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Jeddah, Saudi Arabia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SAJE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SARI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Riyadh, Saudi Arabia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SARI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SEST',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Stockholm, Sweden',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SEST'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SGSI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Singapore, Singapore',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SGSI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SILJ',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Ljubljana, Slovenia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SILJ'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SKBR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Bratislava, Slovakia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SKBR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SLFR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Freetown, Sierra Leone',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SLFR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SNDA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Dakar, Senegal',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SNDA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SVSS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'San Salvador, El Salvador',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'SVSS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'THBA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Bangkok, Thailand',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'THBA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TNTU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Tunis, Tunisia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'TNTU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TRAN',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Ankara, Turkey',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'TRAN'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TRIS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Istanbul, Turkey',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'TRIS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TTPS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Port of Spain, Trinidad and Tobago',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'TTPS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TWTA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Taipei, Taiwan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'TWTA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TZDA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Dar es Salaam, Tanzania',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'TZDA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TZDO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Dodoma, Tanzania',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'TZDO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'UAKI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Kiev, Ukraine',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'UAKI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'UGKA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Kampala, Uganda',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'UGKA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USBO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Boston, Massachusetts, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USBO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USCH',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Chicago, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USCH'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USCR',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Charlotte, North Carolina, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USCR'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USDC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Washington, District of Columbia, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USDC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USD-ICESWAP',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Publication dates for ICE Swap rates based on USD-LIBOR rates',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USD-ICESWAP'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USD-MUNI',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Publication dates for the USD-Municipal Swap Index',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USD-MUNI'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USDN',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Denver, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USDN'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USDT',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Detroit, Michigan, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USDT'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USGS',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'U.S. Government Securities Business Day (as defined in 2006 ISDA Definitions Section 1.11 and 2000 ISDA Definitions Section 1.11)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USGS'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USHL',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Honolulu, Hawaii, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USHL'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USHO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Houston, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USHO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USLA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Los Angeles, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USLA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USMB',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Mobile, Alabama, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USMB'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USMN',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Minneapolis, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USMN'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USNY',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'New York, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USNY'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USPO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Portland, Oregon, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USPO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USSA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Sacramento, California, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USSA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USSE',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Seattle, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USSE'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USSF',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'San Francisco, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USSF'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'USWT',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Wichita, United States',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'USWT'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'UYMO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Montevideo, Uruguay',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'UYMO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'UZTA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Tashkent, Uzbekistan',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'UZTA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'VECA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Caracas, Venezuela',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'VECA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'VGRT',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Road Town, Virgin Islands (British)',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'VGRT'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'VNHA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Hanoi, Vietnam',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'VNHA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'VNHC',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Ho Chi Minh (formerly Saigon), Vietnam',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'VNHC'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'YEAD',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Aden, Yemen',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'YEAD'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ZAJO',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Johannesburg, South Africa',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ZAJO'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ZMLU',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Lusaka, Zambia',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ZMLU'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_business_centres_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ZWHA',
    0,
    'FPML_BUSINESS_CENTER',
    'ISDA',
    'Harare, Zimbabwe',
    'system',
    'system.initial_load',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_business_centres_tbl
    where code = 'ZWHA'
    and coding_scheme_code = 'FPML_BUSINESS_CENTER'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'business_centres' as entity, count(*) as count
from ores.refdata_business_centres_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_business_centres_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
