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
 * DQ Artefact FpML Business Center Population Script
 *
 * Populates the dq_business_centres_artefact_tbl with reference data.
 * Dataset: fpml.business_center
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use dq_populate_business_centres() to publish to production.
 */

set schema 'metadata';

-- =============================================================================
-- DQ Artefact FpML Business Center
-- =============================================================================

\echo '--- DQ Artefact FpML Business Center ---'

do $$
declare
    v_dataset_id uuid;
    v_count integer := 0;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.business_center'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.business_center not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from metadata.dq_business_centres_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AEAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abu Dhabi, Business Day (as defined in 2021 ISDA Definitions Section 2.1.10 (ii))'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AEAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abu Dhabi, Settlement Day (as defined in 2021 ISDA Definitions Section 2.1.10 (i))'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dubai, United Arab Emirates'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AMYE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Yerevan, Armenia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AOLU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Luanda, Angola'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ARBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Buenos Aires, Argentina'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ATVI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vienna, Austria'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Adelaide, Australia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brisbane, Australia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUCA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Canberra, Australia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Darwin, Australia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUME',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Melbourne, Australia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUPE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Perth, Australia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AUSY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sydney, Australia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'AZBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Baku, Azerbaijan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BBBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bridgetown, Barbados'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BDDH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dhaka, Bangladesh'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BEBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brussels, Belgium'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BGSO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sofia, Bulgaria'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BHMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Manama, Bahrain'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BMHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hamilton, Bermuda'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BNBS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bandar Seri Begawan, Brunei'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BOLP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'La Paz, Bolivia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRBD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brazil Business Day.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brasilia, Brazil.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRRJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rio de Janeiro, Brazil.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BRSP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sao Paulo, Brazil.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BSNA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nassau, Bahamas'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BWGA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Gaborone, Botswana'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'BYMI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Minsk, Belarus'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CACL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Calgary, Canada'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Fredericton, Canada.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Montreal, Canada'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAOT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ottawa, Canada'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CATO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Toronto, Canada'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAVA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vancouver, Canada'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CAWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Winnipeg, Canada'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CHBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Basel, Switzerland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CHGE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Geneva, Switzerland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CHZU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Zurich, Switzerland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CIAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abidjan, Cote d''Ivoire'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CLSA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Santiago, Chile'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CMYA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Yaounde, Cameroon'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CNBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Beijing, China'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CNSH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Shanghai, China'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'COBO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bogota, Colombia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CRSJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Jose, Costa Rica'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CWWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Willemstad, Curacao'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CYNI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nicosia, Cyprus'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'CZPR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Prague, Czech Republic'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DECO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Cologne, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dusseldorf, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Frankfurt, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hannover, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEHH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hamburg, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DELE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Leipzig, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mainz, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Munich, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DEST',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Stuttgart, Germany'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DKCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Copenhagen, Denmark'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DOSD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Santo Domingo, Dominican Republic'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'DZAL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Algiers, Algeria'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ECGU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guayaquil, Ecuador'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EETA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tallinn, Estonia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EGCA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Cairo, Egypt'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESAS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'ESAS Settlement Day (as defined in 2006 ISDA Definitions Section 7.1 and Supplement Number 15 to the 2000 ISDA Definitions)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Barcelona, Spain'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Madrid, Spain'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ESSS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Sebastian, Spain'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ETAA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Addis Ababa, Ethiopia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EUR-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for ICE Swap rates based on EUR-EURIBOR rates'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'EUTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'TARGET Settlement Day'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FIHE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Helsinki, Finland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'FRPA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Paris, France'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GBED',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Edinburgh, Scotland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GBLO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'London, United Kingdom'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GBP-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for GBP ICE Swap rates'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GETB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tbilisi, Georgia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GGSP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Saint Peter Port, Guernsey'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GHAC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Accra, Ghana'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GIGI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Gibraltar, Gibraltar'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GMBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Banjul, Gambia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GNCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Conakry, Guinea'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GRAT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Athens, Greece'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GTGC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guatemala City, Guatemala'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'GUGC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guatemala City, Guatemala [DEPRECATED, to be removed in 2024. Replaced by GTGC.]'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HKHK',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hong Kong, Hong Kong'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HNTE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tegucigalpa, Honduras'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HRZA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Zagreb, Republic of Croatia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'HUBU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Budapest, Hungary'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IDJA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jakarta, Indonesia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dublin, Ireland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILJE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jerusalem, Israel'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILS-SHIR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates of the ILS-SHIR index.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILS-TELBOR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates of the ILS-TELBOR index.'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ILTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tel Aviv, Israel'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INAH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ahmedabad, India'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bangalore, India'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INCH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Chennai, India'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INHY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hyderabad, India'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INKO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kolkata, India'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mumbai, India'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'INND',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New Delhi, India'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IQBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Baghdad, Iraq'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'IRTE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Teheran, Iran'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ISRE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Reykjavik, Iceland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ITMI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Milan, Italy'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ITRO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rome, Italy'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ITTU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Turin, Italy'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JESH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'St. Helier, Channel Islands, Jersey'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JMKI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kingston, Jamaica'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JOAM',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Amman, Jordan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'JPTO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tokyo, Japan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KENA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nairobi, Kenya'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KHPP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Phnom Penh, Cambodia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KRSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Seoul, Republic of Korea'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KWKC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kuwait City, Kuwait'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KYGE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'George Town, Cayman Islands'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'KZAL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Almaty, Kazakhstan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LAVI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vientiane, Laos'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LBBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Beirut, Lebanon'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LKCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Colombo, Sri Lanka'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LULU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Luxembourg, Luxembourg'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'LVRI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Riga, Latvia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MACA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Casablanca, Morocco'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MARA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rabat, Morocco'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MCMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Monaco, Monaco'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MNUB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ulan Bator, Mongolia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MOMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Macau, Macao'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MTVA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Valletta, Malta'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MUPL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Port Louis, Mauritius'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MVMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Male, Maldives'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MWLI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lilongwe, Malawi'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MXMC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mexico City, Mexico'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MYKL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kuala Lumpur, Malaysia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MYLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Labuan, Malaysia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'MZMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Maputo, Mozambique'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NAWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Windhoek, Namibia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NGAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abuja, Nigeria'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NGLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lagos, Nigeria'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NLAM',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Amsterdam, Netherlands'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NLRO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rotterdam, Netherlands'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NOOS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Oslo, Norway'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NPKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kathmandu, Nepal'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NYFD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York Fed Business Day (as defined in 2006 ISDA Definitions Section 1.9, 2000 ISDA Definitions Section 1.9, and 2021 ISDA Definitions Section 2.1.7)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NYSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York Stock Exchange Business Day (as defined in 2006 ISDA Definitions Section 1.10, 2000 ISDA Definitions Section 1.10, and 2021 ISDA Definitions Section 2.1.8)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NZAU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Auckland, New Zealand'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NZBD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New Zealand Business Day (proposed effective date: 2025-10-06)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'NZWE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Wellington, New Zealand'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'OMMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Muscat, Oman'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PAPC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Panama City, Panama'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PELI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lima, Peru'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PHMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Manila, Philippines'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PHMK',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Makati, Philippines'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PKKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Karachi, Pakistan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PLWA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Warsaw, Poland'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PRSJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Juan, Puerto Rico'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'PTLI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lisbon, Portugal'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'QADO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Doha, Qatar'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ROBU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bucharest, Romania'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RSBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Belgrade, Serbia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'RUMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Moscow, Russian Federation'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SAAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abha, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SAJE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jeddah, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SARI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Riyadh, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SEST',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Stockholm, Sweden'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SGSI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Singapore, Singapore'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SILJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ljubljana, Slovenia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SKBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bratislava, Slovakia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SLFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Freetown, Sierra Leone'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SNDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dakar, Senegal'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'SVSS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Salvador, El Salvador'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'THBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bangkok, Thailand'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TNTU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tunis, Tunisia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TRAN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ankara, Turkey'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TRIS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Istanbul, Turkey'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TTPS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Port of Spain, Trinidad and Tobago'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TWTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Taipei, Taiwan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TZDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dar es Salaam, Tanzania'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'TZDO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dodoma, Tanzania'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UAKI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kiev, Ukraine'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UGKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kampala, Uganda'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USBO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Boston, Massachusetts, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USCH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Chicago, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USCR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Charlotte, North Carolina, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USDC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Washington, District of Columbia, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USD-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for ICE Swap rates based on USD-LIBOR rates'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USD-MUNI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for the USD-Municipal Swap Index'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USDN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Denver, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USDT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Detroit, Michigan, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USGS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'U.S. Government Securities Business Day (as defined in 2006 ISDA Definitions Section 1.11 and 2000 ISDA Definitions Section 1.11)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USHL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Honolulu, Hawaii, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USHO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Houston, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Los Angeles, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USMB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mobile, Alabama, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USMN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Minneapolis, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USNY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USPO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Portland, Oregon, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USSA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sacramento, California, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Seattle, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USSF',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Francisco, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'USWT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Wichita, United States'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UYMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Montevideo, Uruguay'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'UZTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tashkent, Uzbekistan'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VECA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Caracas, Venezuela'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VGRT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Road Town, Virgin Islands (British)'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VNHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hanoi, Vietnam'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'VNHC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ho Chi Minh (formerly Saigon), Vietnam'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'YEAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Aden, Yemen'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ZAJO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Johannesburg, South Africa'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ZMLU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lusaka, Zambia'
    );
    v_count := v_count + 1;
    insert into metadata.dq_business_centres_artefact_tbl (
        dataset_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        'ZWHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Harare, Zimbabwe'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into dq_business_centres_artefact_tbl', v_count;
end;
$$;

-- =============================================================================
-- Link to flag images
-- =============================================================================

\echo '--- Linking to Flag Images ---'

do $$
declare
    v_dataset_id uuid;
    v_flags_dataset_id uuid;
    v_placeholder_image_id uuid;
    v_updated integer;
begin
    -- Get the dataset ID
    select id into v_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'fpml.business_center'
    and valid_to = public.utility_infinity_timestamp_fn();

    -- Get the flags dataset ID
    select id into v_flags_dataset_id
    from metadata.dq_datasets_tbl
    where code = 'assets.country_flags'
    and valid_to = public.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Flags dataset assets.country_flags not found. Check dataset dependencies.';
    end if;

    -- Get the placeholder image
    select image_id into v_placeholder_image_id
    from metadata.dq_images_artefact_tbl
    where dataset_id = v_flags_dataset_id
      and key = 'xx';

    -- Update records with their flag image_id
    update metadata.dq_business_centres_artefact_tbl bc
    set image_id = coalesce(i.image_id, v_placeholder_image_id)
    from metadata.dq_images_artefact_tbl i
    where bc.dataset_id = v_dataset_id
      and i.dataset_id = v_flags_dataset_id
      and i.key = lower(substring(bc.code, 1, 2));

    get diagnostics v_updated = row_count;
    raise notice 'Updated % records with flag images', v_updated;

    -- Set placeholder for any without matching flags
    update metadata.dq_business_centres_artefact_tbl
    set image_id = v_placeholder_image_id
    where dataset_id = v_dataset_id
      and image_id is null
      and v_placeholder_image_id is not null;

    get diagnostics v_updated = row_count;
    if v_updated > 0 then
        raise notice 'Set placeholder image for % records without matching flags', v_updated;
    end if;
end;
$$;

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_business_centres_artefact' as entity, count(*) as count
from metadata.dq_business_centres_artefact_tbl;

select coding_scheme_code, count(*) as count
from metadata.dq_business_centres_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;

select 'With flag images' as status, count(*) as count
from metadata.dq_business_centres_artefact_tbl
where image_id is not null
union all
select 'Without flag images', count(*)
from metadata.dq_business_centres_artefact_tbl
where image_id is null;
