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
 * Populates the ores_dq_business_centres_artefact_tbl with reference data.
 * Dataset: fpml.business_center
 *
 * This script is idempotent - clears and repopulates for the dataset.
 * Use ores_dq_populate_business_centres() to publish to production.
 */


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
    from ores_dq_datasets_tbl
    where code = 'fpml.business_center'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset fpml.business_center not found. Run dataset population first.';
    end if;

    -- Clear existing data for this dataset
    delete from ores_dq_business_centres_artefact_tbl
    where dataset_id = v_dataset_id;

    -- Insert reference data
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AEAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abu Dhabi, Business Day (as defined in 2021 ISDA Definitions Section 2.1.10 (ii))'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AEAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abu Dhabi, Settlement Day (as defined in 2021 ISDA Definitions Section 2.1.10 (i))'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dubai, United Arab Emirates'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AMYE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Yerevan, Armenia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AOLU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Luanda, Angola'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ARBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Buenos Aires, Argentina'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ATVI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vienna, Austria'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AUAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Adelaide, Australia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AUBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brisbane, Australia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AUCA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Canberra, Australia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AUDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Darwin, Australia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AUME',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Melbourne, Australia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AUPE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Perth, Australia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AUSY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sydney, Australia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'AZBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Baku, Azerbaijan'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BBBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bridgetown, Barbados'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BDDH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dhaka, Bangladesh'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BEBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brussels, Belgium'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BGSO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sofia, Bulgaria'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BHMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Manama, Bahrain'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BMHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hamilton, Bermuda'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BNBS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bandar Seri Begawan, Brunei'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BOLP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'La Paz, Bolivia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BRBD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brazil Business Day.'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BRBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Brasilia, Brazil.'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BRRJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rio de Janeiro, Brazil.'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BRSP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sao Paulo, Brazil.'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BSNA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nassau, Bahamas'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BWGA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Gaborone, Botswana'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'BYMI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Minsk, Belarus'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CACL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Calgary, Canada'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CAFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Fredericton, Canada.'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CAMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Montreal, Canada'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CAOT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ottawa, Canada'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CATO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Toronto, Canada'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CAVA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vancouver, Canada'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CAWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Winnipeg, Canada'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CHBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Basel, Switzerland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CHGE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Geneva, Switzerland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CHZU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Zurich, Switzerland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CIAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abidjan, Cote d''Ivoire'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CLSA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Santiago, Chile'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CMYA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Yaounde, Cameroon'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CNBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Beijing, China'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CNSH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Shanghai, China'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'COBO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bogota, Colombia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CRSJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Jose, Costa Rica'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CWWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Willemstad, Curacao'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CYNI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nicosia, Cyprus'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'CZPR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Prague, Czech Republic'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DECO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Cologne, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dusseldorf, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DEFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Frankfurt, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DEHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hannover, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DEHH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hamburg, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DELE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Leipzig, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DEMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mainz, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DEMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Munich, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DEST',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Stuttgart, Germany'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DKCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Copenhagen, Denmark'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DOSD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Santo Domingo, Dominican Republic'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'DZAL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Algiers, Algeria'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ECGU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guayaquil, Ecuador'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EETA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tallinn, Estonia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EGCA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Cairo, Egypt'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ESAS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'ESAS Settlement Day (as defined in 2006 ISDA Definitions Section 7.1 and Supplement Number 15 to the 2000 ISDA Definitions)'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ESBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Barcelona, Spain'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ESMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Madrid, Spain'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ESSS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Sebastian, Spain'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ETAA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Addis Ababa, Ethiopia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EUR-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for ICE Swap rates based on EUR-EURIBOR rates'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'EUTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'TARGET Settlement Day'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'FIHE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Helsinki, Finland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'FRPA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Paris, France'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GBED',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Edinburgh, Scotland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GBLO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'London, United Kingdom'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GBP-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for GBP ICE Swap rates'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GETB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tbilisi, Georgia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GGSP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Saint Peter Port, Guernsey'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GHAC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Accra, Ghana'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GIGI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Gibraltar, Gibraltar'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GMBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Banjul, Gambia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GNCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Conakry, Guinea'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GRAT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Athens, Greece'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GTGC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guatemala City, Guatemala'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'GUGC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Guatemala City, Guatemala [DEPRECATED, to be removed in 2024. Replaced by GTGC.]'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HKHK',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hong Kong, Hong Kong'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HNTE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tegucigalpa, Honduras'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HRZA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Zagreb, Republic of Croatia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'HUBU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Budapest, Hungary'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'IDJA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jakarta, Indonesia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'IEDU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dublin, Ireland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ILJE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jerusalem, Israel'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ILS-SHIR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates of the ILS-SHIR index.'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ILS-TELBOR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates of the ILS-TELBOR index.'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ILTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tel Aviv, Israel'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'INAH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ahmedabad, India'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'INBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bangalore, India'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'INCH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Chennai, India'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'INHY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hyderabad, India'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'INKO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kolkata, India'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'INMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mumbai, India'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'INND',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New Delhi, India'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'IQBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Baghdad, Iraq'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'IRTE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Teheran, Iran'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ISRE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Reykjavik, Iceland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ITMI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Milan, Italy'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ITRO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rome, Italy'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ITTU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Turin, Italy'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'JESH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'St. Helier, Channel Islands, Jersey'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'JMKI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kingston, Jamaica'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'JOAM',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Amman, Jordan'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'JPTO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tokyo, Japan'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'KENA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Nairobi, Kenya'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'KHPP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Phnom Penh, Cambodia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'KRSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Seoul, Republic of Korea'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'KWKC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kuwait City, Kuwait'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'KYGE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'George Town, Cayman Islands'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'KZAL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Almaty, Kazakhstan'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'LAVI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Vientiane, Laos'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'LBBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Beirut, Lebanon'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'LKCO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Colombo, Sri Lanka'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'LULU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Luxembourg, Luxembourg'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'LVRI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Riga, Latvia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MACA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Casablanca, Morocco'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MARA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rabat, Morocco'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MCMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Monaco, Monaco'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MNUB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ulan Bator, Mongolia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MOMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Macau, Macao'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MTVA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Valletta, Malta'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MUPL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Port Louis, Mauritius'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MVMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Male, Maldives'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MWLI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lilongwe, Malawi'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MXMC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mexico City, Mexico'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MYKL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kuala Lumpur, Malaysia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MYLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Labuan, Malaysia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'MZMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Maputo, Mozambique'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NAWI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Windhoek, Namibia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NGAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abuja, Nigeria'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NGLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lagos, Nigeria'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NLAM',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Amsterdam, Netherlands'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NLRO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Rotterdam, Netherlands'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NOOS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Oslo, Norway'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NPKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kathmandu, Nepal'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NYFD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York Fed Business Day (as defined in 2006 ISDA Definitions Section 1.9, 2000 ISDA Definitions Section 1.9, and 2021 ISDA Definitions Section 2.1.7)'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NYSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York Stock Exchange Business Day (as defined in 2006 ISDA Definitions Section 1.10, 2000 ISDA Definitions Section 1.10, and 2021 ISDA Definitions Section 2.1.8)'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NZAU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Auckland, New Zealand'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NZBD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New Zealand Business Day (proposed effective date: 2025-10-06)'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'NZWE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Wellington, New Zealand'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'OMMU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Muscat, Oman'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PAPC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Panama City, Panama'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PELI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lima, Peru'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PHMA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Manila, Philippines'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PHMK',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Makati, Philippines'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PKKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Karachi, Pakistan'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PLWA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Warsaw, Poland'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PRSJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Juan, Puerto Rico'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'PTLI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lisbon, Portugal'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'QADO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Doha, Qatar'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ROBU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bucharest, Romania'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'RSBE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Belgrade, Serbia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'RUMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Moscow, Russian Federation'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SAAB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Abha, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SAJE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Jeddah, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SARI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Riyadh, Saudi Arabia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SEST',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Stockholm, Sweden'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SGSI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Singapore, Singapore'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SILJ',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ljubljana, Slovenia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SKBR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bratislava, Slovakia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SLFR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Freetown, Sierra Leone'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SNDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dakar, Senegal'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'SVSS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Salvador, El Salvador'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'THBA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Bangkok, Thailand'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TNTU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tunis, Tunisia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TRAN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ankara, Turkey'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TRIS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Istanbul, Turkey'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TTPS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Port of Spain, Trinidad and Tobago'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TWTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Taipei, Taiwan'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TZDA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dar es Salaam, Tanzania'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'TZDO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Dodoma, Tanzania'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'UAKI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kiev, Ukraine'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'UGKA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Kampala, Uganda'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USBO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Boston, Massachusetts, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USCH',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Chicago, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USCR',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Charlotte, North Carolina, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USDC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Washington, District of Columbia, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USD-ICESWAP',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for ICE Swap rates based on USD-LIBOR rates'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USD-MUNI',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Publication dates for the USD-Municipal Swap Index'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USDN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Denver, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USDT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Detroit, Michigan, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USGS',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'U.S. Government Securities Business Day (as defined in 2006 ISDA Definitions Section 1.11 and 2000 ISDA Definitions Section 1.11)'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USHL',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Honolulu, Hawaii, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USHO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Houston, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USLA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Los Angeles, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USMB',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Mobile, Alabama, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USMN',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Minneapolis, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USNY',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'New York, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USPO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Portland, Oregon, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USSA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Sacramento, California, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USSE',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Seattle, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USSF',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'San Francisco, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'USWT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Wichita, United States'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'UYMO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Montevideo, Uruguay'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'UZTA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Tashkent, Uzbekistan'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'VECA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Caracas, Venezuela'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'VGRT',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Road Town, Virgin Islands (British)'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'VNHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Hanoi, Vietnam'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'VNHC',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Ho Chi Minh (formerly Saigon), Vietnam'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'YEAD',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Aden, Yemen'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ZAJO',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Johannesburg, South Africa'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ZMLU',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Lusaka, Zambia'
    );
    v_count := v_count + 1;
    insert into ores_dq_business_centres_artefact_tbl (
        dataset_id, tenant_id, code, version, coding_scheme_code, source, description
    ) values (
        v_dataset_id,
        ores_iam_system_tenant_id_fn(),
        'ZWHA',
        1,
        'FPML_BUSINESS_CENTER',
        'ISDA',
        'Harare, Zimbabwe'
    );
    v_count := v_count + 1;

    raise notice 'Populated % records into ores_dq_business_centres_artefact_tbl', v_count;
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
    from ores_dq_datasets_tbl
    where code = 'fpml.business_center'
    and valid_to = ores_utility_infinity_timestamp_fn();

    -- Get the flags dataset ID
    select id into v_flags_dataset_id
    from ores_dq_datasets_tbl
    where code = 'assets.country_flags'
    and valid_to = ores_utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Flags dataset assets.country_flags not found. Check dataset dependencies.';
    end if;

    -- Get the placeholder image
    select image_id into v_placeholder_image_id
    from ores_dq_images_artefact_tbl
    where dataset_id = v_flags_dataset_id
      and key = 'xx';

    -- Update records with their flag image_id
    update ores_dq_business_centres_artefact_tbl bc
    set image_id = coalesce(i.image_id, v_placeholder_image_id)
    from ores_dq_images_artefact_tbl i
    where bc.dataset_id = v_dataset_id
      and i.dataset_id = v_flags_dataset_id
      and i.key = lower(substring(bc.code, 1, 2));

    get diagnostics v_updated = row_count;
    raise notice 'Updated % records with flag images', v_updated;

    -- Set placeholder for any without matching flags
    update ores_dq_business_centres_artefact_tbl
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
from ores_dq_business_centres_artefact_tbl;

select coding_scheme_code, count(*) as count
from ores_dq_business_centres_artefact_tbl
group by coding_scheme_code
order by coding_scheme_code;

select 'With flag images' as status, count(*) as count
from ores_dq_business_centres_artefact_tbl
where image_id is not null
union all
select 'Without flag images', count(*)
from ores_dq_business_centres_artefact_tbl
where image_id is null;
