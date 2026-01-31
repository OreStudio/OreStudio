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
 * ISO Standards Coding Schemes Artefact Population Script
 *
 * Auto-generated from external/iso/manifest.json
 * Populates the dq_coding_schemes_artefact_tbl staging table.
 *
 * To publish to production:
 *   SELECT * FROM ores_dq_coding_schemes_publish_fn(
 *       (SELECT id FROM ores_dq_datasets_tbl WHERE code = 'iso.coding_schemes' AND valid_to = ores_utility_infinity_timestamp_fn()),
 *       'upsert'
 *   );
 */

-- =============================================================================
-- ISO Standards Coding Schemes Artefacts
-- =============================================================================

\echo '--- ISO Standards Coding Schemes Artefacts ---'

-- Store dataset_id in psql variable for reuse
select id as v_dataset_id from ores_dq_datasets_tbl where code = 'iso.coding_schemes' and valid_to = ores_utility_infinity_timestamp_fn() \gset

-- Clear existing artefacts for this dataset before inserting
delete from ores_dq_coding_schemes_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into ores_dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'ISO_3166_1_ALPHA_2', 0, 'ISO 3166-1 Alpha-2 Country Code', 'official',
    'Countries', 'Reference Data', 'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-2', 'ISO 3166-1 alpha-2 country codes. Two-letter codes (e.g., US, GB, DE) for countries and dependent territories. The most commonly used country code format in financial messaging.'
);

insert into ores_dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'ISO_3166_1_ALPHA_3', 0, 'ISO 3166-1 Alpha-3 Country Code', 'official',
    'Countries', 'Reference Data', 'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-3', 'ISO 3166-1 alpha-3 country codes. Three-letter codes (e.g., USA, GBR, DEU) for countries and dependent territories. More descriptive than alpha-2 codes.'
);

insert into ores_dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'ISO_4217', 0, 'ISO 4217 Currency Code', 'official',
    'Currencies', 'Reference Data', 'http://www.fpml.org/coding-scheme/external/iso4217', 'ISO 4217 currency codes. Three-letter alphabetic codes (e.g., USD, EUR, GBP) and three-digit numeric codes for currencies. The universal standard for currency identification in financial transactions.'
);

