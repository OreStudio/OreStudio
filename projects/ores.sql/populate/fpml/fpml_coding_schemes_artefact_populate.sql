/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Coding Schemes Artefact Population Script
 *
 * Auto-generated from FPML Genericode XML files.
 * Populates the dq_coding_schemes_artefact_tbl staging table.
 *
 * To publish to production:
 *   SELECT * FROM metadata.dq_coding_schemes_publish_fn(
 *       (SELECT id FROM metadata.dq_datasets_tbl WHERE code = 'fpml.coding_schemes' AND valid_to = public.utility_infinity_timestamp_fn()),
 *       'upsert'
 *   );
 */

set schema 'metadata';

-- =============================================================================
-- FPML Coding Schemes Artefacts
-- =============================================================================

\echo '--- FPML Coding Schemes Artefacts ---'

-- Store dataset_id in psql variable for reuse
select id as v_dataset_id from metadata.dq_datasets_tbl where code = 'fpml.coding_schemes' and valid_to = public.utility_infinity_timestamp_fn() \gset

-- Clear existing artefacts for this dataset before inserting
delete from metadata.dq_coding_schemes_artefact_tbl
where dataset_id = :'v_dataset_id';

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_NON_ISO_CURRENCY', 0, 'nonIsoCurrencyScheme', 'industry',
    'Currencies', 'Reference Data', 'http://www.fpml.org/coding-scheme/non-iso-currency', 'Includes the currency codes to expand the ISO 4217 currency list, including the offshore and historical currencies.'
);
