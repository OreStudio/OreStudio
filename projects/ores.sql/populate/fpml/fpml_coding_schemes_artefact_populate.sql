/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Coding Schemes Artefact Population Script
 *
 * Auto-generated from FPML Genericode XML files.
 * Populates the dq_coding_schemes_artefact_tbl staging table.
 *
 * To publish to production:
 *   SELECT * FROM metadata.dq_populate_coding_schemes(
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
    'FPML_ACCOUNT_TYPE', 0, 'accountTypeScheme', 'industry',
    'Trading', 'Reference Data', 'http://www.fpml.org/coding-scheme/account-type', 'Contains a code representing the type of an account, for example in a clearing or exchange model.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_ASSET_CLASS', 0, 'assetClassScheme', 'industry',
    'Market Data', 'Reference Data', 'http://www.fpml.org/coding-scheme/asset-class', 'Defines a simple asset class categorization. Used for classification of the risk class of the trade.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_ASSET_MEASURE', 0, 'assetMeasureScheme', 'industry',
    'Market Data', 'Reference Data', 'http://www.fpml.org/coding-scheme/asset-measure', 'The type of measure about an asset. Used for escribing valuation, sensitivity, and risk measures.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_BENCHMARK_RATE', 0, 'benchmarkRateScheme', 'industry',
    'Market Data', 'Reference Data', 'http://www.fpml.org/coding-scheme/benchmark-rate', 'FpML Benchmark rates'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_BUSINESS_CENTER', 0, 'businessCenterScheme', 'industry',
    'Trading', 'Reference Data', 'http://www.fpml.org/coding-scheme/business-center', 'The coding-scheme accepts a 4 character code of the real geographical business calendar location or FpML format of the rate publication calendar. While the 4 character codes of the business calendar location are implicitly locatable and used for identifying a bad business day for the purpose of payment and rate calculation day adjustments, the rate publication calendar codes are used in the context of the fixing day offsets.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_BUSINESS_PROCESS', 0, 'businessProcessScheme', 'industry',
    'Trading', 'Reference Data', 'http://www.fpml.org/coding-scheme/business-process', 'Contains a code representing the type of business process a message (e.g. a status request) applies to.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_CASHFLOW_TYPE', 0, 'cashflowTypeScheme', 'industry',
    'Trading', 'Reference Data', 'http://www.fpml.org/coding-scheme/cashflow-type', 'The type of cash flows associated with OTC derivatives contracts and their lifecycle events.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_CFTC_ENTITY_CLASSIFICATION', 0, 'entityClassificationScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/cftc-entity-classification', 'Financial Entity Indicator as defined by the CFTC.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_CFTC_ORGANIZATION_TYPE', 0, 'organizationTypeScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/cftc-organization-type', 'Indicates whether a counterparty is an entity established pursuant to a U.S. federal law, including CFTC Amendments to Part 45 (2020).'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_ENTITY_TYPE', 0, 'entityTypeScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/entity-type', 'This specifies the reference entity types corresponding to a list of types defined in the ISDA First to Default documentation.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_LOCAL_JURISDICTION', 0, 'localJurisdictionScheme', 'industry',
    'Regulatory', 'Reference Data', 'http://www.fpml.org/coding-scheme/local-jurisdiction', 'This overrides the countryScheme. Specifies the Local Jurisdiction that applies to a Transaction, for example for the purposes of defining which Local Taxes will apply.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_NON_ISO_CURRENCY', 0, 'nonIsoCurrencyScheme', 'industry',
    'Currencies', 'Reference Data', 'http://www.fpml.org/coding-scheme/non-iso-currency', 'Includes the currency codes to expand the ISO 4217 currency list, including the offshore and historical currencies.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE', 0, 'partyRelationshipTypeScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/hkma-rewrite-party-relationship-type', 'Indicates the relationship between two parties as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_PARTY_RELATIONSHIP_TYPE', 0, 'partyRelationshipTypeScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/party-relationship-type', 'A type is containing a code representing how two parties are related, e.g. Affiliated, Intragroup.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_PARTY_ROLE', 0, 'partyRoleScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/party-role', 'Contains a code representing a related party role. This can be extended to provide custom roles.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_PARTY_ROLE_TYPE', 0, 'partyRoleTypeScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/party-role-type', 'Contains a code representing a related party role type. A type refining the role a role played by a party in one or more transactions. This can be extended to provide custom types.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_PERSON_ROLE', 0, 'personRoleScheme', 'industry',
    'Parties', 'Reference Data', 'http://www.fpml.org/coding-scheme/person-role', 'Indicates the role of a person in a transaction.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR', 0, 'regulatoryCorporateSectorScheme', 'industry',
    'Regulatory', 'Reference Data', 'http://www.fpml.org/coding-scheme/hkma-rewrite-regulatory-corporate-sector', 'Defines the corporate sector under HKMA (Hong Kong Monetary Authority) Rewrite fields 190 - Nature of Counterparty 1 and 191 - Nature of Counterparty 2.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_REGULATORY_CORPORATE_SECTOR', 0, 'regulatoryCorporateSectorScheme', 'industry',
    'Regulatory', 'Reference Data', 'http://www.fpml.org/coding-scheme/regulatory-corporate-sector', 'Specifies Corporate sector as defined by or for regulators including ESMA, CFTC, etc.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_REPORTING_REGIME', 0, 'reportingRegimeNameScheme', 'industry',
    'Regulatory', 'Reference Data', 'http://www.fpml.org/coding-scheme/reporting-regime', 'Contains a code representing a reporting regime under which this transaction may be reported.'
);

insert into metadata.dq_coding_schemes_artefact_tbl (
    dataset_id, code, version, name, authority_type,
    subject_area_name, domain_name, uri, description
) values (
    :'v_dataset_id',
    'FPML_SUPERVISORY_BODY', 0, 'supervisoryBodyScheme', 'industry',
    'Regulatory', 'Reference Data', 'http://www.fpml.org/coding-scheme/supervisory-body', 'Contains a code representing a supervisory-body that may be supervising this transaction.'
);
