/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * FPML Coding Schemes Population Script
 *
 * Auto-generated from FPML Genericode XML files.
 * This script is idempotent.
 */

set schema 'metadata';

-- =============================================================================
-- FPML Coding Schemes
-- =============================================================================

\echo '--- FPML Coding Schemes ---'

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_ACCOUNT_TYPE',
    'accountTypeScheme',
    'industry',
    'Trading',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/account-type',
    'Contains a code representing the type of an account, for example in a clearing or exchange model.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_ASSET_CLASS',
    'assetClassScheme',
    'industry',
    'Market Data',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/asset-class',
    'Defines a simple asset class categorization. Used for classification of the risk class of the trade.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_ASSET_MEASURE',
    'assetMeasureScheme',
    'industry',
    'Market Data',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/asset-measure',
    'The type of measure about an asset. Used for escribing valuation, sensitivity, and risk measures.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_BENCHMARK_RATE',
    'benchmarkRateScheme',
    'industry',
    'Market Data',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/benchmark-rate',
    'FpML Benchmark rates'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_BUSINESS_CENTER',
    'businessCenterScheme',
    'industry',
    'Trading',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/business-center',
    'The coding-scheme accepts a 4 character code of the real geographical business calendar location or FpML format of the rate publication calendar. While the 4 character codes of the business calendar location are implicitly locatable and used for identifying a bad business day for the purpose of payment and rate calculation day adjustments, the rate publication calendar codes are used in the context of the fixing day offsets.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_BUSINESS_PROCESS',
    'businessProcessScheme',
    'industry',
    'Trading',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/business-process',
    'Contains a code representing the type of business process a message (e.g. a status request) applies to.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_CASHFLOW_TYPE',
    'cashflowTypeScheme',
    'industry',
    'Trading',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/cashflow-type',
    'The type of cash flows associated with OTC derivatives contracts and their lifecycle events.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_CFTC_ENTITY_CLASSIFICATION',
    'entityClassificationScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/cftc-entity-classification',
    'Financial Entity Indicator as defined by the CFTC.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_CFTC_ORGANIZATION_TYPE',
    'organizationTypeScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/cftc-organization-type',
    'Indicates whether a counterparty is an entity established pursuant to a U.S. federal law, including CFTC Amendments to Part 45 (2020).'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_ENTITY_TYPE',
    'entityTypeScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/entity-type',
    'This specifies the reference entity types corresponding to a list of types defined in the ISDA First to Default documentation.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_LOCAL_JURISDICTION',
    'localJurisdictionScheme',
    'industry',
    'Regulatory',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/local-jurisdiction',
    'This overrides the countryScheme. Specifies the Local Jurisdiction that applies to a Transaction, for example for the purposes of defining which Local Taxes will apply.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_NON_ISO_CURRENCY',
    'nonIsoCurrencyScheme',
    'industry',
    'Currencies',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/non-iso-currency',
    'Includes the currency codes to expand the ISO 4217 currency list, including the offshore and historical currencies.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_HKMA_REWRITE_PARTY_RELATIONSHIP_TYPE',
    'partyRelationshipTypeScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/hkma-rewrite-party-relationship-type',
    'Indicates the relationship between two parties as defined by Hong Kong Monetary Authority (HKMA) Rewrite field 189 - Intragroup.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_PARTY_RELATIONSHIP_TYPE',
    'partyRelationshipTypeScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/party-relationship-type',
    'A type is containing a code representing how two parties are related, e.g. Affiliated, Intragroup.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_PARTY_ROLE',
    'partyRoleScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/party-role',
    'Contains a code representing a related party role. This can be extended to provide custom roles.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_PARTY_ROLE_TYPE',
    'partyRoleTypeScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/party-role-type',
    'Contains a code representing a related party role type. A type refining the role a role played by a party in one or more transactions. This can be extended to provide custom types.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_PERSON_ROLE',
    'personRoleScheme',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/person-role',
    'Indicates the role of a person in a transaction.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_HKMA_REWRITE_REGULATORY_CORPORATE_SECTOR',
    'regulatoryCorporateSectorScheme',
    'industry',
    'Regulatory',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/hkma-rewrite-regulatory-corporate-sector',
    'Defines the corporate sector under HKMA (Hong Kong Monetary Authority) Rewrite fields 190 - Nature of Counterparty 1 and 191 - Nature of Counterparty 2.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_REGULATORY_CORPORATE_SECTOR',
    'regulatoryCorporateSectorScheme',
    'industry',
    'Regulatory',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/regulatory-corporate-sector',
    'Specifies Corporate sector as defined by or for regulators including ESMA, CFTC, etc.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_REPORTING_REGIME',
    'reportingRegimeNameScheme',
    'industry',
    'Regulatory',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/reporting-regime',
    'Contains a code representing a reporting regime under which this transaction may be reported.'
);

select metadata.dq_coding_schemes_upsert_fn(
    'FPML_SUPERVISORY_BODY',
    'supervisoryBodyScheme',
    'industry',
    'Regulatory',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/supervisory-body',
    'Contains a code representing a supervisory-body that may be supervising this transaction.'
);
