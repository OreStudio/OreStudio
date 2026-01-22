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
 * Reference Data party_roles Population Script
 *
 * Populates the refdata_party_roles_tbl with reference data.
 * Source: party_roles_data.json
 *
 * This script is idempotent - uses INSERT with version handling.
 */

set schema 'ores';

-- =============================================================================
-- Reference Data party_roles
-- =============================================================================

\echo '--- Reference Data party_roles ---'

insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Accountant',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization responsible for preparing the accounting for the trade.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Accountant'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'AllocationAgent',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The organization responsible for supplying the allocations for a trade to be allocated to multiple accounts/organizations.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'AllocationAgent'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ArrangingBroker',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The organization that arranged the trade, i.e. brought together the counterparties. Synonyms/Alternatives: Inter-dealer broker, agent.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ArrangingBroker'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Beneficiary',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization that suffers the economic benefit of the trade. The beneficiary may be distinct from the principal/counterparty - an example occurs when a hedge fund trades via a prime broker; in this case the principal is the prime broker, but the beneficiary is the hedge fund. This can be represented as a payer/receiver account in the name of the hedge fund, but it is also possible to add the party role of "Beneficiary" at the partyTradeInformation level.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Beneficiary'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BookingParty',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The entity for which the organization supporting the trade''s processing has booked/recorded the trade. This is used in non-reporting workflows situations in which the trade doesn''t need to be reported but a firm still wants to specify their own side.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'BookingParty'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Buyer',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Acquirer of the legal title to the financial instrument. In the case of an option, the buyer is the holder of the option. In the case of a swap or forward, the buyer will be determined by industry best practice. This does not refer to an investor or investment manager or other organization on what is typically called the "Buy side"; for that, see the "Client" role. Corresponds to "Buyer" as defined in certain regulations such as ESMA MiFID II/MIFIR RTS 22 field 9.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Buyer'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'BuyerDecisionMaker',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The party or person who, having legal authority to act on behalf of the trade counterparty acting as Buyer as defined in this coding scheme, made the decision to acquire the financial instrument. Corresponds to "buyer decision maker" as defined in ESMA''s MIFIR RTS 23 report. This does not refer to the decision maker for what is traditionally called the "Buy side"; for that, see the "Client Decision Maker" role.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'BuyerDecisionMaker'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ClearingClient',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'An organization that clears trades through a clearing house, via a clearing broker (member of the clearing house) who acts as an agent on its behalf. The term "client" refers to the organization''s role in the clearing process in relation to its clearing broker, and not whether it is a price maker or taker in the execution process.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ClearingClient'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ClearingExceptionParty',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'A party to the trade that claims a clearing exception, such as an end-user exception under Dodd-Frank Act provisions.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ClearingExceptionParty'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ClearingFirm',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization that submits the trade to a clearing house on behalf of the principal. Synonyms/alternates: Futures Commission Merchant (FCM), Clearing Broker, Clearing Member Firm. Some implementations use "Clearing Broker" as synonym.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ClearingFirm'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ClearingOrganization',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The organization that acts as a central counterparty to clear a derivatives contract. This is used to represent the role of Central Counterparties (CCPs) or Derivative Clearing Organizations (DCOs). Sometimes called "ClearingService". Some implementations also use the term "Clearer".',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ClearingOrganization'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Client',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Client as defined under ESMA MIFIR. This is generally the investor or other client of an investment firm, and is synonymous with the Beneficiary in many circumstances.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Client'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ClientDecisionMaker',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The party or person who, having legal authority to act on behalf of a trade counterparty, made the decision to acquire or sell the financial instrument.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ClientDecisionMaker'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ConfirmationPlatform',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization serving as a financial intermediary for the purposes of electronic confirmation or providing services for post-processing of transactional data.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ConfirmationPlatform'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ContractualParty',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'A party to a contractual document. If the intended usage relates to the context of the trade lifecycle, more specific annotations have been defined which might be more appropriate.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ContractualParty'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Counterparty',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'An economic counterparty to the trade. Synonym: principal.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Counterparty'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CounterPartyAffiliate',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization offiially attached to the counterparty. e.g. partner, branch, subsidiary.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'CounterPartyAffiliate'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CounterPartyUltimateParent',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The topmost entity or organization, within the corporate hierarchy, responsible for the reporting party.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'CounterPartyUltimateParent'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'CreditSupportProvider',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization that enhances the credit of another organization (similar to guarantor, but may not fully guarantee the obligation).',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'CreditSupportProvider'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Custodian',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization that maintains custody of the asset represented by the trade on behalf of the owner/principal.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Custodian'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DataSubmitter',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Entity submitting the transaction report to the competent authority.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'DataSubmitter'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DisputingParty',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization that is disputing the trade or transaction.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'DisputingParty'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'DocumentRepository',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'A marketplace organization which purpose is to maintain document records. If the intended usage relates to the context of the trade lifecycle, more specific annotations have been defined which might be more appropriate.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'DocumentRepository'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ExecutingBroker',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The (generally sell-side) organization that executed the trade; the price-making party.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ExecutingBroker'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ExecutingEntity',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Entity executing the transaction. If the transaction is executed directly by the reporting party, it will be the reporting party. If it is executed by an execution agent or an affiliated party on behalf of the reporting party, it will be that affiliate or agent.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ExecutingEntity'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ExecutionAgent',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The (generally buy-side) organization that acts to execute trades on behalf of an investor. Typically this is an investment manager or asset manager, and also makes the investment decisions for the investor. If required, a separate InvestmentDecision role can be specified to distinguish that the party making the investment decision is different.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ExecutionAgent'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ExecutionFacility',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The facility, exchange, or market where the trade was executed. Synonym: Swap Execution Facility, Designated Contract Market, Execution Venue.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ExecutionFacility'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Guarantor',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization that backs (guarantees) the credit risk of the trade.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Guarantor'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'MarginAffiliate',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Margin affiliate as defined by U.S. margin and capital rules §23.151.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'MarginAffiliate'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'OrderTransmitter',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The entity transmitting the order to the reporting firm. Synonym: Transmitting Firm.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'OrderTransmitter'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PrimeBroker',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The organization that takes on or took on the credit risk for this trade by stepping in between the two economic parties (without a central counterparty clearing mechanism).',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'PrimeBroker'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PriorTradeRepository',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The trade repository at which the trade was reported previous to the current trade repository.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'PriorTradeRepository'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PTRRCompressionProvider',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'A party providing a post trade risk reduction service in the form of compression.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'PTRRCompressionProvider'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PTRRRebalancingProvider',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'A party providing a post trade risk reduction service in the form of portfolio rebalancing.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'PTRRRebalancingProvider'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'PublicationVenue',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The reporting service (whether trade repository, market data service, or exchange/facility/venue data distribution service) that published the report of this trade.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'PublicationVenue'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ReportingParty',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The party with the regulatory responsibility to report this trade.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ReportingParty'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ReportingPartyAffiliate',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'Organization offiially attached to the reporting party e.g. partner, branch, subsidiary.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ReportingPartyAffiliate'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'ReportingPartyUltimateParent',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The topmost entity or organization, within the corporate hierarchy, responsible for the reporting party.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'ReportingPartyUltimateParent'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'Seller',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'A counterparty in a trade, which performs in one of the following capacities: 1) it transfers or agrees to transfer in the future an instrument or title to that instrument in exchange for payment, 2) it writes a derivatives instrument such as an option or a swap in which it provides risk protection to the buyer. This does not refer to the broker/dealer or other organization on what is typically called the "Sell side"; for that, see the "Executing Broker" role. Corresponds to "Seller" as defined in certain regulations such as ESMA MiFID II/MIFIR RTS 22 field 16.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'Seller'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SellerDecisionMaker',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The party or person who, having legal authority to act on behalf of the trade counterparty acting as Seller as defined in this coding scheme, made the decision to sell the financial instrument. Corresponds to "seller decision maker" as defined in ESMA''s MIFIR RTS 23 report. This does not refer to the decision maker for what is traditionally called the "Sell side"; for that, see the "Trader" person role.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'SellerDecisionMaker'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'SettlementAgent',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The organization that makes or receives payments on behalf of the given principal party.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'SettlementAgent'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TradeRepository',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'An organization that maintains records of the trade for regulatory reporting purposes.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'TradeRepository'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TradeSource',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The organization that originally supplied the record of the trade. In the context of regulatory reporting, it is the submitter of the trade record to a regulator or TR.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'TradeSource'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TradingManager',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'The entity responsible for managing the assets/investments of this party. Synonnym: Asset Manager, Investment Manager, Trading Advisory.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'TradingManager'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);
insert into ores.refdata_party_roles_tbl (
    code, version, coding_scheme_code, source, description,
    modified_by, change_reason_code, change_commentary
)
select
    'TradingPartner',
    0,
    'FPML_PARTY_ROLE',
    'FpML',
    'An entity with which this party trades from time to time, ie. with which it acts as a counterparty on some transactions. This role is used for static reference data, not individual transactions.',
    'system',
    'INITIAL_LOAD',
    'FPML reference data import'
where not exists (
    select 1 from ores.refdata_party_roles_tbl
    where code = 'TradingPartner'
    and coding_scheme_code = 'FPML_PARTY_ROLE'
    and valid_to = ores.utility_infinity_timestamp_fn()
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'party_roles' as entity, count(*) as count
from ores.refdata_party_roles_tbl
where valid_to = ores.utility_infinity_timestamp_fn();

select coding_scheme_code, count(*) as count
from ores.refdata_party_roles_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
group by coding_scheme_code
order by coding_scheme_code;
