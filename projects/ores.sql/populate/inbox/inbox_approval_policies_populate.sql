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
 * Approval Policies Population Script
 *
 * Seeds which part must approve each gated change, as the controls note
 * decides it. A policy row names an entity type, an operation and, when one
 * field decides the part, that field. The system tenant holds these rows. A
 * tenant may add parts, never remove them. This script runs after the parts and
 * is idempotent.
 */

\echo '--- Approval Policies ---'

insert into ores_inbox_approval_policies_tbl (
    tenant_id, code, version, name, description, entity_type, operation,
    field_name, part_code, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
) values
    (ores_utility_system_tenant_id_fn(), 'book.put.functional_currency', 0, 'Book functional currency',
     'A change needs the approval of finance', 'book', 'put', 'functional_currency', 'finance', 10,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.gl_account_ref', 0, 'Book gl account ref',
     'A change needs the approval of finance', 'book', 'put', 'gl_account_ref', 'finance', 20,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.cost_center', 0, 'Book cost center',
     'A change needs the approval of finance', 'book', 'put', 'cost_center', 'finance', 30,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.owner_unit_id', 0, 'Book owner unit id',
     'A change needs the approval of finance', 'book', 'put', 'owner_unit_id', 'finance', 40,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.regulatory_book_type', 0, 'Book regulatory book type',
     'A change needs the approval of market risk', 'book', 'put', 'regulatory_book_type', 'market_risk', 50,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.book_purpose_type', 0, 'Book book purpose type',
     'A change needs the approval of market risk', 'book', 'put', 'book_purpose_type', 'market_risk', 60,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.rates_centre_code', 0, 'Book rates centre code',
     'A change needs the approval of market risk', 'book', 'put', 'rates_centre_code', 'market_risk', 70,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.is_sweepable', 0, 'Book is sweepable',
     'A change needs the approval of market risk', 'book', 'put', 'is_sweepable', 'market_risk', 80,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.ledger_feed_type', 0, 'Book ledger feed type',
     'A change needs the approval of finance', 'book', 'put', 'ledger_feed_type', 'finance', 90,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.book_status', 0, 'Book book status',
     'A change needs the approval of operations', 'book', 'put', 'book_status', 'operations', 100,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'book.put.parent_portfolio_id', 0, 'Book parent portfolio id',
     'A change needs the approval of operations', 'book', 'put', 'parent_portfolio_id', 'operations', 130,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'portfolio.put', 0, 'Portfolio',
     'A change needs the approval of operations', 'portfolio', 'put', null, 'operations', 140,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'portfolio_right.put', 0, 'Portfolio right grant',
     'A change needs the approval of operations', 'portfolio_right', 'put', null, 'operations', 150,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'portfolio_right.delete', 0, 'Portfolio right removal',
     'A change needs the approval of operations', 'portfolio_right', 'delete', null, 'operations', 160,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'counterparty.put', 0, 'Counterparty',
     'A change needs the approval of operations', 'counterparty', 'put', null, 'operations', 170,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'counterparty_identifier.put', 0, 'Counterparty identifier',
     'A change needs the approval of operations', 'counterparty_identifier', 'put', null, 'operations', 180,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'counterparty_party_link.put', 0, 'Counterparty party link',
     'A change needs the approval of operations', 'counterparty_party_link', 'put', null, 'operations', 190,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'counterparty_party_link.delete', 0, 'Counterparty party link removal',
     'A change needs the approval of operations', 'counterparty_party_link', 'delete', null, 'operations', 200,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'netting_agreement.put', 0, 'Netting agreement',
     'A change needs the approval of market risk', 'netting_agreement', 'put', null, 'market_risk', 210,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'netting_set.put', 0, 'Netting set',
     'A change needs the approval of market risk', 'netting_set', 'put', null, 'market_risk', 220,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'collateral_term.put', 0, 'Collateral term',
     'A change needs the approval of market risk', 'collateral_term', 'put', null, 'market_risk', 230,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'party.put', 0, 'Party',
     'A change needs the approval of finance', 'party', 'put', null, 'finance', 240,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'party_identifier.put', 0, 'Party identifier',
     'A change needs the approval of finance', 'party_identifier', 'put', null, 'finance', 250,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'business_unit.put', 0, 'Business unit',
     'A change needs the approval of finance', 'business_unit', 'put', null, 'finance', 260,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'party_country.put', 0, 'Party country membership',
     'A change needs the approval of operations', 'party_country', 'put', null, 'operations', 270,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'party_country.delete', 0, 'Party country membership removal',
     'A change needs the approval of operations', 'party_country', 'delete', null, 'operations', 280,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'party_currency.put', 0, 'Party currency membership',
     'A change needs the approval of operations', 'party_currency', 'put', null, 'operations', 290,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'party_currency.delete', 0, 'Party currency membership removal',
     'A change needs the approval of operations', 'party_currency', 'delete', null, 'operations', 300,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'convention.put', 0, 'Convention',
     'A change needs the approval of market risk', 'convention', 'put', null, 'market_risk', 310,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies'),
    (ores_utility_system_tenant_id_fn(), 'tenor.put', 0, 'Tenor',
     'A change needs the approval of market risk', 'tenor', 'put', null, 'market_risk', 320,
     current_user, current_user, 'system.initial_load', 'Initial population of approval policies')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;
