/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#include "ores.synthetic/service/organisation_publisher_service.hpp"
#include <algorithm>
#include <unordered_set>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

#include "ores.refdata/repository/party_mapper.hpp"
#include "ores.refdata/repository/party_contact_information_mapper.hpp"
#include "ores.refdata/repository/party_identifier_mapper.hpp"
#include "ores.refdata/repository/counterparty_mapper.hpp"
#include "ores.refdata/repository/counterparty_contact_information_mapper.hpp"
#include "ores.refdata/repository/counterparty_identifier_mapper.hpp"
#include "ores.refdata/repository/party_counterparty_mapper.hpp"
#include "ores.refdata/repository/business_unit_type_mapper.hpp"
#include "ores.refdata/repository/business_unit_mapper.hpp"
#include "ores.refdata/repository/portfolio_mapper.hpp"
#include "ores.refdata/repository/book_mapper.hpp"

namespace ores::synthetic::service {

using namespace ores::logging;
using namespace refdata::repository;

organisation_publisher_service::organisation_publisher_service(
    database::context ctx)
    : ctx_(std::move(ctx)) {}

messaging::generate_organisation_response
organisation_publisher_service::publish(
    const domain::generated_organisation& org) {

    messaging::generate_organisation_response response;

    try {
        BOOST_LOG_SEV(lg(), info) << "Publishing generated organisation "
                                  << "(seed: " << org.seed << ")";

        // Map all domain objects to entities before starting the transaction.
        auto parties = party_mapper::map(org.parties);
        auto party_contacts =
            party_contact_information_mapper::map(org.party_contacts);
        auto party_ids = party_identifier_mapper::map(org.party_identifiers);
        auto counterparties = counterparty_mapper::map(org.counterparties);
        auto cp_contacts =
            counterparty_contact_information_mapper::map(org.counterparty_contacts);
        auto cp_ids =
            counterparty_identifier_mapper::map(org.counterparty_identifiers);
        auto party_cps =
            party_counterparty_mapper::map(org.party_counterparties);
        auto bu_types = business_unit_type_mapper::map(org.business_unit_types);
        auto bus_units = business_unit_mapper::map(org.business_units);
        auto portfolios = portfolio_mapper::map(org.portfolios);
        auto books = book_mapper::map(org.books);

        // Remove BU types that already exist for this tenant (idempotent seed).
        // Multiple publish calls in the same tenant share canonical type codes
        // (DIVISION, BRANCH, etc.) under the same coding scheme; re-inserting
        // them would violate the natural-key unique index.
        if (!bu_types.empty()) {
            const auto& tid = bu_types[0].tenant_id;
            const auto sql =
                "SELECT code FROM ores_refdata_business_unit_types_tbl "
                "WHERE tenant_id = '" + tid + "'::uuid "
                "AND valid_to = ores_utility_infinity_timestamp_fn()";
            const auto existing = database::repository::execute_raw_string_query(
                ctx_, sql, lg(), "checking existing BU types");
            const std::unordered_set<std::string> existing_set(
                existing.begin(), existing.end());
            bu_types.erase(
                std::remove_if(bu_types.begin(), bu_types.end(),
                    [&](const auto& t) { return existing_set.count(t.code) > 0; }),
                bu_types.end());
            BOOST_LOG_SEV(lg(), debug) << "BU types to insert after dedup: "
                                       << bu_types.size();
        }

        BOOST_LOG_SEV(lg(), info) << "Mapped all entities, inserting in a "
                                  << "single transaction";

        // Insert all entities atomically in FK order within a single
        // transaction. If any insert fails, the entire transaction rolls back.
        using namespace sqlgen;
        const auto r = session(ctx_.connection_pool())
            .and_then(begin_transaction)
            .and_then(insert(parties))
            .and_then(insert(party_contacts))
            .and_then(insert(party_ids))
            .and_then(insert(counterparties))
            .and_then(insert(cp_contacts))
            .and_then(insert(cp_ids))
            .and_then(insert(party_cps))
            .and_then(insert(bu_types))
            .and_then(insert(bus_units))
            .and_then(insert(portfolios))
            .and_then(insert(books))
            .and_then(commit);
        database::repository::ensure_success(r, lg());

        BOOST_LOG_SEV(lg(), info) << "Wrote " << parties.size() << " parties, "
            << counterparties.size() << " counterparties, "
            << bu_types.size() << " business unit types, "
            << bus_units.size() << " business units, "
            << portfolios.size() << " portfolios, "
            << books.size() << " books";

        response.success = true;
        response.parties_count =
            static_cast<std::uint32_t>(org.parties.size());
        response.counterparties_count =
            static_cast<std::uint32_t>(org.counterparties.size());
        response.portfolios_count =
            static_cast<std::uint32_t>(org.portfolios.size());
        response.books_count =
            static_cast<std::uint32_t>(org.books.size());
        response.business_unit_types_count =
            static_cast<std::uint32_t>(org.business_unit_types.size());
        response.business_units_count =
            static_cast<std::uint32_t>(org.business_units.size());
        response.contacts_count =
            static_cast<std::uint32_t>(
                org.party_contacts.size() + org.counterparty_contacts.size());
        response.identifiers_count =
            static_cast<std::uint32_t>(
                org.party_identifiers.size() + org.counterparty_identifiers.size());

        BOOST_LOG_SEV(lg(), info) << "Organisation publication complete";

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Organisation publication failed: "
                                   << e.what();
        response.success = false;
        response.error_message = e.what();
    }

    return response;
}

}
