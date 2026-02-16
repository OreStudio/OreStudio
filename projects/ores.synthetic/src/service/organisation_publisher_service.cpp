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
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"

#include "ores.refdata/repository/party_mapper.hpp"
#include "ores.refdata/repository/party_contact_information_mapper.hpp"
#include "ores.refdata/repository/party_identifier_mapper.hpp"
#include "ores.refdata/repository/counterparty_mapper.hpp"
#include "ores.refdata/repository/counterparty_contact_information_mapper.hpp"
#include "ores.refdata/repository/counterparty_identifier_mapper.hpp"
#include "ores.refdata/repository/party_counterparty_mapper.hpp"
#include "ores.refdata/repository/business_unit_mapper.hpp"
#include "ores.refdata/repository/portfolio_mapper.hpp"
#include "ores.refdata/repository/book_mapper.hpp"

namespace ores::synthetic::service {

using namespace ores::logging;
using namespace refdata::repository;

organisation_publisher_service::organisation_publisher_service(
    database::context ctx)
    : ctx_(std::move(ctx)) {}

void organisation_publisher_service::seed_currencies() {
    using database::service::tenant_context;
    auto sys_ctx = tenant_context::with_system_tenant(ctx_);

    struct currency_info {
        const char* iso_code;
        const char* name;
        const char* numeric_code;
        const char* symbol;
        const char* fraction_symbol;
        int fractions_per_unit;
        int rounding_precision;
        const char* format;
    };

    static constexpr std::array currencies = {
        currency_info{.iso_code = "GBP", .name = "Pound Sterling",
            .numeric_code = "826", .symbol = "£",
            .fraction_symbol = "p", .fractions_per_unit = 100,
            .rounding_precision = 2, .format = "£#,##0.00"},
        currency_info{.iso_code = "USD", .name = "US Dollar",
            .numeric_code = "840", .symbol = "$",
            .fraction_symbol = "¢", .fractions_per_unit = 100,
            .rounding_precision = 2, .format = "$#,##0.00"},
        currency_info{.iso_code = "EUR", .name = "Euro",
            .numeric_code = "978", .symbol = "€",
            .fraction_symbol = "c", .fractions_per_unit = 100,
            .rounding_precision = 2, .format = "€#,##0.00"},
        currency_info{.iso_code = "JPY", .name = "Yen",
            .numeric_code = "392", .symbol = "¥",
            .fraction_symbol = "", .fractions_per_unit = 1,
            .rounding_precision = 0, .format = "¥#,##0"},
    };

    BOOST_LOG_SEV(lg(), info) << "Seeding required currencies into "
                              << "system tenant";

    for (const auto& c : currencies) {
        const auto sql = std::string(
            "INSERT INTO ores_refdata_currencies_tbl "
            "(iso_code, tenant_id, version, name, numeric_code, symbol, "
            "fraction_symbol, fractions_per_unit, rounding_type, "
            "rounding_precision, format, currency_type, "
            "modified_by, performed_by, change_reason_code, "
            "change_commentary) "
            "VALUES ('") + c.iso_code + "', '"
            + tenant_context::system_tenant_id + "'::uuid, 0, '"
            + c.name + "', '" + c.numeric_code + "', '"
            + c.symbol + "', '" + c.fraction_symbol + "', "
            + std::to_string(c.fractions_per_unit) + ", 'Closest', "
            + std::to_string(c.rounding_precision) + ", '"
            + c.format + "', 'fiat.major', "
            "current_user, current_user, "
            "'system.external_data_import', "
            "'Seeded by synthetic organisation generator') "
            "ON CONFLICT DO NOTHING";
        database::repository::execute_raw_command(
            sys_ctx, sql, lg(),
            std::string("Seeding currency ") + c.iso_code);
    }
}

messaging::generate_organisation_response
organisation_publisher_service::publish(
    const domain::generated_organisation& org) {

    messaging::generate_organisation_response response;

    try {
        BOOST_LOG_SEV(lg(), info) << "Publishing generated organisation "
                                  << "(seed: " << org.seed << ")";

        // Ensure the system tenant has the currencies referenced by
        // portfolios and books. Without these, the portfolio insert
        // trigger will reject the aggregation_ccy values.
        seed_currencies();

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
        auto bus_units = business_unit_mapper::map(org.business_units);
        auto portfolios = portfolio_mapper::map(org.portfolios);
        auto books = book_mapper::map(org.books);

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
            .and_then(insert(bus_units))
            .and_then(insert(portfolios))
            .and_then(insert(books))
            .and_then(commit);
        database::repository::ensure_success(r, lg());

        BOOST_LOG_SEV(lg(), info) << "Wrote " << parties.size() << " parties, "
            << counterparties.size() << " counterparties, "
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
