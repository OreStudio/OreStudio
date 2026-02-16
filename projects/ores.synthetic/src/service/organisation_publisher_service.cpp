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

#include "ores.refdata/repository/party_repository.hpp"
#include "ores.refdata/repository/party_contact_information_repository.hpp"
#include "ores.refdata/repository/party_identifier_repository.hpp"
#include "ores.refdata/repository/counterparty_repository.hpp"
#include "ores.refdata/repository/counterparty_contact_information_repository.hpp"
#include "ores.refdata/repository/counterparty_identifier_repository.hpp"
#include "ores.refdata/repository/party_counterparty_repository.hpp"
#include "ores.refdata/repository/business_unit_repository.hpp"
#include "ores.refdata/repository/portfolio_repository.hpp"
#include "ores.refdata/repository/book_repository.hpp"

namespace ores::synthetic::service {

using namespace ores::logging;

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

        // Write in FK order to satisfy foreign key constraints.
        refdata::repository::party_repository party_repo(ctx_);
        party_repo.write(org.parties);
        BOOST_LOG_SEV(lg(), info) << "Wrote " << org.parties.size()
                                  << " parties";

        if (!org.party_contacts.empty()) {
            refdata::repository::party_contact_information_repository pci_repo(ctx_);
            pci_repo.write(org.party_contacts);
            BOOST_LOG_SEV(lg(), info) << "Wrote " << org.party_contacts.size()
                                      << " party contacts";
        }

        if (!org.party_identifiers.empty()) {
            refdata::repository::party_identifier_repository pi_repo(ctx_);
            pi_repo.write(org.party_identifiers);
            BOOST_LOG_SEV(lg(), info) << "Wrote " << org.party_identifiers.size()
                                      << " party identifiers";
        }

        refdata::repository::counterparty_repository cp_repo(ctx_);
        cp_repo.write(org.counterparties);
        BOOST_LOG_SEV(lg(), info) << "Wrote " << org.counterparties.size()
                                  << " counterparties";

        if (!org.counterparty_contacts.empty()) {
            refdata::repository::counterparty_contact_information_repository cci_repo(ctx_);
            cci_repo.write(org.counterparty_contacts);
            BOOST_LOG_SEV(lg(), info) << "Wrote " << org.counterparty_contacts.size()
                                      << " counterparty contacts";
        }

        if (!org.counterparty_identifiers.empty()) {
            refdata::repository::counterparty_identifier_repository ci_repo(ctx_);
            ci_repo.write(org.counterparty_identifiers);
            BOOST_LOG_SEV(lg(), info) << "Wrote " << org.counterparty_identifiers.size()
                                      << " counterparty identifiers";
        }

        if (!org.party_counterparties.empty()) {
            refdata::repository::party_counterparty_repository pc_repo(ctx_);
            pc_repo.write(org.party_counterparties);
            BOOST_LOG_SEV(lg(), info) << "Wrote " << org.party_counterparties.size()
                                      << " party-counterparty links";
        }

        refdata::repository::business_unit_repository bu_repo(ctx_);
        bu_repo.write(org.business_units);
        BOOST_LOG_SEV(lg(), info) << "Wrote " << org.business_units.size()
                                  << " business units";

        refdata::repository::portfolio_repository portfolio_repo(ctx_);
        portfolio_repo.write(org.portfolios);
        BOOST_LOG_SEV(lg(), info) << "Wrote " << org.portfolios.size()
                                  << " portfolios";

        refdata::repository::book_repository book_repo(ctx_);
        book_repo.write(org.books);
        BOOST_LOG_SEV(lg(), info) << "Wrote " << org.books.size()
                                  << " books";

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
