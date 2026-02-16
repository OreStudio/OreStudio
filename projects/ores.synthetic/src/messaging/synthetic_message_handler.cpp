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
#include "ores.synthetic/messaging/synthetic_message_handler.hpp"
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"
#include "ores.synthetic/service/organisation_generator_service.hpp"
#include "ores.synthetic/service/organisation_publisher_service.hpp"
#include "ores.iam/domain/permission.hpp"

namespace ores::synthetic::messaging {

using namespace ores::logging;
using ores::comms::messaging::message_type;

synthetic_message_handler::synthetic_message_handler(
    database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions,
    std::shared_ptr<iam::service::authorization_service> auth_service)
    : tenant_aware_handler(std::move(ctx), std::move(sessions)),
      auth_service_(std::move(auth_service)) {}

synthetic_message_handler::auth_check_result
synthetic_message_handler::check_authorization(
    const std::string& remote_address,
    std::string_view permission,
    std::string_view operation_name) {

    auto session_result = require_authentication(remote_address, operation_name);
    if (!session_result) {
        return session_result;
    }

    const auto& session = *session_result;
    if (!auth_service_->has_permission(session.account_id, permission)) {
        BOOST_LOG_SEV(lg(), warn) << operation_name
                                  << " denied: account "
                                  << session.account_id
                                  << " lacks permission "
                                  << permission;
        return std::unexpected(
            ores::utility::serialization::error_code::authorization_failed);
    }

    return session;
}

synthetic_message_handler::handler_result
synthetic_message_handler::handle_message(
    message_type type,
    std::span<const std::byte> payload,
    const std::string& remote_address) {

    switch (type) {
    case message_type::generate_organisation_request:
        co_return co_await handle_generate_organisation_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown synthetic message type: "
                                   << type;
        co_return std::unexpected(
            ores::utility::serialization::error_code::invalid_message_type);
    }
}

namespace {

/**
 * @brief Stamp tenant_id and modified_by on all entities in a generated
 * organisation.
 */
void stamp_audit_fields(domain::generated_organisation& org,
    const std::string& tenant_id, const std::string& modified_by) {
    for (auto& p : org.parties) {
        p.tenant_id = tenant_id;
        p.modified_by = modified_by;
    }
    for (auto& c : org.party_contacts) {
        c.tenant_id = tenant_id;
        c.modified_by = modified_by;
    }
    for (auto& i : org.party_identifiers) {
        i.tenant_id = tenant_id;
        i.modified_by = modified_by;
    }
    for (auto& c : org.counterparties) {
        c.tenant_id = tenant_id;
        c.modified_by = modified_by;
    }
    for (auto& c : org.counterparty_contacts) {
        c.tenant_id = tenant_id;
        c.modified_by = modified_by;
    }
    for (auto& i : org.counterparty_identifiers) {
        i.tenant_id = tenant_id;
        i.modified_by = modified_by;
    }
    for (auto& pc : org.party_counterparties) {
        pc.tenant_id = tenant_id;
        pc.modified_by = modified_by;
    }
    for (auto& bu : org.business_units) {
        bu.tenant_id = tenant_id;
        bu.modified_by = modified_by;
    }
    for (auto& p : org.portfolios) {
        p.tenant_id = tenant_id;
        p.modified_by = modified_by;
    }
    for (auto& b : org.books) {
        b.tenant_id = tenant_id;
        b.modified_by = modified_by;
    }
}

}

synthetic_message_handler::handler_result
synthetic_message_handler::handle_generate_organisation_request(
    std::span<const std::byte> payload,
    const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing generate_organisation_request from "
                               << remote_address;

    auto auth_result = check_authorization(remote_address,
        iam::domain::permissions::dataset_bundles_write,
        "Generate organisation");
    if (!auth_result) {
        co_return std::unexpected(auth_result.error());
    }

    auto request_result = generate_organisation_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to deserialize generate_organisation_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), info) << "Generating organisation: country="
                              << request.country
                              << ", parties=" << request.party_count
                              << ", counterparties=" << request.counterparty_count;

    auto ctx = make_request_context(*auth_result);
    const auto tenant_id_str = auth_result->tenant_id.to_string();
    const auto& modified_by = auth_result->username;

    // Build generation options from request
    domain::organisation_generation_options options;
    options.country = request.country;
    options.party_count = request.party_count;
    options.party_max_depth = request.party_max_depth;
    options.counterparty_count = request.counterparty_count;
    options.counterparty_max_depth = request.counterparty_max_depth;
    options.portfolio_leaf_count = request.portfolio_leaf_count;
    options.portfolio_max_depth = request.portfolio_max_depth;
    options.books_per_leaf_portfolio = request.books_per_leaf_portfolio;
    options.business_unit_count = request.business_unit_count;
    options.business_unit_max_depth = request.business_unit_max_depth;
    options.generate_addresses = request.generate_addresses;
    options.generate_identifiers = request.generate_identifiers;
    options.contacts_per_party = request.contacts_per_party;
    options.contacts_per_counterparty = request.contacts_per_counterparty;
    options.seed = request.seed;

    generate_organisation_response response;
    try {
        // Generate the organisation
        service::organisation_generator_service generator;
        auto org = generator.generate(options);

        // Stamp correct tenant_id and modified_by from session
        stamp_audit_fields(org, tenant_id_str, modified_by);

        // Persist to database
        service::organisation_publisher_service publisher(ctx);
        response = publisher.publish(org);

        BOOST_LOG_SEV(lg(), info) << "Organisation generation complete: "
                                  << response.parties_count << " parties, "
                                  << response.counterparties_count << " counterparties, "
                                  << response.portfolios_count << " portfolios, "
                                  << response.books_count << " books, "
                                  << response.business_units_count << " business units";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Organisation generation failed: " << e.what();
        response.success = false;
        response.error_message = e.what();
    }

    co_return response.serialize();
}

}
