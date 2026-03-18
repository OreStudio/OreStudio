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
#ifndef ORES_SYNTHETIC_MESSAGING_ORGANISATION_HANDLER_HPP
#define ORES_SYNTHETIC_MESSAGING_ORGANISATION_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic/domain/organisation_generation_options.hpp"
#include "ores.synthetic/messaging/generate_organisation_protocol.hpp"
#include "ores.synthetic/service/organisation_generator_service.hpp"
#include "ores.synthetic/service/organisation_publisher_service.hpp"

namespace ores::synthetic::messaging {

namespace {
inline auto& organisation_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.synthetic.messaging.organisation_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using namespace ores::logging;

class organisation_handler {
public:
    organisation_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void generate(ores::nats::message msg) {
        BOOST_LOG_SEV(organisation_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<generate_organisation_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(organisation_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        try {
            domain::organisation_generation_options opts;
            opts.country = req->country;
            opts.party_count = req->party_count;
            opts.counterparty_count = req->counterparty_count;
            opts.portfolio_leaf_count = req->portfolio_leaf_count;
            opts.books_per_leaf_portfolio = req->books_per_leaf_portfolio;
            opts.business_unit_count = req->business_unit_count;
            opts.generate_addresses = req->generate_addresses;
            opts.generate_identifiers = req->generate_identifiers;

            service::organisation_generator_service gen;
            const auto org = gen.generate(opts);

            service::organisation_publisher_service pub(ctx);
            const auto result = pub.publish(org);

            generate_organisation_response resp;
            resp.success = result.success;
            resp.error_message = result.error_message;
            resp.parties_count = static_cast<int>(result.parties_count);
            resp.counterparties_count =
                static_cast<int>(result.counterparties_count);
            resp.business_unit_types_count =
                static_cast<int>(result.business_unit_types_count);
            resp.business_units_count =
                static_cast<int>(result.business_units_count);
            resp.portfolios_count = static_cast<int>(result.portfolios_count);
            resp.books_count = static_cast<int>(result.books_count);
            resp.contacts_count = static_cast<int>(result.contacts_count);
            resp.identifiers_count =
                static_cast<int>(result.identifiers_count);

            BOOST_LOG_SEV(organisation_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(organisation_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, generate_organisation_response{
                .success = false, .error_message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::synthetic::messaging

#endif
