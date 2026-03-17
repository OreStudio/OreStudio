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
#ifndef ORES_DQ_MESSAGING_DATASET_BUNDLE_MEMBER_HANDLER_HPP
#define ORES_DQ_MESSAGING_DATASET_BUNDLE_MEMBER_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq/messaging/dataset_bundle_member_protocol.hpp"
#include "ores.dq/service/dataset_bundle_member_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using namespace ores::logging;

namespace {
inline auto& dataset_bundle_member_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.dataset_bundle_member_handler");
    return instance;
}
} // namespace

class dataset_bundle_member_handler {
public:
    dataset_bundle_member_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_dataset_bundle_members_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dataset_bundle_member_service svc(ctx);
        try {
            const auto items = svc.list_members();
            get_dataset_bundle_members_response resp;
            resp.members = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_dataset_bundle_members_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void by_bundle(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Handling " << msg.subject;
        auto req =
            decode<get_dataset_bundle_members_by_bundle_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::dataset_bundle_member_service svc(ctx);
        try {
            const auto members =
                svc.list_members_by_bundle(req->bundle_code);
            get_dataset_bundle_members_by_bundle_response resp;
            resp.success = true;
            resp.members = members;
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_dataset_bundle_members_by_bundle_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

private:

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
