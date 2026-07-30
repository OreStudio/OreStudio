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
#ifndef ORES_DQ_CORE_MESSAGING_DATASET_BUNDLE_MEMBER_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_DATASET_BUNDLE_MEMBER_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/messaging/dataset_bundle_member_protocol.hpp"
#include "ores.dq.core/service/dataset_bundle_member_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::dq::messaging {

namespace {
inline auto& dataset_bundle_member_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.dq.messaging.dataset_bundle_member_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

/**
 * @brief NATS message handler for dataset bundle members operations.
 */
class dataset_bundle_member_handler {
public:
    dataset_bundle_member_handler(ores::nats::service::client& nats,
                                  ores::database::context ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list_by_bundle(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::dataset_bundle_member_service svc(req_ctx);
        if (auto req = decode<get_dataset_bundle_members_by_bundle_request>(msg)) {
            get_dataset_bundle_members_by_bundle_response resp;
            try {
                resp.dataset_bundle_members =
                    svc.list_members_by_bundle(req->bundle_code, req->offset, req->limit);
                resp.total_available_count =
                    static_cast<int>(svc.get_total_member_count_by_bundle(req->bundle_code));
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        service::dataset_bundle_member_service svc(req_ctx);
        if (auto req = decode<get_dataset_bundle_members_request>(msg)) {
            get_dataset_bundle_members_response resp;
            try {
                resp.dataset_bundle_members = svc.list_members();
                resp.total_available_count = static_cast<int>(resp.dataset_bundle_members.size());
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
            }
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(dataset_bundle_member_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::dq::messaging

#endif
