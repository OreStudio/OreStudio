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
#ifndef ORES_DQ_CORE_MESSAGING_DATASET_BUNDLE_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_DATASET_BUNDLE_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq.api/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq.core/service/dataset_bundle_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

namespace {
inline auto& dataset_bundle_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.dataset_bundle_handler");
    return instance;
}
} // namespace

class dataset_bundle_handler {
public:
    dataset_bundle_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_dataset_bundles_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_bundle_service svc(ctx);
        try {
            const auto items = svc.list_bundles();
            get_dataset_bundles_response resp;
            resp.bundles = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_dataset_bundles_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<save_dataset_bundle_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "dq::dataset_bundles:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::dataset_bundle_service svc(ctx);
        try {
            for (auto& b : req->bundles)
                stamp(b, ctx);
            svc.save_bundles(req->bundles);
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_dataset_bundle_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_dataset_bundle_response{false, e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<delete_dataset_bundle_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "dq::dataset_bundles:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::dataset_bundle_service svc(ctx);
        try {
            boost::uuids::string_generator gen;
            for (const auto& id : req->ids)
                svc.remove_bundle(gen(id));
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_dataset_bundle_response{true, {}});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                delete_dataset_bundle_response{false, e.what()});
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_dataset_bundle_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::dataset_bundle_service svc(ctx);
        try {
            const auto hist = svc.get_bundle_history(
                boost::uuids::string_generator{}(req->id));
            get_dataset_bundle_history_response resp;
            resp.success = true;
            resp.history = hist;
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_bundle_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_dataset_bundle_history_response resp;
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
