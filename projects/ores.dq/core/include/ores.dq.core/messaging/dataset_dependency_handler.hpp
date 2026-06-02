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
#ifndef ORES_DQ_CORE_MESSAGING_DATASET_DEPENDENCY_HANDLER_HPP
#define ORES_DQ_CORE_MESSAGING_DATASET_DEPENDENCY_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include <vector>
#include <boost/uuid/string_generator.hpp>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.dq.api/messaging/dataset_dependency_protocol.hpp"
#include "ores.dq.core/service/data_organization_service.hpp"
#include "ores.dq.core/service/publication_service.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::dq::messaging {

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

namespace {
inline auto& dataset_dependency_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.dq.messaging.dataset_dependency_handler");
    return instance;
}
} // namespace

class dataset_dependency_handler {
public:
    dataset_dependency_handler(
        ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_dependency_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<get_dataset_dependencies_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto items = svc.list_dataset_dependencies();
            get_dataset_dependencies_response resp;
            resp.dependencies = items;
            resp.total_available_count = static_cast<int>(items.size());
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_dataset_dependencies_response resp;
            resp.total_available_count = 0;
            reply(nats_, msg, resp);
        }
    }

    void by_dataset(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_dependency_handler_lg(), debug) << "Handling " << msg.subject;
        auto req =
            decode<get_dataset_dependencies_by_dataset_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::data_organization_service svc(ctx);
        try {
            const auto deps =
                svc.list_dataset_dependencies_by_dataset(req->dataset_code);
            get_dataset_dependencies_by_dataset_response resp;
            resp.success = true;
            resp.dependencies = deps;
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), error) << msg.subject << " failed: " << e.what();
            get_dataset_dependencies_by_dataset_response resp;
            resp.success = false;
            resp.message = e.what();
            reply(nats_, msg, resp);
        }
    }

    void resolve(ores::nats::message msg) {
        BOOST_LOG_SEV(dataset_dependency_handler_lg(), debug) << "Handling " << msg.subject;
        auto req = decode<resolve_dependencies_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::publication_service svc(ctx);
        try {
            boost::uuids::string_generator gen;
            std::vector<boost::uuids::uuid> uuids;
            uuids.reserve(req->dataset_ids.size());
            for (const auto& id : req->dataset_ids)
                uuids.push_back(gen(id));
            const auto datasets = svc.resolve_publication_order(uuids);
            resolve_dependencies_response resp;
            resp.success = true;
            resp.datasets = datasets;
            resp.requested_ids = req->dataset_ids;
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(dataset_dependency_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resolve_dependencies_response resp;
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
