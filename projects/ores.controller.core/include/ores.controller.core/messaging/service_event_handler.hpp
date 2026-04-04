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
#ifndef ORES_CONTROLLER_CORE_MESSAGING_SERVICE_EVENT_HANDLER_HPP
#define ORES_CONTROLLER_CORE_MESSAGING_SERVICE_EVENT_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.controller.api/messaging/service_event_protocol.hpp"
#include "ores.controller.core/repository/service_event_repository.hpp"

namespace ores::controller::messaging {

namespace {
inline auto& service_event_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.controller.messaging.service_event_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class service_event_handler {
public:
    service_event_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto cid =
            log_handler_entry(service_event_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        auto req = decode<api::messaging::list_service_events_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(service_event_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        repository::service_event_repository repo;
        api::messaging::list_service_events_response resp;
        try {
            const auto events = repo.read_latest(ctx_,
                req->service_name, req->limit);
            resp.service_events = events;
            resp.total_count = static_cast<std::uint64_t>(events.size());
            resp.success = true;
            BOOST_LOG_SEV(service_event_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(service_event_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::controller::messaging
#endif
