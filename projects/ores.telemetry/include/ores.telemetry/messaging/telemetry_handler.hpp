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
#ifndef ORES_TELEMETRY_MESSAGING_TELEMETRY_HANDLER_HPP
#define ORES_TELEMETRY_MESSAGING_TELEMETRY_HANDLER_HPP

#include <chrono>
#include <optional>
#include <stdexcept>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.telemetry/messaging/telemetry_protocol.hpp"
#include "ores.telemetry/messaging/nats_samples_protocol.hpp"
#include "ores.telemetry/messaging/service_samples_protocol.hpp"
#include "ores.telemetry.database/repository/telemetry_repository.hpp"

namespace ores::telemetry::messaging {

namespace {
inline auto& telemetry_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.telemetry.messaging.telemetry_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class telemetry_handler {
public:
    telemetry_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void logs_list(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        // No telemetry query service implemented yet; return empty.
        if (decode<get_telemetry_logs_request>(msg)) {
            BOOST_LOG_SEV(telemetry_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_telemetry_logs_response{.success = true});
        } else {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void nats_server_samples_list(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (decode<get_nats_server_samples_request>(msg)) {
            BOOST_LOG_SEV(telemetry_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                get_nats_server_samples_response{.success = true});
        } else {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    /**
     * @brief Receives a fire-and-forget service heartbeat and stores it.
     *
     * The message is a plain publish (no reply-to). We decode the payload,
     * stamp sampled_at with the current time, and persist via the repository.
     */
    void service_heartbeat(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto hb = decode<service_heartbeat_message>(msg);
        if (!hb) {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        domain::service_sample sample;
        sample.sampled_at = std::chrono::system_clock::now();
        sample.service_name = hb->service_name;
        sample.instance_id = hb->instance_id;
        sample.version = hb->version;

        database::repository::telemetry_repository repo;
        repo.insert_service_sample(ctx_, sample);
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Stored heartbeat: " << sample.service_name
            << " instance=" << sample.instance_id;
    }

    void service_samples_list(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (decode<get_service_samples_request>(msg)) {
            database::repository::telemetry_repository repo;
            get_service_samples_response resp;
            try {
                resp.samples = repo.list_service_samples(ctx);
                resp.success = true;
            } catch (const std::exception& e) {
                resp.success = false;
                resp.message = e.what();
            }
            reply(nats_, msg, resp);
        } else {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void nats_stream_samples_list(ores::nats::message msg) {
        BOOST_LOG_SEV(telemetry_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (decode<get_nats_stream_samples_request>(msg)) {
            BOOST_LOG_SEV(telemetry_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                get_nats_stream_samples_response{.success = true});
        } else {
            BOOST_LOG_SEV(telemetry_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::telemetry::messaging

#endif
