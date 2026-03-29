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
#include "ores.iam.client/client/service_token_provider.hpp"

#include <chrono>
#include <memory>
#include <span>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"

namespace ores::iam::client {

namespace {

using namespace ores::logging;

inline static std::string_view logger_name =
    "ores.iam.client.client.service_token_provider";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

constexpr std::string_view service_login_subject = "iam.v1.auth.service-login";
constexpr std::string_view refresh_subject = "iam.v1.auth.refresh";

struct token_state {
    ores::nats::service::client& nats;
    std::string username;
    std::string password;
    std::chrono::seconds refresh_margin;
    std::string jwt;
    std::chrono::system_clock::time_point expires_at{};
    int access_lifetime_s = 1800;

    void authenticate() {
        BOOST_LOG_SEV(lg(), info) << "Authenticating service account: " << username;

        const auto req_json = rfl::json::write(ores::iam::messaging::service_login_request{
            .username = username, .password = password});
        const auto reply = nats.request_sync(
            service_login_subject, ores::nats::as_bytes(req_json));

        auto resp = rfl::json::read<ores::iam::messaging::service_login_response>(
            ores::nats::as_string_view(reply.data));
        if (!resp || !resp->success || resp->token.empty()) {
            const auto msg = resp ? resp->message : "parse error";
            BOOST_LOG_SEV(lg(), error)
                << "Service authentication failed for " << username << ": " << msg;
            throw std::runtime_error(
                "Service authentication failed for " + username + ": " + msg);
        }

        jwt = std::move(resp->token);
        access_lifetime_s = resp->access_lifetime_s;
        expires_at = std::chrono::system_clock::now() +
                     std::chrono::seconds(access_lifetime_s);
        // Tune proactive refresh margin to 20% of the actual TTL so the token
        // is renewed at ~80% of its lifetime, regardless of the configured TTL.
        refresh_margin = std::chrono::seconds(access_lifetime_s / 5);
        BOOST_LOG_SEV(lg(), info)
            << "Service authentication successful for " << username
            << " (lifetime=" << access_lifetime_s << "s"
            << ", refresh_margin=" << refresh_margin.count() << "s)";
    }

    void refresh() {
        BOOST_LOG_SEV(lg(), debug) << "Refreshing JWT for " << username;
        const std::unordered_map<std::string, std::string> headers{
            {"Authorization", "Bearer " + jwt}};
        const auto reply = nats.request_sync(
            refresh_subject, std::span<const std::byte>{}, headers);

        auto resp = rfl::json::read<ores::iam::messaging::refresh_response>(
            ores::nats::as_string_view(reply.data));
        if (!resp || !resp->success || resp->token.empty()) {
            BOOST_LOG_SEV(lg(), warn)
                << "Token refresh failed for " << username << ", re-authenticating";
            authenticate();
            return;
        }

        jwt = std::move(resp->token);
        access_lifetime_s = resp->access_lifetime_s;
        expires_at = std::chrono::system_clock::now() +
                     std::chrono::seconds(access_lifetime_s);
        BOOST_LOG_SEV(lg(), debug) << "JWT refreshed for " << username;
    }

    void refresh_if_needed() {
        if (std::chrono::system_clock::now() >= expires_at - refresh_margin)
            refresh();
    }
};

} // namespace

ores::nats::service::nats_client::token_provider
make_service_token_provider(ores::nats::service::client& nats,
    std::string username, std::string password,
    std::chrono::seconds refresh_margin) {

    auto state = std::make_shared<token_state>(token_state{
        .nats = nats,
        .username = std::move(username),
        .password = std::move(password),
        .refresh_margin = refresh_margin
    });

    // Authenticate eagerly so the first request does not incur a login
    // round-trip and startup failures are surfaced immediately.
    state->authenticate();

    return [state](bool force) -> std::string {
        if (force)
            state->authenticate();
        else
            state->refresh_if_needed();
        return state->jwt;
    };
}

} // namespace ores::iam::client
