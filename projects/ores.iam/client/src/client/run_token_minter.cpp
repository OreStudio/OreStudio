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
#include "ores.iam.client/client/run_token_minter.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include <chrono>
#include <exception>
#include <optional>
#include <string>

namespace ores::iam::client {

using namespace ores::logging;
using ores::service::service::cache::run_token;
using ores::service::service::cache::run_token_key;
using ores::service::service::cache::run_token_minter;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger("ores.iam.client.run_token_minter");
    return instance;
}

}

run_token_minter make_run_token_minter(ores::nats::service::nats_client& nats) {
    return
        [&nats](const run_token_key& key, std::string_view tenant_id) -> std::optional<run_token> {
            messaging::exchange_run_grant_request request;
            request.grant_id = key.grant_id;
            request.run_id = key.run_id;
            request.tenant_id = std::string(tenant_id);
            const auto& codec = ores::nats::default_wire_codec();
            try {
                const auto reply = nats.authenticated_request(
                    messaging::exchange_run_grant_request::nats_subject, codec.encode(request));
                auto response = codec.decode<messaging::exchange_run_grant_response>(reply.data);
                if (!response || !response->success || response->token.empty()) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "No run token for grant " << key.grant_id << " run " << key.run_id
                        << ": " << (response ? response->message : "the answer could not be read");
                    return std::nullopt;
                }
                return run_token{.token = response->token,
                                 .expires_at = std::chrono::system_clock::time_point{} +
                                               std::chrono::seconds(response->expires_at)};
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "No run token for grant " << key.grant_id << " run "
                                          << key.run_id << ": the exchange failed: " << e.what();
                return std::nullopt;
            }
        };
}

}
