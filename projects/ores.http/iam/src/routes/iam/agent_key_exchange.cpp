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
#include "ores.http/routes/iam/agent_key_exchange.hpp"
#include "ores.iam.api/domain/agent_key_format.hpp"
#include "ores.iam.api/messaging/agent_grant_operations_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <cstddef>
#include <exception>
#include <span>
#include <utility>

namespace ores::http::routes::iam {

std::optional<std::string> agent_key_cache::find(const std::string& key_hash,
                                                 std::chrono::system_clock::time_point now) {
    std::lock_guard lock(mutex_);
    const auto it = entries_.find(key_hash);
    if (it == entries_.end())
        return std::nullopt;
    const auto life = it->second.expires_at - it->second.issued_at;
    const auto left = it->second.expires_at - now;
    if (left * 5 <= life) {
        entries_.erase(it);
        return std::nullopt;
    }
    return it->second.token;
}

void agent_key_cache::put(const std::string& key_hash, entry e) {
    std::lock_guard lock(mutex_);
    entries_[key_hash] = std::move(e);
}

void agent_key_cache::clear() {
    std::lock_guard lock(mutex_);
    entries_.clear();
}

ores::http::domain::bearer_exchanger
agent_key_exchange::exchanger(ores::nats::service::nats_client& service,
                              std::shared_ptr<agent_key_cache> cache) {
    using ores::http::domain::bearer_exchange;
    // The client is the application's, which outlives the server that calls this.
    return [&service, cache = std::move(cache)](
               const std::string& bearer,
               const std::string& client_address) -> std::optional<bearer_exchange> {
        if (!ores::iam::domain::is_agent_key(bearer))
            return std::nullopt;

        const auto now = std::chrono::system_clock::now();
        const auto key_hash = ores::utility::crypto::sha256::hex_digest(bearer);
        if (auto token = cache->find(key_hash, now))
            return bearer_exchange{.result = bearer_exchange::outcome::exchanged,
                                   .token = std::move(*token)};

        using request_type = ores::iam::messaging::exchange_agent_key_request;
        try {
            request_type request;
            request.key = bearer;
            request.client_address = client_address;
            const auto wire = ores::nats::default_wire_codec().encode(request);
            const auto reply = service.authenticated_request(request_type::nats_subject,
                                                             std::span<const std::byte>(wire));
            if (reply.headers.contains(std::string(ores::nats::headers::x_error)))
                return bearer_exchange{};
            const auto decoded =
                ores::nats::default_wire_codec().decode<request_type::response_type>(reply.data);
            if (!decoded || !decoded->success || decoded->token.empty())
                return bearer_exchange{};
            const auto expires_at =
                std::chrono::system_clock::time_point(std::chrono::seconds(decoded->expires_at));
            cache->put(key_hash,
                       {.token = decoded->token, .issued_at = now, .expires_at = expires_at});
            return bearer_exchange{.result = bearer_exchange::outcome::exchanged,
                                   .token = decoded->token};
        } catch (const std::exception& e) {
            using namespace ores::logging;
            // The key is never logged; the reason is.
            ORES_LOG_SEV(lg(), warn) << "An agent key exchange failed: " << e.what();
            return bearer_exchange{};
        }
    };
}

}
