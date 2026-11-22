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
#ifndef ORES_HTTP_ROUTES_IAM_AGENT_KEY_EXCHANGE_HPP
#define ORES_HTTP_ROUTES_IAM_AGENT_KEY_EXCHANGE_HPP

#include "ores.http.api/domain/bearer_exchange.hpp"
#include "ores.logging/logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>

namespace ores::http::routes::iam {

/**
 * @brief The tokens the gateway holds for LLM agents' keys, in its own memory.
 *
 * An entry is keyed by the SHA-256 of the key, never the key, and is used
 * while its token has more than a fifth of its life left. The agent never sees
 * the token: it holds only its key. IAM publishes every change of an agent
 * grant or key, and the gateway clears the cache on each, so a revocation
 * reaches the agent's next call; a missed event is bounded by the token's
 * five-minute life. Nothing here is ever written to disk or a log.
 */
class agent_key_cache final {
public:
    struct entry {
        std::string token;
        std::chrono::system_clock::time_point issued_at;
        std::chrono::system_clock::time_point expires_at;
    };

    [[nodiscard]] std::optional<std::string> find(const std::string& key_hash,
                                                  std::chrono::system_clock::time_point now);

    void put(const std::string& key_hash, entry e);

    void clear();

private:
    std::mutex mutex_;
    std::unordered_map<std::string, entry> entries_;
};

/**
 * @brief Exchanges an LLM agent's key for a token IAM issues.
 *
 * The exchange is sent under the gateway's own identity, which alone holds
 * iam::agent_keys:exchange. A value that is not an agent key is left to the
 * gateway to verify as a token. Every refusal is answered alike and is never
 * cached.
 */
class agent_key_exchange final {
private:
    inline static std::string_view logger_name = "ores.http.routes.iam.agent_key_exchange";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief The exchanger the gateway's bearer path calls.
     */
    [[nodiscard]] static ores::http::domain::bearer_exchanger
    exchanger(ores::nats::service::nats_client& service, std::shared_ptr<agent_key_cache> cache);
};

}

#endif
