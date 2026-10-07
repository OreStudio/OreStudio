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
#ifndef ORES_EVENTING_CORE_SERVICE_CACHE_PARTITION_TOKEN_CACHE_HPP
#define ORES_EVENTING_CORE_SERVICE_CACHE_PARTITION_TOKEN_CACHE_HPP

#include <chrono>
#include <functional>
#include <mutex>
#include <string>
#include <unordered_map>
#include <utility>

namespace ores::eventing::service::cache {

/**
 * @brief The token a cache partition loads with, and when it stops working.
 */
struct partition_token {
    std::string token;
    std::chrono::system_clock::time_point expires_at;
};

/**
 * @brief Obtains a token that acts inside one partition's tenant, by Token
 * Exchange. An empty token means the exchange was refused.
 */
using partition_token_minter = std::function<partition_token(const std::string& partition)>;

/**
 * @brief Gives the token a partition loads with. @p renew asks for a new one,
 * because the target answered @c token_expired to the one held.
 */
using partition_token_provider =
    std::function<std::string(const std::string& partition, bool renew)>;

/**
 * @brief One token per partition of an entity-mirror cache, each acting
 * inside its own tenant.
 *
 * A partition is one tenant's copy of another service's rows, so it is read
 * as that tenant: row-level security then returns exactly what the tenant
 * may see, and no policy has to let a service read every tenant. The tokens
 * come from Token Exchange behind this cache: a token is reused until it is
 * within @c margin of its expiry, or until a target refuses it as expired,
 * so loading a partition does not mean one exchange per request. Concurrent
 * misses share one exchange, because the exchange runs under the lock.
 */
class partition_token_cache {
public:
    using clock_fn = std::function<std::chrono::system_clock::time_point()>;

    explicit partition_token_cache(
        partition_token_minter mint,
        std::chrono::seconds margin = std::chrono::seconds(30),
        clock_fn now = [] { return std::chrono::system_clock::now(); })
        : mint_(std::move(mint))
        , margin_(margin)
        , now_(std::move(now)) {}

    /**
     * @brief The token for @p partition: the one held while it is fresh, a
     * new one when it is near expiry or when @p renew is set. A refused
     * exchange returns an empty token and holds nothing.
     */
    std::string get(const std::string& partition, bool renew = false) {
        std::lock_guard lock(mutex_);
        const auto held = tokens_.find(partition);
        if (!renew && held != tokens_.end() && now_() + margin_ < held->second.expires_at)
            return held->second.token;
        auto minted = mint_(partition);
        if (minted.token.empty()) {
            tokens_.erase(partition);
            return {};
        }
        auto& slot = tokens_[partition];
        slot = std::move(minted);
        return slot.token;
    }

    /**
     * @brief This cache as the provider a generated entity cache takes. The
     * cache must outlive every provider it hands out.
     */
    partition_token_provider provider() {
        return [this](const std::string& partition, bool renew) {
            return get(partition, renew);
        };
    }

private:
    partition_token_minter mint_;
    std::chrono::seconds margin_;
    clock_fn now_;
    std::mutex mutex_;
    std::unordered_map<std::string, partition_token> tokens_;
};

}

#endif
