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
#ifndef ORES_HTTP_NET_RATE_LIMITER_HPP
#define ORES_HTTP_NET_RATE_LIMITER_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/route.hpp"
#include "ores.http.api/export.hpp"
#include "ores.http.api/net/client_address.hpp"
#include "ores.http.api/net/http_server_options.hpp"
#include <array>
#include <atomic>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <map>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

namespace ores::http::net {

/**
 * @brief Who a rate policy counts.
 *
 * A route that needs no session counts the client address, because there is
 * nobody else to count. A route that needs one counts the account, so many
 * people behind one address do not share a budget.
 */
enum class rate_key { client, account };

/**
 * @brief How many requests a caller may make, and how fast the allowance comes back.
 *
 * The caller may spend the whole limit at once, and one request's worth comes
 * back every window divided by the limit. Routes that name the same policy
 * share its budget.
 */
struct ORES_HTTP_API_EXPORT rate_policy final {
    /// The name the RateLimit headers state, and routes name the policy by.
    std::string name;
    /// The requests in one window, and the burst.
    std::uint32_t limit = 0;
    std::chrono::seconds window{60};
    rate_key key = rate_key::client;
    /// What the refusal says was too many, as in "Too many sign-in attempts".
    std::string what;
};

/**
 * @brief What the limiter answered for one request.
 */
struct ORES_HTTP_API_EXPORT rate_decision final {
    bool allowed = false;
    /// The requests the caller may still make at once.
    std::uint32_t remaining = 0;
    /// Seconds until the whole allowance is back.
    std::chrono::seconds reset{0};
    /// Seconds until a refused caller may try again; zero when allowed.
    std::chrono::seconds retry_after{0};
};

/**
 * @brief The policies the server runs routes under.
 *
 * Every route runs under one: the policy it names, or the default for its
 * authentication position. A name no policy holds is refused when the server
 * starts, so a typo cannot leave a route unlimited.
 */
class ORES_HTTP_API_EXPORT rate_policies final {
public:
    /// The policy names the server defines for routes to use.
    static constexpr std::string_view credential = "credential";
    static constexpr std::string_view registration_policy = "registration-policy";

    /**
     * @brief The defaults and the named policies, at the rates the options state.
     */
    [[nodiscard]] static rate_policies from(const http_server_options& options);

    /**
     * @brief The policy a request to this route runs under. A request no route
     * answers runs under the default for an open route.
     */
    [[nodiscard]] const rate_policy& for_route(const std::optional<domain::route>& route) const;

    /**
     * @brief The routes that name a policy this set does not hold.
     */
    [[nodiscard]] std::vector<std::string>
    unknown_policies(const std::vector<domain::route>& routes) const;

private:
    rate_policy open_;
    rate_policy authenticated_;
    std::map<std::string, rate_policy, std::less<>> named_;
};

/**
 * @brief Counts requests against policies, for every session of the server.
 *
 * Each policy and caller has one bucket, kept as the time it will be full
 * again: the generic cell rate algorithm, a token bucket with one number per
 * bucket. A request spends one interval of that time and is refused when the
 * bucket would hold less than nothing. A full bucket says nothing a fresh one
 * does not, so sweep() drops it.
 *
 * The buckets live in shards, one lock each, so sessions on different threads
 * rarely wait for each other. A shard that reaches its share of the key bound
 * sweeps itself, and drops the bucket closest to full when that is not enough.
 * That bucket is the least throttled one: a throttled caller's bucket is full
 * furthest in the future, so it is the last to go, and a flood of new callers
 * cannot reset it. Finding it scans the shard under its lock, which happens only
 * when the shard holds its whole share and none of it is full; the bound is
 * what keeps that scan short.
 *
 * Every method may be called from any thread.
 */
class ORES_HTTP_API_EXPORT rate_limiter final {
public:
    using clock = std::function<std::chrono::steady_clock::time_point()>;

    /**
     * @param max_keys The most buckets kept at once, across every shard.
     * @param now The clock, so a test can decide what time it is.
     */
    explicit rate_limiter(
        std::size_t max_keys, clock now = [] { return std::chrono::steady_clock::now(); });

    rate_limiter(const rate_limiter&) = delete;
    rate_limiter& operator=(const rate_limiter&) = delete;

    /**
     * @brief Count one request by this caller against this policy.
     */
    [[nodiscard]] rate_decision take(const rate_policy& policy, std::string_view caller);

    /**
     * @brief Drop every bucket that is full again, and say how many went.
     */
    std::size_t sweep();

    /**
     * @brief How many buckets are kept now.
     */
    [[nodiscard]] std::size_t tracked() const;

private:
    using time_point = std::chrono::steady_clock::time_point;

    static constexpr std::size_t shard_count = 16;

    struct shard final {
        mutable std::mutex mutex;
        /// The time each bucket is full again, by policy and caller.
        std::unordered_map<std::string, time_point> full_at;
    };

    static std::size_t sweep_shard(shard& s, time_point now);

    std::array<shard, shard_count> shards_;
    std::size_t max_keys_per_shard_;
    clock now_;
};

/**
 * @brief The policy a request ran under and what the limiter answered, so the
 * answer can state it.
 */
struct ORES_HTTP_API_EXPORT rate_note final {
    const rate_policy* policy = nullptr;
    rate_decision decision;
};

/**
 * @brief Everything the server limits requests with: the policies, the proxies
 * whose forwarded address it believes, and the buckets.
 */
class ORES_HTTP_API_EXPORT rate_limiting final {
public:
    rate_limiting(rate_policies policies, trusted_proxies trusted, std::size_t max_keys);

    rate_limiting(const rate_limiting&) = delete;
    rate_limiting& operator=(const rate_limiting&) = delete;

    const rate_policies policies;
    const trusted_proxies trusted;
    rate_limiter limiter;

    /**
     * @brief Whether a refusal may be logged as a warning now: at most one in
     * ten seconds, so a flood does not flood the journal. Every refusal is
     * still counted and logged at debug.
     */
    [[nodiscard]] bool may_warn();

private:
    /// Seconds on the steady clock; far in the past, so the first refusal warns.
    std::atomic<std::int64_t> last_warned_{std::numeric_limits<std::int32_t>::min()};
};

/**
 * @brief State a policy and a decision on an answer, in the RateLimit-Policy
 * and RateLimit headers of the IETF HTTP API rate limit headers draft.
 */
ORES_HTTP_API_EXPORT void state_rate_limit(domain::http_response& response,
                                           const rate_policy& policy,
                                           const rate_decision& decision);

/**
 * @brief The answer to a refused request: 429, too-many-requests in the words
 * the BFF used, Retry-After, and the RateLimit headers.
 */
[[nodiscard]] ORES_HTTP_API_EXPORT domain::http_response
rate_limited(const rate_policy& policy, const rate_decision& decision);

}

#endif
