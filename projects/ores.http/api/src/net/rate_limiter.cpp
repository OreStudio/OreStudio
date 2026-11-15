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
#include "ores.http.api/net/rate_limiter.hpp"
#include <algorithm>
#include <cassert>
#include <stdexcept>
#include <utility>

namespace ores::http::net {

namespace {

using std::chrono::duration_cast;
using std::chrono::nanoseconds;
using std::chrono::seconds;

/// Whole seconds, rounded up, so a caller told to wait never waits too little.
seconds ceil_seconds(nanoseconds d) {
    return d <= nanoseconds::zero() ? seconds(0) : std::chrono::ceil<seconds>(d);
}

rate_policy policy_of(std::string name, std::uint32_t per_minute, rate_key key, std::string what) {
    return rate_policy{std::move(name), per_minute, seconds(60), key, std::move(what)};
}

}

rate_policies rate_policies::from(const http_server_options& o) {
    rate_policies r;
    r.open_ = policy_of("open", o.open_requests_per_minute, rate_key::client, "requests");
    r.authenticated_ = policy_of(
        "authenticated", o.authenticated_requests_per_minute, rate_key::account, "requests");
    for (auto p : {policy_of(std::string(credential),
                             o.credential_attempts_per_minute,
                             rate_key::client,
                             "sign-in attempts"),
                   policy_of(std::string(registration_policy),
                             o.registration_policy_reads_per_minute,
                             rate_key::client,
                             "policy reads")})
        r.named_.emplace(p.name, std::move(p));
    std::vector<const rate_policy*> every{&r.open_, &r.authenticated_};
    for (const auto& [name, p] : r.named_)
        every.push_back(&p);
    for (const auto* p : every)
        if (p->limit == 0)
            throw std::invalid_argument("The " + p->name + " rate policy allows no requests.");
    return r;
}

const rate_policy& rate_policies::for_route(const std::optional<domain::route>& route) const {
    if (!route)
        return open_;
    if (!route->rate_policy.empty()) {
        const auto found = named_.find(route->rate_policy);
        // The server refuses to start on a name no policy holds.
        assert(found != named_.end());
        if (found != named_.end())
            return found->second;
    }
    return route->requires_auth ? authenticated_ : open_;
}

std::vector<std::string>
rate_policies::unknown_policies(const std::vector<domain::route>& routes) const {
    std::vector<std::string> out;
    for (const auto& r : routes)
        if (!r.rate_policy.empty() && !named_.contains(r.rate_policy))
            out.push_back(r.pattern + " names the rate policy " + r.rate_policy);
    return out;
}

rate_limiter::rate_limiter(std::size_t max_keys, clock now)
    : max_keys_per_shard_(std::max<std::size_t>(1, max_keys / shard_count))
    , now_(std::move(now)) {}

rate_decision rate_limiter::take(const rate_policy& policy, std::string_view caller) {
    /*
     * The generic cell rate algorithm. Each request spends one interval; the
     * bucket stores the time its spending is paid back. A request is allowed
     * while that time is no more than one window ahead, which lets the caller
     * spend the whole limit at once.
     */
    const auto window = duration_cast<nanoseconds>(policy.window);
    const auto interval = window / policy.limit;
    const auto now = now_();

    std::string key;
    key.reserve(policy.name.size() + 1 + caller.size());
    key.append(policy.name).push_back('\0');
    key.append(caller);

    auto& s = shards_[std::hash<std::string>{}(key) % shard_count];
    std::lock_guard lock(s.mutex);
    auto found = s.full_at.find(key);
    const auto full_at = found == s.full_at.end() ? now : std::max(found->second, now);
    const auto spent = full_at + interval;

    if (spent - now > window) {
        return rate_decision{false,
                             0,
                             ceil_seconds(full_at - now),
                             std::max(seconds(1), ceil_seconds(spent - window - now))};
    }

    if (found == s.full_at.end()) {
        if (s.full_at.size() >= max_keys_per_shard_ && sweep_shard(s, now) == 0) {
            const auto closest = std::ranges::min_element(
                s.full_at, {}, [](const auto& entry) { return entry.second; });
            s.full_at.erase(closest);
        }
        s.full_at.emplace(std::move(key), spent);
    } else {
        found->second = spent;
    }
    const auto left = (window - (spent - now)) / interval;
    return rate_decision{true,
                         static_cast<std::uint32_t>(std::max<std::int64_t>(0, left)),
                         ceil_seconds(spent - now),
                         seconds(0)};
}

std::size_t rate_limiter::sweep_shard(shard& s, time_point now) {
    return std::erase_if(s.full_at, [now](const auto& entry) { return entry.second <= now; });
}

std::size_t rate_limiter::sweep() {
    const auto now = now_();
    std::size_t dropped = 0;
    for (auto& s : shards_) {
        std::lock_guard lock(s.mutex);
        dropped += sweep_shard(s, now);
    }
    return dropped;
}

rate_limiting::rate_limiting(rate_policies p, trusted_proxies t, std::size_t max_keys)
    : policies(std::move(p))
    , trusted(std::move(t))
    , limiter(max_keys) {}

bool rate_limiting::may_warn() {
    const auto now =
        std::chrono::duration_cast<seconds>(std::chrono::steady_clock::now().time_since_epoch())
            .count();
    auto last = last_warned_.load(std::memory_order_relaxed);
    return now - last >= 10 &&
           last_warned_.compare_exchange_strong(last, now, std::memory_order_relaxed);
}

void state_rate_limit(domain::http_response& response,
                      const rate_policy& policy,
                      const rate_decision& decision) {
    response.set_header("RateLimit-Policy",
                        "\"" + policy.name + "\";q=" + std::to_string(policy.limit) +
                            ";w=" + std::to_string(policy.window.count()));
    response.set_header("RateLimit",
                        "\"" + policy.name + "\";r=" + std::to_string(decision.remaining) +
                            ";t=" + std::to_string(decision.reset.count()));
}

domain::http_response rate_limited(const rate_policy& policy, const rate_decision& decision) {
    auto response = domain::http_response::api_error(domain::http_status::too_many_requests,
                                                     "too-many-requests",
                                                     "Too many " + policy.what +
                                                         ". Wait a minute and try again.");
    response.set_header("Retry-After", std::to_string(decision.retry_after.count()));
    state_rate_limit(response, policy, decision);
    return response;
}

std::size_t rate_limiter::tracked() const {
    std::size_t n = 0;
    for (const auto& s : shards_) {
        std::lock_guard lock(s.mutex);
        n += s.full_at.size();
    }
    return n;
}

}
