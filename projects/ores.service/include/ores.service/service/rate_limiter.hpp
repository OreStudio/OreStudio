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
#ifndef ORES_SERVICE_SERVICE_RATE_LIMITER_HPP
#define ORES_SERVICE_SERVICE_RATE_LIMITER_HPP

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <functional>
#include <mutex>
#include <string>
#include <unordered_map>

namespace ores::service::service {

/**
 * @brief Whether one request may proceed, and, when it may not, when to try.
 */
struct rate_limit_decision {
    bool allowed = true;
    std::chrono::milliseconds retry_after{0};
};

/**
 * @brief Limits how much one key may ask for in a period, in memory.
 *
 * A token bucket per key: a bucket holds at most @p burst tokens and refills
 * at @p limit tokens per @p period. A request takes one token; a request that
 * finds none is refused, and the decision carries the time until the next
 * token, so the caller backs off by exactly that much rather than guessing.
 * The caller adds the limit and the key to the refusal's message, as
 * [[id:3A3752D7-9322-45F0-9932-762B727642E1][Throttling]] asks. The wait is
 * what the caller then backs off by; the backoff itself stays in
 * ores.nats/service/retry.hpp, so this class counts use and nothing else.
 *
 * The clock is injected the way partition_token_cache injects its own, so a
 * test moves time rather than sleeping.
 *
 * The limiter is per process: several replicas each grant the limit, so the
 * effective limit multiplies by the replica count, the pattern's "a limit per
 * replica" pitfall. That is deliberate for the run grant exchange, where a
 * shared store would cost more than the fault it guards against.
 */
class rate_limiter {
public:
    using clock_fn = std::function<std::chrono::system_clock::time_point()>;

    /**
     * @param limit  Tokens added per @p period.
     * @param period The refill period.
     * @param burst  The most tokens one key may hold at once.
     * @param now    The clock; a test passes its own.
     */
    rate_limiter(int limit,
                 std::chrono::seconds period,
                 int burst,
                 clock_fn now = [] { return std::chrono::system_clock::now(); })
        : limit_(limit)
        , period_(period)
        , burst_(burst)
        , now_(std::move(now)) {}

    /**
     * @brief Takes one token for @p key, or refuses with the wait.
     *
     * A key seen for the first time starts with a full bucket, so a caller
     * that has been quiet may burst. A limit of nothing lets every request
     * through, which is how a deployment states that it sets no limit.
     */
    rate_limit_decision allow(const std::string& key) {
        if (limit_ <= 0 || burst_ <= 0)
            return {};
        std::lock_guard lock(mutex_);
        const auto now = now_();
        auto& slot = buckets_[key];
        if (!slot.seeded) {
            slot.tokens = burst_;
            slot.refreshed_at = now;
            slot.seeded = true;
        }
        const auto elapsed = std::chrono::duration<double>(now - slot.refreshed_at).count();
        if (elapsed > 0) {
            slot.tokens = std::min(burst_, slot.tokens + elapsed * rate());
            slot.refreshed_at = now;
        }
        if (slot.tokens >= 1.0) {
            slot.tokens -= 1.0;
            return {};
        }
        const auto wait_s = (1.0 - slot.tokens) / rate();
        return {.allowed = false,
                .retry_after = std::chrono::milliseconds(
                    static_cast<std::int64_t>(std::ceil(wait_s * 1000.0)))};
    }

    /**
     * @brief Drops @p key's bucket, so its next request starts full.
     */
    void forget(const std::string& key) {
        std::lock_guard lock(mutex_);
        buckets_.erase(key);
    }

    /**
     * @brief How many keys the limiter holds; for tests and diagnostics.
     */
    [[nodiscard]] std::size_t tracked() const {
        std::lock_guard lock(mutex_);
        return buckets_.size();
    }

private:
    /// Tokens added per second.
    [[nodiscard]] double rate() const {
        return static_cast<double>(limit_) / static_cast<double>(period_.count());
    }

    struct bucket {
        bool seeded = false;
        double tokens = 0.0;
        std::chrono::system_clock::time_point refreshed_at{};
    };

    int limit_;
    std::chrono::seconds period_;
    double burst_;
    clock_fn now_;
    mutable std::mutex mutex_;
    std::unordered_map<std::string, bucket> buckets_;
};

}

#endif
