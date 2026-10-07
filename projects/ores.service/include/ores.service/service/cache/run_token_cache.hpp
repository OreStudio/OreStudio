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
#ifndef ORES_SERVICE_SERVICE_CACHE_RUN_TOKEN_CACHE_HPP
#define ORES_SERVICE_SERVICE_CACHE_RUN_TOKEN_CACHE_HPP

#include <chrono>
#include <cstddef>
#include <functional>
#include <list>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

namespace ores::service::service::cache {

/**
 * @brief The pair a run token is held under.
 *
 * Two runs under one grant use two tokens, so the =run_id= claim stays exact,
 * and a token refreshed mid-run replaces the same entry rather than adding one.
 */
struct run_token_key {
    std::string grant_id;
    std::string run_id;

    friend bool operator==(const run_token_key&, const run_token_key&) = default;
};

struct run_token_key_hash {
    std::size_t operator()(const run_token_key& key) const {
        const auto first = std::hash<std::string>{}(key.grant_id);
        const auto second = std::hash<std::string>{}(key.run_id);
        return first ^ (second + 0x9e3779b97f4a7c15ULL + (first << 6) + (first >> 2));
    }
};

/**
 * @brief A held run token, and when it stops working.
 */
struct run_token {
    std::string token;
    std::chrono::system_clock::time_point expires_at;
};

/**
 * @brief Exchanges a grant for a run token in @p tenant_id. An empty answer
 * means the exchange was refused, and the cache holds nothing for the key.
 *
 * The tenant travels with the call rather than the key, because a grant id is
 * unique across tenants and only the exchange needs it.
 */
using run_token_minter =
    std::function<std::optional<run_token>(const run_token_key& key, std::string_view tenant_id)>;

/**
 * @brief Holds one run token per grant and run, in memory, in one process.
 *
 * A step service keeps one of these. A token is used while more than @p margin
 * of its life remains, and exchanged again after that, so a run of hours lives
 * through many tokens without a caller tracking the clock. A miss mints under
 * the lock, so concurrent misses for one key share one exchange.
 *
 * The cache is bounded. It holds at most @p capacity entries and drops the
 * least recently used when full, so a missed end-of-run event costs memory for
 * at most that many tokens. It is never written to disk, a database, a message
 * or a log, and a restart starts empty.
 *
 * A caller whose request an owner refused as =token_expired= calls
 * @ref invalidate, then @ref token_for, then repeats the request once. The
 * repeat stays with the caller, because only it knows how to make the request
 * again.
 *
 * @see [[id:78B82824-0BBB-4442-A9FA-E1F265DF57BD][Run grants and run tokens]]
 */
class run_token_cache {
public:
    using clock_fn = std::function<std::chrono::system_clock::time_point()>;

    /**
     * @param mint     Exchanges a grant for a token.
     * @param capacity The most entries the cache holds.
     * @param margin   How much life a token must have left to be used.
     * @param now      The clock; a test passes its own.
     */
    explicit run_token_cache(
        run_token_minter mint,
        std::size_t capacity = 1024,
        std::chrono::milliseconds margin = std::chrono::seconds(60),
        clock_fn now = [] { return std::chrono::system_clock::now(); })
        : mint_(std::move(mint))
        , capacity_(capacity)
        , margin_(margin)
        , now_(std::move(now)) {}

    /**
     * @brief The run token for @p key, minted when the held one is close to
     * expiry or when none is held. Empty when the exchange is refused.
     *
     * @param tenant_id The tenant the run acts in, for the exchange on a miss.
     */
    std::string token_for(const run_token_key& key, std::string_view tenant_id) {
        std::lock_guard lock(mutex_);
        const auto held = entries_.find(key);
        if (held != entries_.end() && now_() + margin_ < held->second.value.expires_at) {
            touch(held);
            return held->second.value.token;
        }
        auto minted = mint_(key, tenant_id);
        if (!minted) {
            erase(key);
            return {};
        }
        insert(key, std::move(*minted));
        return entries_.at(key).value.token;
    }

    /**
     * @brief Drops @p key, so the next call exchanges again. A caller that read
     * =token_expired= calls this before it repeats the request once.
     */
    void invalidate(const run_token_key& key) {
        std::lock_guard lock(mutex_);
        erase(key);
    }

    /**
     * @brief Drops every entry of @p run_id, at the end of a step or the end of
     * a run.
     */
    void evict_run(const std::string& run_id) {
        std::lock_guard lock(mutex_);
        for (auto it = entries_.begin(); it != entries_.end();) {
            if (it->first.run_id == run_id)
                it = erase(it);
            else
                ++it;
        }
    }

    /// How many entries the cache holds; for tests and diagnostics.
    [[nodiscard]] std::size_t size() const {
        std::lock_guard lock(mutex_);
        return entries_.size();
    }

private:
    struct entry {
        run_token value;
        std::list<run_token_key>::iterator recency;
    };

    /// Moves @p held to the front, where the least recently used is the back.
    void
    touch(typename std::unordered_map<run_token_key, entry, run_token_key_hash>::iterator held) {
        recency_.splice(recency_.begin(), recency_, held->second.recency);
        held->second.recency = recency_.begin();
    }

    void insert(const run_token_key& key, run_token value) {
        if (const auto held = entries_.find(key); held != entries_.end()) {
            held->second.value = std::move(value);
            touch(held);
            return;
        }
        recency_.push_front(key);
        entries_.emplace(key, entry{.value = std::move(value), .recency = recency_.begin()});
        while (entries_.size() > capacity_) {
            entries_.erase(recency_.back());
            recency_.pop_back();
        }
    }

    void erase(const run_token_key& key) {
        if (const auto held = entries_.find(key); held != entries_.end())
            erase(held);
    }

    typename std::unordered_map<run_token_key, entry, run_token_key_hash>::iterator
    erase(typename std::unordered_map<run_token_key, entry, run_token_key_hash>::iterator held) {
        recency_.erase(held->second.recency);
        return entries_.erase(held);
    }

    run_token_minter mint_;
    std::size_t capacity_;
    std::chrono::milliseconds margin_;
    clock_fn now_;
    mutable std::mutex mutex_;
    std::unordered_map<run_token_key, entry, run_token_key_hash> entries_;
    std::list<run_token_key> recency_;
};

/**
 * @brief Drops a run's cached tokens when the scope ends.
 *
 * A step wraps its work in one of these, so nothing the cache holds outlives
 * the step on any return path, including a failure.
 */
class run_token_step_scope final {
public:
    run_token_step_scope(run_token_cache& cache, std::string run_id)
        : cache_(&cache)
        , run_id_(std::move(run_id)) {}

    ~run_token_step_scope() {
        cache_->evict_run(run_id_);
    }

    run_token_step_scope(const run_token_step_scope&) = delete;
    run_token_step_scope& operator=(const run_token_step_scope&) = delete;
    run_token_step_scope(run_token_step_scope&&) = delete;
    run_token_step_scope& operator=(run_token_step_scope&&) = delete;

private:
    run_token_cache* cache_;
    std::string run_id_;
};

}

#endif
