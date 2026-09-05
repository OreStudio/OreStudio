/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DATABASE_TENANT_AWARE_POOL_HPP
#define ORES_DATABASE_TENANT_AWARE_POOL_HPP

#include "ores.database/domain/session_utilities.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstddef>
#include <functional>
#include <mutex>
#include <optional>
#include <sqlgen/ConnectionPool.hpp>
#include <sqlgen/postgres.hpp>
#include <string>
#include <thread>
#include <vector>

namespace ores::database {

/**
 * @brief How long to wait between connection-acquisition attempts.
 *
 * Linear waits a fixed interval between attempts (the historical sqlgen
 * behaviour). Exponential doubles the interval per attempt, bounded by an
 * internal cap, so sustained contention is absorbed politely while a freed
 * connection is still discovered promptly.
 */
enum class pool_backoff_strategy { linear, exponential };

/**
 * @brief Retry policy for acquiring a connection from the pool.
 *
 * The tenant-aware pool probes the underlying pool (configured to fail fast)
 * and retries up to @c num_attempts times, sleeping between attempts. The
 * retry limit is bounded: after the last attempt the acquisition fails with
 * the usual "No available connections in the pool." error.
 */
struct pool_acquire_policy {
    /// Maximum number of acquisition attempts.
    std::size_t num_attempts = 10;
    /// Base wait between attempts, in seconds.
    std::size_t wait_time_in_seconds = 1;
    /// Wait shape between attempts.
    pool_backoff_strategy strategy = pool_backoff_strategy::exponential;
};

/**
 * @brief A connection pool wrapper that sets tenant and party context on acquire.
 *
 * PostgreSQL session variables are per-connection. When using a connection
 * pool, different operations may get different connections, causing RLS
 * policies that check the session variable to fail.
 *
 * This wrapper ensures that whenever a connection is acquired from the pool,
 * the tenant context (and optionally party context) is set via SET_CONFIG
 * before returning the session. This allows RLS policies to work correctly
 * with connection pooling.
 */
template <class Connection>
class tenant_aware_pool {
private:
    inline static std::string_view logger_name = "ores.database.domain.tenant_aware_pool";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs a tenant-aware pool wrapper (tenant-only).
     */
    tenant_aware_pool(sqlgen::ConnectionPool<Connection> pool,
                      sqlgen::postgres::Credentials credentials,
                      utility::uuid::tenant_id tenant_id,
                      std::string actor = "",
                      std::string service_account = "",
                      pool_acquire_policy policy = {})
        : pool_(std::move(pool))
        , credentials_(std::move(credentials))
        , pool_size_(pool_.size())
        , policy_(std::move(policy))
        , reconnect_mutex_(std::make_shared<std::mutex>())
        , pool_generation_(std::make_shared<std::atomic<std::size_t>>(0))
        , tenant_id_(std::move(tenant_id))
        , actor_(std::move(actor))
        , service_account_(std::move(service_account)) {}

    /**
     * @brief Constructs a tenant-and-party-aware pool wrapper.
     */
    tenant_aware_pool(sqlgen::ConnectionPool<Connection> pool,
                      sqlgen::postgres::Credentials credentials,
                      utility::uuid::tenant_id tenant_id,
                      boost::uuids::uuid party_id,
                      std::vector<boost::uuids::uuid> visible_party_ids,
                      std::string actor = "",
                      std::string service_account = "",
                      pool_acquire_policy policy = {})
        : pool_(std::move(pool))
        , credentials_(std::move(credentials))
        , pool_size_(pool_.size())
        , policy_(std::move(policy))
        , reconnect_mutex_(std::make_shared<std::mutex>())
        , pool_generation_(std::make_shared<std::atomic<std::size_t>>(0))
        , tenant_id_(std::move(tenant_id))
        , party_id_(party_id)
        , visible_party_ids_(std::move(visible_party_ids))
        , actor_(std::move(actor))
        , service_account_(std::move(service_account)) {}

    /**
     * @brief Acquires a session and sets the tenant (and party) context.
     *
     * If the probing ROLLBACK fails (dead connection after a DB restart), the
     * entire pool is rebuilt from the stored credentials under the backoff
     * policy and the acquire is retried.
     */
    sqlgen::Result<sqlgen::Ref<sqlgen::Session<Connection>>> acquire() noexcept {
        using namespace ores::logging;

        auto session_result = acquire_with_backoff();
        if (!session_result) {
            return session_result;
        }

        // The generation of the pool this session was acquired from; the
        // rebuild path uses it to skip re-making when another thread already
        // replaced the pool.
        const auto pool_generation_at_start = pool_generation_->load(std::memory_order_relaxed);

        // Speculatively rollback any aborted transaction left by a previous
        // failed operation. PostgreSQL accepts ROLLBACK even when no
        // transaction is active, so this is always safe.
        // If ROLLBACK itself fails the connection is dead — rebuild the pool.
        bool needs_rebuild = false;
        std::string rollback_error;
        {
            auto rollback_result = (*session_result)->execute("ROLLBACK");
            if (!rollback_result) {
                needs_rebuild = true;
                rollback_error = rollback_result.error().what();
            }
        } // session_result still held here intentionally

        if (needs_rebuild) {
            BOOST_LOG_SEV(lg(), warn) << "Pool connection dead (ROLLBACK failed: " << rollback_error
                                      << "). Rebuilding pool...";
            // The mutex guards each make attempt and the pool swap only; the
            // backoff waits between attempts happen outside it, so threads
            // queueing behind a rebuild are not blocked for the whole budget.
            // Probe mode: the backoff policy governs the rebuild attempts, so
            // a database still restarting is absorbed instead of failing on
            // the first refused connection.
            sqlgen::ConnectionPoolConfig cfg{
                .size = pool_size_, .num_attempts = 1, .wait_time_in_seconds = 0};
            std::string rebuild_error;
            const bool rebuilt = retry_probe(
                [&]() {
                    std::lock_guard lock(*reconnect_mutex_);
                    // Another thread already replaced the dead pool while this
                    // one waited; nothing to rebuild.
                    if (pool_generation_->load(std::memory_order_relaxed) !=
                        pool_generation_at_start)
                        return true;
                    auto new_pool = sqlgen::make_connection_pool<Connection>(cfg, credentials_);
                    if (!new_pool) {
                        rebuild_error = new_pool.error().what();
                        return false;
                    }
                    // Sessions co-own their connection and in-use flag, so the
                    // swap is safe while the dead session is still outstanding.
                    pool_ = std::move(*new_pool);
                    pool_generation_->fetch_add(1, std::memory_order_relaxed);
                    BOOST_LOG_SEV(lg(), info) << "Pool rebuilt successfully.";
                    return true;
                },
                [&](std::size_t attempt) {
                    BOOST_LOG_SEV(lg(), warn)
                        << "Pool rebuild attempt " << attempt << " of " << policy_.num_attempts
                        << " failed: " << rebuild_error;
                });
            if (!rebuilt) {
                BOOST_LOG_SEV(lg(), error) << "Pool rebuild failed: " << rebuild_error;
                return sqlgen::error("Pool rebuild failed: " + rebuild_error);
            }
            session_result = acquire_with_backoff();
            if (!session_result)
                return session_result;
            // Best-effort: roll back any aborted transaction on the fresh
            // connection. PostgreSQL accepts ROLLBACK when no transaction is
            // active, so this is always safe.
            (*session_result)->execute("ROLLBACK");
        }

        // Force UTC for all timestamp operations on this connection.
        // PostgreSQL returns timestamptz values as "YYYY-MM-DD HH:MM:SS+00"
        // when the session timezone is UTC, which from_iso8601_utc accepts.
        auto tz_result = domain::force_session_utc(**session_result);
        if (!tz_result) {
            return sqlgen::error("Failed to set session timezone to UTC: " +
                                 std::string(tz_result.error().what()));
        }

        const auto tenant_id_str = tenant_id_.to_string();
        const std::string sql =
            "SELECT set_config('app.current_tenant_id', '" + tenant_id_str + "', false)";

        auto exec_result = (*session_result)->execute(sql);
        if (!exec_result) {
            return sqlgen::error("Failed to set tenant context: " +
                                 std::string(exec_result.error().what()));
        }

        BOOST_LOG_SEV(lg(), debug) << "Set tenant context to: " << tenant_id_str;

        // Set party context if available
        if (party_id_.has_value()) {
            const auto party_id_str = boost::uuids::to_string(*party_id_);
            const std::string party_sql =
                "SELECT set_config('app.current_party_id', '" + party_id_str + "', false)";

            auto party_result = (*session_result)->execute(party_sql);
            if (!party_result) {
                return sqlgen::error("Failed to set party context: " +
                                     std::string(party_result.error().what()));
            }

            BOOST_LOG_SEV(lg(), debug) << "Set party context to: " << party_id_str;
        }

        // Set visible party IDs if available
        if (!visible_party_ids_.empty()) {
            std::string ids_str = "{";
            for (std::size_t i = 0; i < visible_party_ids_.size(); ++i) {
                if (i > 0)
                    ids_str += ",";
                ids_str += boost::uuids::to_string(visible_party_ids_[i]);
            }
            ids_str += "}";

            const std::string vis_sql =
                "SELECT set_config('app.visible_party_ids', '" + ids_str + "', false)";

            auto vis_result = (*session_result)->execute(vis_sql);
            if (!vis_result) {
                return sqlgen::error("Failed to set visible party IDs: " +
                                     std::string(vis_result.error().what()));
            }

            BOOST_LOG_SEV(lg(), debug)
                << "Set visible party IDs (" << visible_party_ids_.size() << " parties)";
        }

        // Set current actor (username) if available.
        if (!actor_.empty()) {
            const std::string actor_sql =
                "SELECT set_config('app.current_actor', '" + actor_ + "', false)";

            auto actor_result = (*session_result)->execute(actor_sql);
            if (!actor_result) {
                return sqlgen::error("Failed to set actor context: " +
                                     std::string(actor_result.error().what()));
            }

            BOOST_LOG_SEV(lg(), debug) << "Set actor context to: " << actor_;
        }

        // Set current service (service account) if available.
        // This is used by DB triggers to stamp performed_by.
        if (!service_account_.empty()) {
            const std::string svc_sql =
                "SELECT set_config('app.current_service', '" + service_account_ + "', false)";
            auto svc_result = (*session_result)->execute(svc_sql);
            if (!svc_result) {
                return sqlgen::error("Failed to set service context: " +
                                     std::string(svc_result.error().what()));
            }

            BOOST_LOG_SEV(lg(), debug) << "Set service context to: " << service_account_;
        }

        return session_result;
    }

    /**
     * @brief Gets the current tenant ID.
     */
    const utility::uuid::tenant_id& tenant_id() const {
        return tenant_id_;
    }

    /**
     * @brief Gets the current party ID, if set.
     */
    std::optional<boost::uuids::uuid> party_id() const {
        return party_id_;
    }

    /**
     * @brief Gets the visible party IDs.
     */
    const std::vector<boost::uuids::uuid>& visible_party_ids() const {
        return visible_party_ids_;
    }

    /**
     * @brief Gets the current actor (username), if set.
     */
    const std::string& actor() const {
        return actor_;
    }

    /**
     * @brief Gets the current service account, if set.
     */
    const std::string& service_account() const {
        return service_account_;
    }

    /**
     * @brief Gets the underlying connection pool.
     */
    const sqlgen::ConnectionPool<Connection>& underlying_pool() const {
        return pool_;
    }

    /**
     * @brief Gets the number of available connections.
     */
    size_t available() const {
        return pool_.available();
    }

    /**
     * @brief Gets the total number of connections in the pool.
     */
    size_t size() const {
        return pool_.size();
    }

private:
    /**
     * @brief Runs a fail-fast probe until it succeeds or the policy
     * budget is exhausted.
     *
     * The first probe runs immediately; after each failure @c on_failure
     * runs so the caller can log the probe's own failure detail, then
     * the wait before the next probe follows the policy (linear or
     * exponential, with per-thread jitter). The first wait logs at warn
     * level, later waits at debug level.
     *
     * @return true when a probe succeeded.
     */
    template <typename Probe, typename OnFailure>
    bool retry_probe(Probe&& probe, OnFailure&& on_failure) noexcept {
        using namespace ores::logging;
        bool ok = probe();
        for (std::size_t attempt = 1; !ok && attempt < policy_.num_attempts; ++attempt) {
            on_failure(attempt);
            std::this_thread::sleep_for(backoff_delay(attempt, policy_));
            if (attempt == 1)
                BOOST_LOG_SEV(lg(), warn)
                    << "No available database connections in the pool; waiting with "
                    << (policy_.strategy == pool_backoff_strategy::exponential ? "exponential" :
                                                                                 "linear")
                    << " backoff (attempt " << (attempt + 1) << " of " << policy_.num_attempts
                    << ")";
            else
                BOOST_LOG_SEV(lg(), debug)
                    << "Still waiting for a database connection (attempt " << (attempt + 1)
                    << " of " << policy_.num_attempts << ")";
            ok = probe();
        }
        return ok;
    }

    /**
     * @brief retry_probe without a per-failure callback.
     */
    template <typename Probe>
    bool retry_probe(Probe&& probe) noexcept {
        return retry_probe(probe, [](std::size_t) {});
    }

    /**
     * @brief Probes the pool until a connection is free or the policy
     * budget is exhausted.
     *
     * The underlying pool is configured to fail fast; the retry policy
     * lives here. The last probe's outcome is returned, whether a
     * success or the error that exhausted the budget.
     */
    [[nodiscard]] sqlgen::Result<sqlgen::Ref<sqlgen::Session<Connection>>>
    acquire_with_backoff() noexcept {
        sqlgen::Result<sqlgen::Ref<sqlgen::Session<Connection>>> session_result =
            sqlgen::error("No available connections in the pool.");
        retry_probe([&]() {
            session_result = pool_.acquire();
            return static_cast<bool>(session_result);
        });
        return session_result;
    }

    /**
     * @brief Sleep duration before the given acquisition attempt.
     *
     * Linear waits a fixed interval per attempt. Exponential doubles the
     * interval per attempt, capped at 10 seconds, so the wait is absorbed
     * politely under sustained contention while a freed connection is still
     * discovered promptly. A small deterministic per-thread jitter spreads
     * waiting threads so they do not re-probe in lockstep.
     */
    [[nodiscard]] static std::chrono::milliseconds
    backoff_delay(std::size_t attempt, const pool_acquire_policy& policy) noexcept {
        constexpr auto max_delay = std::chrono::milliseconds(10'000);
        auto delay = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::seconds(policy.wait_time_in_seconds));
        if (policy.strategy == pool_backoff_strategy::exponential &&
            delay > std::chrono::milliseconds::zero()) {
            for (std::size_t i = 1; i < attempt; ++i)
                delay = std::min(max_delay, delay * 2);
        }
        const auto tid = std::hash<std::thread::id>{}(std::this_thread::get_id());
        const auto jitter = std::chrono::milliseconds(static_cast<long long>(tid % 201) - 100);
        delay += jitter;
        if (delay < std::chrono::milliseconds(1))
            delay = std::chrono::milliseconds(1);
        return delay;
    }

    sqlgen::ConnectionPool<Connection> pool_;
    sqlgen::postgres::Credentials credentials_;
    std::size_t pool_size_;
    pool_acquire_policy policy_;
    std::shared_ptr<std::mutex> reconnect_mutex_;
    /// Bumped on each rebuild; shared so copies of this pool share it.
    std::shared_ptr<std::atomic<std::size_t>> pool_generation_;
    utility::uuid::tenant_id tenant_id_;
    std::optional<boost::uuids::uuid> party_id_;
    std::vector<boost::uuids::uuid> visible_party_ids_;
    std::string actor_;
    std::string service_account_;
};

}

namespace sqlgen {

template <class Connection>
Result<Ref<Session<Connection>>>
session(ores::database::tenant_aware_pool<Connection>& pool) noexcept {
    return pool.acquire();
}

}

#endif
