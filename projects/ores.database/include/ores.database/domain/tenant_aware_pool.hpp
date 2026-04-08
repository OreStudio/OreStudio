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

#include <mutex>
#include <optional>
#include <string>
#include <vector>
#include <sqlgen/ConnectionPool.hpp>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::database {

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
    inline static std::string_view logger_name =
        "ores.database.domain.tenant_aware_pool";

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
                      std::string service_account = "")
        : pool_(std::move(pool)), credentials_(std::move(credentials)),
          pool_size_(pool_.size()),
          reconnect_mutex_(std::make_shared<std::mutex>()),
          tenant_id_(std::move(tenant_id)),
          actor_(std::move(actor)),
          service_account_(std::move(service_account)) {}

    /**
     * @brief Constructs a tenant-and-party-aware pool wrapper.
     */
    tenant_aware_pool(sqlgen::ConnectionPool<Connection> pool,
                      sqlgen::postgres::Credentials credentials,
                      utility::uuid::tenant_id tenant_id,
                      boost::uuids::uuid party_id,
                      std::vector<boost::uuids::uuid> visible_party_ids,
                      std::string actor = "",
                      std::string service_account = "")
        : pool_(std::move(pool)), credentials_(std::move(credentials)),
          pool_size_(pool_.size()),
          reconnect_mutex_(std::make_shared<std::mutex>()),
          tenant_id_(std::move(tenant_id)),
          party_id_(party_id),
          visible_party_ids_(std::move(visible_party_ids)),
          actor_(std::move(actor)),
          service_account_(std::move(service_account)) {}

    /**
     * @brief Acquires a session and sets the tenant (and party) context.
     *
     * If the probing ROLLBACK fails (dead connection after a DB restart), the
     * entire pool is rebuilt from the stored credentials and the acquire is
     * retried once.
     */
    sqlgen::Result<sqlgen::Ref<sqlgen::Session<Connection>>> acquire() noexcept {
        using namespace ores::logging;

        auto session_result = pool_.acquire();
        if (!session_result) {
            return session_result;
        }

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
            BOOST_LOG_SEV(lg(), warn)
                << "Pool connection dead (ROLLBACK failed: "
                << rollback_error << "). Rebuilding pool...";
            // Drop session so the connection flag is released before we
            // replace the pool (avoids use-after-free of the old pool entry).
            session_result = pool_.acquire(); // re-acquire to replace below
            {
                std::lock_guard lock(*reconnect_mutex_);
                sqlgen::ConnectionPoolConfig cfg{
                    .size = pool_size_,
                    .num_attempts = 3,
                    .wait_time_in_seconds = 1
                };
                auto new_pool = sqlgen::make_connection_pool<Connection>(
                    cfg, credentials_);
                if (new_pool) {
                    pool_ = std::move(*new_pool);
                    BOOST_LOG_SEV(lg(), info) << "Pool rebuilt successfully.";
                } else {
                    BOOST_LOG_SEV(lg(), error)
                        << "Pool rebuild failed: " << new_pool.error().what();
                    return sqlgen::error("Pool rebuild failed: " +
                        std::string(new_pool.error().what()));
                }
            }
            session_result = pool_.acquire();
            if (!session_result) return session_result;
            (*session_result)->execute("ROLLBACK"); // best-effort on fresh conn
        }

        // Force UTC for all timestamp operations on this connection.
        // PostgreSQL returns timestamptz values as "YYYY-MM-DD HH:MM:SS+00"
        // when the session timezone is UTC, which from_iso8601_utc accepts.
        auto tz_result = (*session_result)->execute(
            "SELECT set_config('TimeZone', 'UTC', false)");
        if (!tz_result) {
            return sqlgen::error("Failed to set session timezone to UTC: " +
                std::string(tz_result.error().what()));
        }

        const auto tenant_id_str = tenant_id_.to_string();
        const std::string sql =
            "SELECT set_config('app.current_tenant_id', '" +
            tenant_id_str + "', false)";

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
                "SELECT set_config('app.current_party_id', '" +
                party_id_str + "', false)";

            auto party_result = (*session_result)->execute(party_sql);
            if (!party_result) {
                return sqlgen::error("Failed to set party context: " +
                    std::string(party_result.error().what()));
            }

            BOOST_LOG_SEV(lg(), debug) << "Set party context to: "
                                       << party_id_str;
        }

        // Set visible party IDs if available
        if (!visible_party_ids_.empty()) {
            std::string ids_str = "{";
            for (std::size_t i = 0; i < visible_party_ids_.size(); ++i) {
                if (i > 0) ids_str += ",";
                ids_str += boost::uuids::to_string(visible_party_ids_[i]);
            }
            ids_str += "}";

            const std::string vis_sql =
                "SELECT set_config('app.visible_party_ids', '" +
                ids_str + "', false)";

            auto vis_result = (*session_result)->execute(vis_sql);
            if (!vis_result) {
                return sqlgen::error("Failed to set visible party IDs: " +
                    std::string(vis_result.error().what()));
            }

            BOOST_LOG_SEV(lg(), debug) << "Set visible party IDs ("
                                       << visible_party_ids_.size()
                                       << " parties)";
        }

        // Set current actor (username) if available.
        if (!actor_.empty()) {
            const std::string actor_sql =
                "SELECT set_config('app.current_actor', '" +
                actor_ + "', false)";

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
                "SELECT set_config('app.current_service', '" +
                service_account_ + "', false)";
            auto svc_result = (*session_result)->execute(svc_sql);
            if (!svc_result) {
                return sqlgen::error("Failed to set service context: " +
                    std::string(svc_result.error().what()));
            }

            BOOST_LOG_SEV(lg(), debug) << "Set service context to: "
                                       << service_account_;
        }

        return session_result;
    }

    /**
     * @brief Gets the current tenant ID.
     */
    const utility::uuid::tenant_id& tenant_id() const { return tenant_id_; }

    /**
     * @brief Gets the current party ID, if set.
     */
    std::optional<boost::uuids::uuid> party_id() const { return party_id_; }

    /**
     * @brief Gets the visible party IDs.
     */
    const std::vector<boost::uuids::uuid>& visible_party_ids() const {
        return visible_party_ids_;
    }

    /**
     * @brief Gets the current actor (username), if set.
     */
    const std::string& actor() const { return actor_; }

    /**
     * @brief Gets the current service account, if set.
     */
    const std::string& service_account() const { return service_account_; }

    /**
     * @brief Gets the underlying connection pool.
     */
    const sqlgen::ConnectionPool<Connection>& underlying_pool() const {
        return pool_;
    }

    /**
     * @brief Gets the number of available connections.
     */
    size_t available() const { return pool_.available(); }

    /**
     * @brief Gets the total number of connections in the pool.
     */
    size_t size() const { return pool_.size(); }

private:
    sqlgen::ConnectionPool<Connection> pool_;
    sqlgen::postgres::Credentials credentials_;
    std::size_t pool_size_;
    std::shared_ptr<std::mutex> reconnect_mutex_;
    utility::uuid::tenant_id tenant_id_;
    std::optional<boost::uuids::uuid> party_id_;
    std::vector<boost::uuids::uuid> visible_party_ids_;
    std::string actor_;
    std::string service_account_;
};

}

namespace sqlgen {

template <class Connection>
Result<Ref<Session<Connection>>> session(
    ores::database::tenant_aware_pool<Connection>& pool) noexcept {
    return pool.acquire();
}

}

#endif
