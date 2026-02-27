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

#include <optional>
#include <string>
#include <vector>
#include <sqlgen/ConnectionPool.hpp>
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
                      utility::uuid::tenant_id tenant_id)
        : pool_(std::move(pool)), tenant_id_(std::move(tenant_id)) {}

    /**
     * @brief Constructs a tenant-and-party-aware pool wrapper.
     */
    tenant_aware_pool(sqlgen::ConnectionPool<Connection> pool,
                      utility::uuid::tenant_id tenant_id,
                      boost::uuids::uuid party_id,
                      std::vector<boost::uuids::uuid> visible_party_ids)
        : pool_(std::move(pool)), tenant_id_(std::move(tenant_id)),
          party_id_(party_id),
          visible_party_ids_(std::move(visible_party_ids)) {}

    /**
     * @brief Acquires a session and sets the tenant (and party) context.
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
        (*session_result)->execute("ROLLBACK");

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
    utility::uuid::tenant_id tenant_id_;
    std::optional<boost::uuids::uuid> party_id_;
    std::vector<boost::uuids::uuid> visible_party_ids_;
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
