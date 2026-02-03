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

#include <string>
#include <functional>
#include <sqlgen/ConnectionPool.hpp>

namespace ores::database {

/**
 * @brief A connection pool wrapper that sets tenant context on acquire.
 *
 * PostgreSQL session variables are per-connection. When using a connection
 * pool, different operations may get different connections, causing RLS
 * policies that check the session variable to fail.
 *
 * This wrapper ensures that whenever a connection is acquired from the pool,
 * the tenant context is set via SET_CONFIG before returning the session.
 * This allows RLS policies to work correctly with connection pooling.
 */
template <class Connection>
class tenant_aware_pool {
public:
    /**
     * @brief Constructs a tenant-aware pool wrapper.
     *
     * @param pool The underlying connection pool
     * @param tenant_id The tenant ID to set on each acquired connection
     */
    tenant_aware_pool(sqlgen::ConnectionPool<Connection> pool,
                      std::string tenant_id)
        : pool_(std::move(pool)), tenant_id_(std::move(tenant_id)) {}

    /**
     * @brief Acquires a session and sets the tenant context.
     *
     * This method acquires a connection from the underlying pool and
     * executes SET_CONFIG to set the tenant context before returning.
     *
     * @return A session with tenant context set, or an error
     */
    sqlgen::Result<sqlgen::Ref<sqlgen::Session<Connection>>> acquire() noexcept {
        auto session_result = pool_.acquire();
        if (!session_result) {
            return session_result;
        }

        // Set tenant context on this connection
        if (!tenant_id_.empty()) {
            const std::string sql =
                "SELECT set_config('app.current_tenant_id', '" +
                tenant_id_ + "', false)";

            auto exec_result = (*session_result)->execute(sql);
            if (!exec_result) {
                return sqlgen::error("Failed to set tenant context: " +
                    std::string(exec_result.error().what()));
            }
        }

        return session_result;
    }

    /**
     * @brief Updates the tenant ID for subsequent acquisitions.
     *
     * @param tenant_id The new tenant ID to use
     */
    void set_tenant_id(std::string tenant_id) {
        tenant_id_ = std::move(tenant_id);
    }

    /**
     * @brief Gets the current tenant ID.
     */
    const std::string& tenant_id() const { return tenant_id_; }

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
    std::string tenant_id_;
};

}

namespace sqlgen {

/**
 * @brief Acquires a session from a tenant-aware pool.
 *
 * This overload in the sqlgen namespace allows tenant_aware_pool to be used
 * with the same sqlgen::session() pattern as regular ConnectionPool.
 */
template <class Connection>
Result<Ref<Session<Connection>>> session(
    ores::database::tenant_aware_pool<Connection>& pool) noexcept {
    return pool.acquire();
}

}

#endif
