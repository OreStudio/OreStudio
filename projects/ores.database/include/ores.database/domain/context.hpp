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
#ifndef ORES_DATABASE_CONTEXT_HPP
#define ORES_DATABASE_CONTEXT_HPP

#include <sqlgen/postgres.hpp>
#include "ores.database/domain/tenant_aware_pool.hpp"

namespace ores::database {

/**
 * @brief Context for the operations on a postgres database.
 *
 * The context wraps a tenant-aware connection pool that automatically sets
 * the tenant context (via PostgreSQL session variable) whenever a connection
 * is acquired. This ensures RLS policies work correctly with connection pooling.
 */
class context {
public:
    using connection_type = sqlgen::postgres::Connection;
    using connection_pool_type = tenant_aware_pool<connection_type>;

    explicit context(sqlgen::ConnectionPool<connection_type> connection_pool,
                     sqlgen::postgres::Credentials credentials,
                     std::string tenant_id = {})
    : connection_pool_(std::move(connection_pool), tenant_id),
      credentials_(std::move(credentials)) {}

    /**
     * @brief Gets the tenant-aware connection pool.
     *
     * Sessions acquired from this pool will automatically have the
     * tenant context set via SET_CONFIG before use.
     */
    connection_pool_type& connection_pool() { return connection_pool_; }

    const sqlgen::postgres::Credentials& credentials() const { return credentials_; }

    /**
     * @brief Gets the tenant ID for this context.
     */
    const std::string& tenant_id() const { return connection_pool_.tenant_id(); }

    /**
     * @brief Sets the tenant ID for this context.
     *
     * Updates the tenant ID used by the connection pool. All subsequent
     * connection acquisitions will use the new tenant ID.
     */
    void set_tenant_id(std::string tenant_id) {
        connection_pool_.set_tenant_id(std::move(tenant_id));
    }

private:
    connection_pool_type connection_pool_;
    const sqlgen::postgres::Credentials credentials_;
};

}

#endif
