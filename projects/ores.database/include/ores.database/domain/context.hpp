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

namespace ores::database {

/**
 * @brief Context for the operations on a postgres database.
 */
class context {
public:
    using connection_type = sqlgen::postgres::Connection;
    using connection_pool_type = sqlgen::Result<
        sqlgen::ConnectionPool<connection_type>>;

    explicit context(connection_pool_type connection_pool,
                     sqlgen::postgres::Credentials credentials,
                     std::string tenant_id = {})
    : connection_pool_(std::move(connection_pool)),
      credentials_(std::move(credentials)),
      tenant_id_(std::move(tenant_id)) {}

    connection_pool_type connection_pool() { return connection_pool_; }
    const sqlgen::postgres::Credentials& credentials() const { return credentials_; }

    /**
     * @brief Gets the tenant ID for this context.
     *
     * Returns the tenant ID that was set during context creation.
     * This is the preferred way to get the tenant ID for repository operations.
     */
    const std::string& tenant_id() const { return tenant_id_; }

    /**
     * @brief Sets the tenant ID for this context.
     *
     * This is used by tenant_context::set() to store the tenant ID
     * after setting the session variable.
     */
    void set_tenant_id(std::string tenant_id) { tenant_id_ = std::move(tenant_id); }

private:
    connection_pool_type connection_pool_;
    const sqlgen::postgres::Credentials credentials_;
    std::string tenant_id_;

};

}

#endif
