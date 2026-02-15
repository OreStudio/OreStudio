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

#include <vector>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::database {

/**
 * @brief Context for the operations on a postgres database.
 *
 * The context wraps a tenant-aware connection pool that automatically sets
 * the tenant context (and optionally party context) via PostgreSQL session
 * variables whenever a connection is acquired. This ensures RLS policies
 * work correctly with connection pooling.
 */
class context {
public:
    using connection_type = sqlgen::postgres::Connection;
    using connection_pool_type = tenant_aware_pool<connection_type>;

    /**
     * @brief Constructs a tenant-only context.
     */
    explicit context(sqlgen::ConnectionPool<connection_type> connection_pool,
                     sqlgen::postgres::Credentials credentials,
                     utility::uuid::tenant_id tenant_id)
    : connection_pool_(std::move(connection_pool), std::move(tenant_id)),
      credentials_(std::move(credentials)) {}

    /**
     * @brief Constructs a tenant-and-party-aware context.
     */
    explicit context(sqlgen::ConnectionPool<connection_type> connection_pool,
                     sqlgen::postgres::Credentials credentials,
                     utility::uuid::tenant_id tenant_id,
                     boost::uuids::uuid party_id,
                     std::vector<boost::uuids::uuid> visible_party_ids)
    : connection_pool_(std::move(connection_pool), std::move(tenant_id),
                       party_id, std::move(visible_party_ids)),
      credentials_(std::move(credentials)) {}

    /**
     * @brief Gets the tenant-aware connection pool.
     */
    connection_pool_type& connection_pool() { return connection_pool_; }

    /**
     * @brief Gets the credentials for this context.
     */
    const sqlgen::postgres::Credentials& credentials() const { return credentials_; }

    /**
     * @brief Gets the tenant ID for this context.
     */
    const utility::uuid::tenant_id& tenant_id() const {
        return connection_pool_.tenant_id();
    }

    /**
     * @brief Gets the party ID for this context, if set.
     */
    std::optional<boost::uuids::uuid> party_id() const {
        return connection_pool_.party_id();
    }

    /**
     * @brief Gets the visible party IDs for this context.
     */
    const std::vector<boost::uuids::uuid>& visible_party_ids() const {
        return connection_pool_.visible_party_ids();
    }

    /**
     * @brief Gets the underlying raw connection pool.
     */
    const sqlgen::ConnectionPool<connection_type>& underlying_pool() const {
        return connection_pool_.underlying_pool();
    }

    /**
     * @brief Creates a new context with a different tenant ID (no party).
     */
    [[nodiscard]] context with_tenant(utility::uuid::tenant_id tenant_id) const {
        return context(connection_pool_.underlying_pool(), credentials_,
                       std::move(tenant_id));
    }

    /**
     * @brief Creates a new context with tenant and party isolation.
     */
    [[nodiscard]] context with_party(
        utility::uuid::tenant_id tenant_id,
        boost::uuids::uuid party_id,
        std::vector<boost::uuids::uuid> visible_party_ids) const {
        return context(connection_pool_.underlying_pool(), credentials_,
                       std::move(tenant_id), party_id,
                       std::move(visible_party_ids));
    }

private:
    connection_pool_type connection_pool_;
    sqlgen::postgres::Credentials credentials_;
};

}

#endif
