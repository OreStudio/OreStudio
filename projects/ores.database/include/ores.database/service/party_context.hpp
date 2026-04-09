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
#ifndef ORES_DATABASE_SERVICE_PARTY_CONTEXT_HPP
#define ORES_DATABASE_SERVICE_PARTY_CONTEXT_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::database::service {

/**
 * @brief Manages party context for party-isolated database operations.
 *
 * Party isolation is a second layer of RLS on top of tenant isolation.
 * Tables with a party_id column use AS RESTRICTIVE FOR SELECT policies
 * that filter rows to those visible to the current party (and its
 * descendants in the party hierarchy).
 *
 * Use with_party() to derive a new context that sets app.visible_party_ids
 * on every connection, enabling all party-isolated reads and writes.
 * Publishers must call this once per party before a batch of DML.
 */
class party_context final {
public:
    party_context() = delete;

    /**
     * @brief Creates a new context with tenant and party isolation.
     *
     * Queries ores_refdata_visible_party_ids_fn to compute the visible party
     * set (the given party and all its descendants), then returns a new
     * context that sets app.visible_party_ids on each connection acquire.
     *
     * @param ctx     Source context (must already have tenant context set).
     * @param tenant  Tenant ID.
     * @param party   Party ID whose subtree should be visible.
     * @param actor   Optional actor name for audit trail.
     * @return A new context configured for party-scoped operations.
     * @throws std::runtime_error if the party is not found in the tenant.
     */
    [[nodiscard]] static context with_party(const context& ctx,
        const utility::uuid::tenant_id& tenant,
        const boost::uuids::uuid& party,
        const std::string& actor = "");
};

}

#endif
