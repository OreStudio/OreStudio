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
#ifndef ORES_IAM_REPOSITORY_AUDIT_SCOPE_HPP
#define ORES_IAM_REPOSITORY_AUDIT_SCOPE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <string>

namespace ores::iam::repository {

/**
 * @brief The tenant an audit row belongs to, and the context it is written in.
 *
 * The authentication and run token logs carry their tenant as text, and their
 * rows are isolated by tenant like every other IAM table. A row can only be
 * written by a session of the tenant it names, so a writer that holds another
 * tenant's context, such as the service's own, writes through the context of
 * the row's tenant.
 *
 * An event that names no tenant, or one that is not a tenant id, belongs to
 * the platform: it is written as the system tenant's and only the system
 * tenant reads it back.
 */
struct audit_scope {
    ores::database::context ctx;
    std::string tenant_id;
};

[[nodiscard]] inline audit_scope audit_scope_for(const ores::database::context& ctx,
                                                 const std::string& tenant_id) {
    const auto parsed = ores::utility::uuid::tenant_id::from_string(tenant_id);
    const auto tenant = parsed ? *parsed : ores::utility::uuid::tenant_id::system();
    return {ctx.with_tenant(tenant, ctx.actor()), tenant.to_string()};
}

}

#endif
