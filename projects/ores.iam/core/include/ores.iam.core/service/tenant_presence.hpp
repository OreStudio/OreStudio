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
#ifndef ORES_IAM_CORE_SERVICE_TENANT_PRESENCE_HPP
#define ORES_IAM_CORE_SERVICE_TENANT_PRESENCE_HPP

#include "ores.iam.api/domain/tenant.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <vector>

namespace ores::iam::service {

/**
 * Whether a deployment has a tenant of its own.
 *
 * The tenant registry is system-scoped: the table's own check requires every
 * row to carry the system tenant in =tenant_id=, so that column names the
 * owner of the registry and never the tenant the row describes. A row's
 * identity is its =id=, and the deployment's own bookkeeping is the row whose
 * id is the system id. Every other live row is a tenant somebody set up, so a
 * deployment with none of them has not been set up yet, whatever else it holds.
 *
 * Reading the wrong column here is not a quiet mistake: a predicate that asks
 * =tenant_id= answers "no tenant" for a deployment full of them, which is what
 * a setup screen turns into a screen that never lets go.
 */
[[nodiscard]] inline bool has_tenant_of_its_own(const std::vector<domain::tenant>& tenants) {
    const auto system_tenant = utility::uuid::tenant_id::system().to_uuid();
    return std::ranges::any_of(tenants, [system_tenant](const domain::tenant& tenant) {
        return tenant.id != system_tenant;
    });
}

} // namespace ores::iam::service

#endif
