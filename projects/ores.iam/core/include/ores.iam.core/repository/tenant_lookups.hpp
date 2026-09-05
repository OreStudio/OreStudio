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
#ifndef ORES_IAM_CORE_REPOSITORY_TENANT_LOOKUPS_HPP
#define ORES_IAM_CORE_REPOSITORY_TENANT_LOOKUPS_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/tenant.hpp"
#include "ores.iam.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::iam::repository {

// Hand-authored counterpart of tenant_repository: reads that must see every
// tenant, not only the caller's. The C3 rebind made tenant_repository
// tenant-scoped (every read filters on the tenant carried by the context),
// which serves the CRUD flows but not login (hostname resolution happens
// before any tenant context exists) or the registrar party-cache warm-up
// (system tenant acting for all tenants). These functions keep the
// pre-rebind, unfiltered queries. No codegen template emits this file, so
// regeneration leaves it alone.
//
// The reads are plain queries, so row-level security still applies: a
// session resolves what the tenants_read_policy admits -- the system
// tenant (every row) and a tenant's own record by id. Bootstrap callers
// run under the system-scoped base context, never under a peer tenant's.

/**
 * @brief Reads the latest version of all non-deleted tenants, ordered by name.
 */
ORES_IAM_CORE_EXPORT std::vector<domain::tenant>
read_all_active_tenants(const ores::database::context& ctx);

/**
 * @brief Reads the latest version of the tenant with the given id.
 */
ORES_IAM_CORE_EXPORT std::vector<domain::tenant>
read_active_tenant_by_id(const ores::database::context& ctx, const boost::uuids::uuid& id);

/**
 * @brief Reads the latest version of the tenant with the given hostname.
 */
ORES_IAM_CORE_EXPORT std::vector<domain::tenant>
read_active_tenant_by_hostname(const ores::database::context& ctx, const std::string& hostname);

} // namespace ores::iam::repository

#endif
