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
#ifndef ORES_IAM_DOMAIN_PERMISSION_HPP
#define ORES_IAM_DOMAIN_PERMISSION_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::iam::domain {

/**
 * @brief Represents an atomic permission that can be granted to roles.
 *
 * Permissions follow a hierarchical naming convention:
 * "component::resource:action" (e.g., "iam::accounts:create").
 *
 * Wildcard permissions:
 * - "*" grants all permissions (superuser)
 * - "component::*" grants all permissions within a component
 *
 * @note This type does not include change tracking fields (change_reason_code,
 * change_commentary) because permissions are system-defined constants populated
 * from bootstrap data. They are not user-editable and do not require audit
 * trails. Roles, which reference permissions, are user-manageable and do have
 * change tracking.
 */
struct permission final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for the permission.
     */
    boost::uuids::uuid id;

    /**
     * @brief Permission code following the format "component::resource:action".
     *
     * Examples: "iam::accounts:create", "refdata::currencies:read".
     * Use "*" for wildcard (all permissions).
     */
    std::string code;

    /**
     * @brief Human-readable description of what this permission allows.
     */
    std::string description;
};

/**
 * @brief Well-known permission codes used throughout the system.
 *
 * Permission naming convention: "component::resource:action"
 * - component: The system component (iam, refdata, variability, dq)
 * - resource: The entity type (accounts, currencies, etc.)
 * - action: The operation (create, read, update, delete, etc.)
 */
namespace permissions {
    // =========================================================================
    // IAM Component (iam::)
    // =========================================================================

    // Account management
    constexpr auto accounts_create = "iam::accounts:create";
    constexpr auto accounts_read = "iam::accounts:read";
    constexpr auto accounts_update = "iam::accounts:update";
    constexpr auto accounts_delete = "iam::accounts:delete";
    constexpr auto accounts_lock = "iam::accounts:lock";
    constexpr auto accounts_unlock = "iam::accounts:unlock";
    constexpr auto accounts_reset_password = "iam::accounts:reset_password";

    // Roles management
    constexpr auto roles_create = "iam::roles:create";
    constexpr auto roles_read = "iam::roles:read";
    constexpr auto roles_update = "iam::roles:update";
    constexpr auto roles_delete = "iam::roles:delete";
    constexpr auto roles_assign = "iam::roles:assign";
    constexpr auto roles_revoke = "iam::roles:revoke";

    // Login info (read-only audit data)
    constexpr auto login_info_read = "iam::login_info:read";

    // Tenant management
    constexpr auto tenants_create = "iam::tenants:create";
    constexpr auto tenants_read = "iam::tenants:read";
    constexpr auto tenants_update = "iam::tenants:update";
    constexpr auto tenants_delete = "iam::tenants:delete";
    constexpr auto tenants_suspend = "iam::tenants:suspend";
    constexpr auto tenants_terminate = "iam::tenants:terminate";
    constexpr auto tenants_impersonate = "iam::tenants:impersonate";

    // IAM component wildcard
    constexpr auto iam_all = "iam::*";

    // =========================================================================
    // Reference Data Component (refdata::)
    // =========================================================================

    // Currency management
    constexpr auto currencies_create = "refdata::currencies:create";
    constexpr auto currencies_read = "refdata::currencies:read";
    constexpr auto currencies_update = "refdata::currencies:update";
    constexpr auto currencies_delete = "refdata::currencies:delete";
    constexpr auto currencies_history = "refdata::currencies:history";

    // Refdata component wildcard
    constexpr auto refdata_all = "refdata::*";

    // =========================================================================
    // Variability Component (variability::)
    // =========================================================================

    // Feature flags management
    constexpr auto flags_create = "variability::flags:create";
    constexpr auto flags_read = "variability::flags:read";
    constexpr auto flags_update = "variability::flags:update";
    constexpr auto flags_delete = "variability::flags:delete";

    // Variability component wildcard
    constexpr auto variability_all = "variability::*";

    // =========================================================================
    // Data Quality Component (dq::)
    // =========================================================================

    // Change reasons
    constexpr auto change_reasons_read = "dq::change_reasons:read";
    constexpr auto change_reasons_write = "dq::change_reasons:write";
    constexpr auto change_reasons_delete = "dq::change_reasons:delete";

    // Change reason categories
    constexpr auto change_reason_categories_read = "dq::change_reason_categories:read";
    constexpr auto change_reason_categories_write = "dq::change_reason_categories:write";
    constexpr auto change_reason_categories_delete = "dq::change_reason_categories:delete";

    // Catalogs
    constexpr auto catalogs_read = "dq::catalogs:read";
    constexpr auto catalogs_write = "dq::catalogs:write";
    constexpr auto catalogs_delete = "dq::catalogs:delete";

    // Data domains
    constexpr auto data_domains_read = "dq::data_domains:read";
    constexpr auto data_domains_write = "dq::data_domains:write";
    constexpr auto data_domains_delete = "dq::data_domains:delete";

    // Subject areas
    constexpr auto subject_areas_read = "dq::subject_areas:read";
    constexpr auto subject_areas_write = "dq::subject_areas:write";
    constexpr auto subject_areas_delete = "dq::subject_areas:delete";

    // Datasets
    constexpr auto datasets_read = "dq::datasets:read";
    constexpr auto datasets_write = "dq::datasets:write";
    constexpr auto datasets_delete = "dq::datasets:delete";

    // Methodologies
    constexpr auto methodologies_read = "dq::methodologies:read";
    constexpr auto methodologies_write = "dq::methodologies:write";
    constexpr auto methodologies_delete = "dq::methodologies:delete";

    // Coding schemes
    constexpr auto coding_schemes_read = "dq::coding_schemes:read";
    constexpr auto coding_schemes_write = "dq::coding_schemes:write";
    constexpr auto coding_schemes_delete = "dq::coding_schemes:delete";

    // Coding scheme authority types
    constexpr auto coding_scheme_authority_types_read = "dq::coding_scheme_authority_types:read";
    constexpr auto coding_scheme_authority_types_write = "dq::coding_scheme_authority_types:write";
    constexpr auto coding_scheme_authority_types_delete = "dq::coding_scheme_authority_types:delete";

    // Nature dimensions
    constexpr auto nature_dimensions_read = "dq::nature_dimensions:read";
    constexpr auto nature_dimensions_write = "dq::nature_dimensions:write";
    constexpr auto nature_dimensions_delete = "dq::nature_dimensions:delete";

    // Origin dimensions
    constexpr auto origin_dimensions_read = "dq::origin_dimensions:read";
    constexpr auto origin_dimensions_write = "dq::origin_dimensions:write";
    constexpr auto origin_dimensions_delete = "dq::origin_dimensions:delete";

    // Treatment dimensions
    constexpr auto treatment_dimensions_read = "dq::treatment_dimensions:read";
    constexpr auto treatment_dimensions_write = "dq::treatment_dimensions:write";
    constexpr auto treatment_dimensions_delete = "dq::treatment_dimensions:delete";

    // Dataset bundles
    constexpr auto dataset_bundles_read = "dq::dataset_bundles:read";
    constexpr auto dataset_bundles_write = "dq::dataset_bundles:write";
    constexpr auto dataset_bundles_delete = "dq::dataset_bundles:delete";

    // Dataset bundle members
    constexpr auto dataset_bundle_members_read = "dq::dataset_bundle_members:read";
    constexpr auto dataset_bundle_members_write = "dq::dataset_bundle_members:write";
    constexpr auto dataset_bundle_members_delete = "dq::dataset_bundle_members:delete";

    // Data Quality component wildcard
    constexpr auto dq_all = "dq::*";

    // =========================================================================
    // Global Wildcard
    // =========================================================================

    // Wildcard - grants all permissions
    constexpr auto all = "*";
}

}

#endif
