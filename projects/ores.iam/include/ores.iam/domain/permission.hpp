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

namespace ores::iam::domain {

/**
 * @brief Represents an atomic permission that can be granted to roles.
 *
 * Permissions follow a hierarchical naming convention using colons as
 * separators: "resource:action" (e.g., "accounts:create", "currencies:read").
 * The special code "*" represents all permissions (superuser).
 *
 * @note This type does not include change tracking fields (change_reason_code,
 * change_commentary) because permissions are system-defined constants populated
 * from bootstrap data. They are not user-editable and do not require audit
 * trails. Roles, which reference permissions, are user-manageable and do have
 * change tracking.
 */
struct permission final {
    /**
     * @brief Unique identifier for the permission.
     */
    boost::uuids::uuid id;

    /**
     * @brief Permission code following the format "resource:action".
     *
     * Examples: "accounts:create", "currencies:read", "flags:update".
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
 */
namespace permissions {
    // Account management
    constexpr auto accounts_create = "accounts:create";
    constexpr auto accounts_read = "accounts:read";
    constexpr auto accounts_update = "accounts:update";
    constexpr auto accounts_delete = "accounts:delete";
    constexpr auto accounts_lock = "accounts:lock";
    constexpr auto accounts_unlock = "accounts:unlock";
    constexpr auto accounts_reset_password = "accounts:reset_password";

    // Currency management
    constexpr auto currencies_create = "currencies:create";
    constexpr auto currencies_read = "currencies:read";
    constexpr auto currencies_update = "currencies:update";
    constexpr auto currencies_delete = "currencies:delete";
    constexpr auto currencies_history = "currencies:history";

    // Feature flags management
    constexpr auto flags_create = "flags:create";
    constexpr auto flags_read = "flags:read";
    constexpr auto flags_update = "flags:update";
    constexpr auto flags_delete = "flags:delete";

    // Login info (read-only audit data)
    constexpr auto login_info_read = "login_info:read";

    // Roles management
    constexpr auto roles_create = "roles:create";
    constexpr auto roles_read = "roles:read";
    constexpr auto roles_update = "roles:update";
    constexpr auto roles_delete = "roles:delete";
    constexpr auto roles_assign = "roles:assign";
    constexpr auto roles_revoke = "roles:revoke";

    // Data Quality - Change reasons
    constexpr auto change_reasons_read = "change_reasons:read";
    constexpr auto change_reasons_write = "change_reasons:write";
    constexpr auto change_reasons_delete = "change_reasons:delete";

    // Data Quality - Change reason categories
    constexpr auto change_reason_categories_read = "change_reason_categories:read";
    constexpr auto change_reason_categories_write = "change_reason_categories:write";
    constexpr auto change_reason_categories_delete = "change_reason_categories:delete";

    // Data Quality - Catalogs
    constexpr auto catalogs_read = "catalogs:read";
    constexpr auto catalogs_write = "catalogs:write";
    constexpr auto catalogs_delete = "catalogs:delete";

    // Data Quality - Data domains
    constexpr auto data_domains_read = "data_domains:read";
    constexpr auto data_domains_write = "data_domains:write";
    constexpr auto data_domains_delete = "data_domains:delete";

    // Data Quality - Subject areas
    constexpr auto subject_areas_read = "subject_areas:read";
    constexpr auto subject_areas_write = "subject_areas:write";
    constexpr auto subject_areas_delete = "subject_areas:delete";

    // Data Quality - Datasets
    constexpr auto datasets_read = "datasets:read";
    constexpr auto datasets_write = "datasets:write";
    constexpr auto datasets_delete = "datasets:delete";

    // Data Quality - Methodologies
    constexpr auto methodologies_read = "methodologies:read";
    constexpr auto methodologies_write = "methodologies:write";
    constexpr auto methodologies_delete = "methodologies:delete";

    // Data Quality - Coding schemes
    constexpr auto coding_schemes_read = "coding_schemes:read";
    constexpr auto coding_schemes_write = "coding_schemes:write";
    constexpr auto coding_schemes_delete = "coding_schemes:delete";

    // Data Quality - Coding scheme authority types
    constexpr auto coding_scheme_authority_types_read = "coding_scheme_authority_types:read";
    constexpr auto coding_scheme_authority_types_write = "coding_scheme_authority_types:write";
    constexpr auto coding_scheme_authority_types_delete = "coding_scheme_authority_types:delete";

    // Data Quality - Nature dimensions
    constexpr auto nature_dimensions_read = "nature_dimensions:read";
    constexpr auto nature_dimensions_write = "nature_dimensions:write";
    constexpr auto nature_dimensions_delete = "nature_dimensions:delete";

    // Data Quality - Origin dimensions
    constexpr auto origin_dimensions_read = "origin_dimensions:read";
    constexpr auto origin_dimensions_write = "origin_dimensions:write";
    constexpr auto origin_dimensions_delete = "origin_dimensions:delete";

    // Data Quality - Treatment dimensions
    constexpr auto treatment_dimensions_read = "treatment_dimensions:read";
    constexpr auto treatment_dimensions_write = "treatment_dimensions:write";
    constexpr auto treatment_dimensions_delete = "treatment_dimensions:delete";

    // Data Quality - Dataset bundles
    constexpr auto dataset_bundles_read = "dataset_bundles:read";
    constexpr auto dataset_bundles_write = "dataset_bundles:write";
    constexpr auto dataset_bundles_delete = "dataset_bundles:delete";

    // Data Quality - Dataset bundle members
    constexpr auto dataset_bundle_members_read = "dataset_bundle_members:read";
    constexpr auto dataset_bundle_members_write = "dataset_bundle_members:write";
    constexpr auto dataset_bundle_members_delete = "dataset_bundle_members:delete";

    // Wildcard - grants all permissions
    constexpr auto all = "*";
}

}

#endif
