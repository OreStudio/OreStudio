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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_API_DOMAIN_PERMISSION_HPP
#define ORES_IAM_API_DOMAIN_PERMISSION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief An atomic permission that can be granted to roles.
 *
 * An atomic permission that can be granted to roles. Permissions follow the
 * hierarchical naming convention component::resource:action (for example
 * iam::accounts:create; "*" grants everything and component::* grants
 * every action within one component).
 *
 * The table is temporal (see
 * projects/ores.sql/create/iam/iam_permissions_create.sql): it carries
 * valid_from/valid_to, the GIST exclusion and the delete rule, but it has
 * no version column and no audit tail -- permissions are system-defined
 * constants seeded from bootstrap data, not user-editable records, so they
 * need no change tracking. The :no_audit_columns: flag in the * SQL **
 * Flags drawer selects exactly that shape: it drops the version column and
 * the four audit columns while keeping the transaction-time window. The
 * :skip_uuid_check: suppression on id drops the nil-UUID check the
 * hand-written table never had.
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
     * Examples: "iam::accounts:create", "refdata::currencies:read". Use "*" for the wildcard that
     * grants every permission, and "component::*" for every permission within one component.
     */
    std::string code;

    /**
     * @brief Human-readable description of what this permission allows.
     */
    std::string description;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for permission, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const permission&) {
    return "ores.iam.permission";
}

}

#endif
