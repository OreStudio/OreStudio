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
#ifndef ORES_DQ_API_DOMAIN_CATALOG_HPP
#define ORES_DQ_API_DOMAIN_CATALOG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Named collection of related datasets.
 *
 * A catalog is a named collection of related datasets, providing a
 * high-level grouping mechanism for datasets that share a common theme,
 * source, or purpose. Examples: "ISO Standards" (ISO 3166 countries,
 * ISO 4217 currencies), "Cryptocurrency" (crypto reference data and
 * icons). Catalogs are optional for datasets but provide useful
 * organizational structure for managing large numbers of them. Rows are
 * authored directly (not mirrored from an external source).
 */
struct catalog final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique name identifying this catalog.
     *
     * Examples: "ISO Standards", "Cryptocurrency", "FpML Standards".
     */
    std::string name;

    /**
     * @brief Human-readable description of the catalog's purpose.
     */
    std::string description;

    /**
     * @brief Optional owner or responsible party for this catalog.
     */
    std::optional<std::string> owner;

    /**
     * @brief Username of the person who last modified this catalog.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Dispatch-key identifier for catalog, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const catalog&) {
    return "ores.dq.catalog";
}

}

#endif
