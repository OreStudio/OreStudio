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
#ifndef ORES_REFDATA_API_DOMAIN_PORTFOLIO_HPP
#define ORES_REFDATA_API_DOMAIN_PORTFOLIO_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Logical aggregation node for risk and reporting.
 *
 * Represents organizational, risk, or reporting groupings.
 * Never holds trades directly. Supports hierarchical structure
 * via self-referencing parent_portfolio_id.
 */
struct portfolio final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief UUID uniquely identifying this portfolio.
     *
     * Surrogate key for the portfolio record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this portfolio.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Human-readable name for the portfolio.
     *
     * e.g., 'Global Rates', 'APAC Credit'.
     */
    std::string name;

    /**
     * @brief Optional free-text description of the portfolio.
     */
    std::string description;

    /**
     * @brief Self-referencing FK. NULL indicates a root node.
     *
     * Links to the parent portfolio in the hierarchy.
     */
    std::optional<boost::uuids::uuid> parent_portfolio_id;

    /**
     * @brief Business unit responsible for management.
     *
     * References the business_units table.
     */
    std::optional<boost::uuids::uuid> owner_unit_id;

    /**
     * @brief Portfolio purpose classification.
     *
     * References purpose_types lookup (Risk, Regulatory, ClientReporting, Internal). Defaults to
     * Risk so a freshly-constructed portfolio (the Add dialog, before the Purpose Type combo's
     * async populate completes) always carries a value the FK-validation trigger accepts.
     */
    std::string purpose_type = "Risk";

    /**
     * @brief Currency for P&L/risk aggregation at this node.
     *
     * ISO 4217 currency code.
     */
    std::string aggregation_ccy;

    /**
     * @brief If 1, node is purely for on-demand reporting.
     *
     * Not persisted in trade attribution when virtual.
     */
    int is_virtual = 0;

    /**
     * @brief Current lifecycle status (Active, Inactive, Closed).
     */
    std::string status;

    /**
     * @brief Username of the person who last modified this portfolio.
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
 * @brief Dispatch-key identifier for portfolio, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const portfolio&) {
    return "ores.refdata.portfolio";
}

}

#endif
