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
#ifndef ORES_TRADING_API_DOMAIN_BOND_ISSUE_CONVERSION_TARGET_HPP
#define ORES_TRADING_API_DOMAIN_BOND_ISSUE_CONVERSION_TARGET_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One conversion target of a convertible bond issue, keyed to the issue.
 *
 * One row per equity share a convertible bond issue converts into,
 * family-owned by the issue, with the conversion ratio as the row
 * weight (ER row). The share the doc names is the target of
 * cbConversionData (instruments.xsd lines 2981-3045). A convertible
 * whose conversion ratio changes over dated steps has no home in the
 * nine tables; one row per dated ratio is a recorded scope limit for
 * the mapping task (task D7943D7E wave 1.3), as are the fixed-amount,
 * mandatory, contingent and exchangeable conversion variants.
 */
struct bond_issue_conversion_target final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the issue this conversion target belongs to.
     *
     * The issue row carries the workspace; the child rows are family-owned and ride the issue's
     * scope, so no workspace column rides them.
     */
    boost::uuids::uuid issue_id;

    /**
     * @brief Ordinal of this conversion target within the issue's target list.
     */
    int sequence_number;

    /**
     * @brief Identifier of the equity share the issue converts into, as the document names it.
     */
    std::string underlying_id;

    /**
     * @brief Number of shares per bond face value at conversion. The row weight.
     */
    double conversion_ratio = 0.0;

    /**
     * @brief Username of the person who last modified this bond issue conversion target.
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
 * @brief Dispatch-key identifier for bond_issue_conversion_target, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_issue_conversion_target&) {
    return "ores.trading.bond_issue_conversion_target";
}

}

#endif
