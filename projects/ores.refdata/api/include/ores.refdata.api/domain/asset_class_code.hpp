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
#ifndef ORES_REFDATA_API_DOMAIN_ASSET_CLASS_CODE_HPP
#define ORES_REFDATA_API_DOMAIN_ASSET_CLASS_CODE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Top-level asset class classification (fx, rates, credit, equity, commodity, inflation,
 * bond, cross_asset).
 *
 * General-purpose classification of the top-level asset class a market
 * series, instrument, or curve belongs to. Mirrors
 * ores::marketdata::domain::asset_class (a plain, non-persisted C++
 * enum used for market-series filtering) as a managed, persisted
 * refdata code table, so other entities (instrument_code, and
 * anything else needing to classify by asset class) can FK-validate
 * against it instead of duplicating a hardcoded string list. Managed by
 * the system tenant, like other shared code tables.
 */
struct asset_class_code final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique asset class code.
     *
     * Examples: 'fx', 'rates', 'credit', 'equity', 'commodity', 'inflation', 'bond', 'cross_asset'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the asset class.
     */
    std::string name;

    /**
     * @brief Detailed description of the asset class.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this asset class code.
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
 * @brief Dispatch-key identifier for asset_class_code, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const asset_class_code&) {
    return "ores.refdata.asset_class_code";
}

}

#endif
