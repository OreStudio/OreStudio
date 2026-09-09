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
#ifndef ORES_TRADING_API_DOMAIN_BOND_REPO_HPP
#define ORES_TRADING_API_DOMAIN_BOND_REPO_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Per-trade bond repo facts: one row per repo instrument, keyed by instrument_id.
 *
 * One row per bond repurchase agreement trade, keyed by the instrument
 * row it extends. The repo row references the issue as its collateral
 * (review answer 4): the instrument row of the trade carries the
 * issue_id. The columns fix the ER row ("repo rate and type") from the
 * repo leg (bondRepoData, instruments.xsd lines 2284-2295, one
 * legData). The repo payment dates and the leg schedule have no
 * destination in the nine tables; they land in the shared
 * instrument-keyed schedule tables of the parent story (recorded scope
 * limit, task D7943D7E wave 1.3).
 */
struct bond_repo final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the bond repo instrument this fact row extends.
     *
     * The instrument row carries the trade, workspace and party; the fact row only carries the repo
     * terms. Per the ER, no workspace column rides the fact tables.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Leg type of the repo leg (Fixed, Floating).
     *
     * The flattening mapper corrupts the coupon frequency from this value today; the reworked
     * mapper must stop.
     */
    std::string repo_type;

    /**
     * @brief Rate of the repo leg, when the leg is fixed.
     */
    double repo_rate;

    /**
     * @brief Index code of the repo leg, when the leg is floating.
     */
    std::string repo_index;

    /**
     * @brief Username of the person who last modified this bond repo.
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
 * @brief Dispatch-key identifier for bond_repo, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_repo&) {
    return "ores.trading.bond_repo";
}

}

#endif
