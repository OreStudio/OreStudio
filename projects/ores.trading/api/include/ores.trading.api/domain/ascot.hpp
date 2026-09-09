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
#ifndef ORES_TRADING_API_DOMAIN_ASCOT_HPP
#define ORES_TRADING_API_DOMAIN_ASCOT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Per-trade ascot facts: one row per ascot instrument, keyed by instrument_id.
 *
 * One row per asset swapped convertible option transaction (ascot), keyed
 * by the instrument row it extends. The columns fix the ER row
 * ("ascot_option_type and the conversion-option facts the XSD
 * carries") from ascotData, instruments.xsd lines 2357-2363: a
 * convertible bond plus an optionData plus a reference swap. The
 * option's single-valued scalar that the row carries is its type; the
 * reference swap is a funding leg whose terms belong to the shared
 * schedule tables of the parent story. The deliverable leaves Ascot
 * import coverage to a recorded decision in the mapping task; this row
 * carries what the ER names, no more.
 */
struct ascot final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the ascot instrument this fact row extends.
     *
     * The instrument row carries the trade, workspace and party; the fact row only carries the
     * ascot terms. Per the ER, no workspace column rides the fact tables.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Option type of the conversion option (Call, Put).
     */
    std::string ascot_option_type;

    /**
     * @brief Username of the person who last modified this ascot.
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
 * @brief Dispatch-key identifier for ascot, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ascot&) {
    return "ores.trading.ascot";
}

}

#endif
