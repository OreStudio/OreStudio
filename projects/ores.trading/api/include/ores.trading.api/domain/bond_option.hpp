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
#ifndef ORES_TRADING_API_DOMAIN_BOND_OPTION_HPP
#define ORES_TRADING_API_DOMAIN_BOND_OPTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Per-trade bond option facts: one row per option instrument, keyed by instrument_id.
 *
 * One row per bond option trade, keyed by the instrument row it
 * extends. The ER row names option_type and option_strike, which map
 * from optionData and the strikeGroup of bondOptionData
 * (instruments.xsd lines 2273-2282).
 *
 * Three members of bondOptionData sit beside its option block rather
 * than inside it: the redemption code, the price type and the knock-out
 * flag. The shared option element states none of them, so an Ascot never
 * writes them and no other table can hold them. They ride here, on the
 * one row that is a bond option.
 *
 * The rest of the block lands elsewhere. The option element's own
 * members go to instrument_option and its keyed children, and the
 * exercise dates land as schedule rows in the shared instrument-keyed
 * schedule tables, under the owner role option.
 */
struct bond_option final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the bond option instrument this fact row extends.
     *
     * The instrument row carries the trade, workspace and party; the fact row only carries the
     * option terms. Per the ER, no workspace column rides the fact tables.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Call or Put.
     */
    std::string option_type;

    /**
     * @brief Strike price of the option. Non-negative.
     */
    double option_strike = 0.0;

    /**
     * @brief The redemption code the document states.
     */
    std::optional<std::string> redemption;

    /**
     * @brief The price type the document states.
     */
    std::optional<std::string> price_type;

    /**
     * @brief The document's spelling of the knock-out flag.
     *
     * The schema types this member as its own bool, which enumerates thirteen spellings including
     * the empty one. The column holds the spelling the document chose rather than a decoded
     * boolean, so export re-emits the same text. The corpus states false, so a decoded column would
     * lose the spelling on every document that carries one.
     */
    std::optional<std::string> knocks_out;

    /**
     * @brief Username of the person who last modified this bond option.
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
 * @brief Dispatch-key identifier for bond_option, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_option&) {
    return "ores.trading.bond_option";
}

}

#endif
