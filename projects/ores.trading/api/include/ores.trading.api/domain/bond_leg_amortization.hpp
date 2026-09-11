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
#ifndef ORES_TRADING_API_DOMAIN_BOND_LEG_AMORTIZATION_HPP
#define ORES_TRADING_API_DOMAIN_BOND_LEG_AMORTIZATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One step of a bond leg's amortization schedule, keyed to the leg and the step's ordinal.
 *
 * One row per step of a bond leg's amortization schedule, keyed to the
 * leg and the step's ordinal.
 *
 * A step is not a bare amount. The schema states a type that says how
 * the step amortizes, a value, the window it applies over, a frequency
 * and an underflow flag, and the container mirrors all six. The step
 * therefore has a table of its own rather than a role in the leg's
 * number list.
 *
 * The type is required and the other five members are optional, so the
 * type is not null here and the rest are nullable.
 */
struct bond_leg_amortization final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the instrument this amortization belongs to.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Which leg list of the instrument the leg holding this step belongs to: bond,
     * trs_funding, repo or ascot_swap.
     */
    std::string leg_role;

    /**
     * @brief Ordinal of the leg within its list, counting from one.
     */
    int leg_number;

    /**
     * @brief Ordinal of this step within the leg's amortization list.
     */
    int sequence_number;

    /**
     * @brief How the step amortizes, as the document spells it.
     */
    std::string amortization_type;

    /**
     * @brief The amount the step amortizes by.
     */
    std::optional<double> value;

    /**
     * @brief First date of the step's window (ISO 8601 date string).
     */
    std::optional<std::string> start_date;

    /**
     * @brief Last date of the step's window (ISO 8601 date string).
     */
    std::optional<std::string> end_date;

    /**
     * @brief How often the step's amount applies within its window.
     */
    std::optional<std::string> frequency;

    /**
     * @brief True when the step allows the notional to fall below zero.
     */
    std::optional<bool> underflow;

    /**
     * @brief Username of the person who last modified this bond leg amortization.
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
 * @brief Dispatch-key identifier for bond_leg_amortization, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_leg_amortization&) {
    return "ores.trading.bond_leg_amortization";
}

}

#endif
