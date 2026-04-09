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
#ifndef ORES_TRADING_DOMAIN_SCRIPTED_INSTRUMENT_HPP
#define ORES_TRADING_DOMAIN_SCRIPTED_INSTRUMENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::trading::domain {

/**
 * @brief Scripted instrument economics for ORE AMC script-based product types.
 *
 * Discriminated by trade_type_code (ScriptedTrade, Autocallable_01,
 * DoubleDigitalOption, PerformanceOption_01). The script definition and
 * parameterisation are carried in the script_body, events_json,
 * underlyings_json, and parameters_json fields.
 */
struct scripted_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this scripted instrument.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this instrument.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief UUID of the associated trade record.
     *
     * Soft FK to ores_trading_trades_tbl. Absent for standalone instruments.
     */
    std::optional<boost::uuids::uuid> trade_id;

    /**
     * @brief ORE product type code (ScriptedTrade, Autocallable_01, etc.).
     */
    std::string trade_type_code;

    /**
     * @brief ORE script name identifying the payoff script.
     */
    std::string script_name;

    /**
     * @brief Embedded ORE AMC script body. Empty when using a library script.
     */
    std::string script_body;

    /**
     * @brief JSON array of event schedule entries. Empty when not applicable.
     */
    std::string events_json;

    /**
     * @brief JSON array of underlying codes. Empty when not applicable.
     */
    std::string underlyings_json;

    /**
     * @brief JSON object of script parameters. Empty when not applicable.
     */
    std::string parameters_json;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this record.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
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

}

#endif
