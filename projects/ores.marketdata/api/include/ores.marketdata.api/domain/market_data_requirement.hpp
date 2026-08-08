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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_REQUIREMENT_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_DATA_REQUIREMENT_HPP

#include "ores.marketdata.api/domain/oresmd_enums.hpp"
#include <optional>
#include <string>
#include <variant>

namespace ores::marketdata::domain {

/**
 * @brief A logical, possibly-partial oresmd requirement for an FX pair.
 *
 * Every field is optional here (unlike fx_market_data_identifier, where `pair` is
 * mandatory) -- a requirement is a symbolic description that may still need resolving,
 * per the "Logical vs. physical" section of id:C3E053CA-0D4B-480B-9119-E11530160EC1.
 */
struct fx_market_data_requirement final {
    std::optional<std::string> pair;
    std::optional<instrument_type> type;

    bool operator==(const fx_market_data_requirement&) const = default;
};

/** @brief A logical, possibly-partial oresmd requirement for an interest-rate instrument. */
struct ir_market_data_requirement final {
    std::optional<std::string> ccy;
    std::optional<instrument_type> type;
    std::optional<index_family> index;
    std::optional<std::string> tenor;
    std::optional<curve_role> role;
    std::optional<domain::metric> metric;
    std::optional<domain::ir_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const ir_market_data_requirement&) const = default;
};

/** @brief A logical, possibly-partial oresmd requirement for an equity/index instrument. */
struct equity_market_data_requirement final {
    std::optional<std::string> ticker;
    std::optional<std::string> ccy;
    std::optional<instrument_type> type;
    std::optional<equity_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const equity_market_data_requirement&) const = default;
};

/** @brief A logical, possibly-partial oresmd requirement for a credit instrument. */
struct credit_market_data_requirement final {
    std::optional<std::string> reference_entity;
    std::optional<std::string> ccy;
    std::optional<instrument_type> type;
    std::optional<credit_quote_type> quote_type;
    std::optional<std::string> point;

    bool operator==(const credit_market_data_requirement&) const = default;
};

/** @brief A logical, possibly-partial oresmd requirement for a commodity instrument. */
struct commodity_market_data_requirement final {
    std::optional<std::string> commodity_code;
    std::optional<std::string> ccy;
    std::optional<instrument_type> type;
    std::optional<std::string> point;

    bool operator==(const commodity_market_data_requirement&) const = default;
};

/**
 * @brief Tagged union of the five per-asset-class requirement structs -- see
 * market_data_identifier.hpp for the rationale against a common base class.
 */
using market_data_requirement = std::variant<fx_market_data_requirement,
                                             ir_market_data_requirement,
                                             equity_market_data_requirement,
                                             credit_market_data_requirement,
                                             commodity_market_data_requirement>;

}

#endif
