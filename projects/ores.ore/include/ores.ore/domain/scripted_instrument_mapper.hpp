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
#ifndef ORES_ORE_DOMAIN_SCRIPTED_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_SCRIPTED_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Result of a forward mapping from ORE XSD to the ORES scripted
 * instrument domain type.
 */
struct scripted_mapping_result {
    ores::trading::domain::scripted_instrument instrument;
};

/**
 * @brief Maps ORE XSD scripted trade types to ORES domain types and back.
 *
 * Handles:
 *   - ScriptedTrade           (scriptedTradeData or named product elements
 *                              such as AsianBasketOptionData)
 *
 * For named scripted products (AsianBasketOptionData, etc.) the script_name
 * field captures the product name and underlyings_json / parameters_json
 * store the key economic parameters as JSON.
 */
class scripted_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.scripted_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static scripted_mapping_result forward_scripted_trade(const trade& t);
    static scripted_mapping_result forward_double_digital_option(const trade& t);
    static scripted_mapping_result forward_performance_option_01(const trade& t);
    static scripted_mapping_result forward_knock_out_swap(const trade& t);

    static trade reverse_scripted_trade(
        const ores::trading::domain::scripted_instrument& instr);
    static trade reverse_double_digital_option(
        const ores::trading::domain::scripted_instrument& instr);
    static trade reverse_performance_option_01(
        const ores::trading::domain::scripted_instrument& instr);
    static trade reverse_knock_out_swap(
        const ores::trading::domain::scripted_instrument& instr);
};

}

#endif
