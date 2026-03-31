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
#ifndef ORES_ORE_DOMAIN_COMPOSITE_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_COMPOSITE_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Result of a forward mapping from ORE XSD to the ORES composite
 * instrument domain type.
 */
struct composite_mapping_result {
    ores::trading::domain::composite_instrument instrument;
};

/**
 * @brief Maps ORE XSD composite trade types to ORES domain types and back.
 *
 * Handles:
 *   - CompositeTrade         (compositeTradeData)
 *   - MultiLegOption         (multiLegOptionData)
 *   - TotalReturnSwap        (totalReturnSwapData)
 *   - ContractForDifference  (totalReturnSwapData via ContractForDifferenceData)
 */
class composite_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.composite_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static composite_mapping_result forward_composite_trade(const trade& t);
    static composite_mapping_result forward_multi_leg_option(const trade& t);
    static composite_mapping_result forward_total_return_swap(const trade& t);
    static composite_mapping_result forward_contract_for_difference(
        const trade& t);

    static trade reverse_composite_trade(
        const ores::trading::domain::composite_instrument& instr);
    static trade reverse_multi_leg_option(
        const ores::trading::domain::composite_instrument& instr);
    static trade reverse_total_return_swap(
        const ores::trading::domain::composite_instrument& instr);
    static trade reverse_contract_for_difference(
        const ores::trading::domain::composite_instrument& instr);
};

}

#endif
