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
#ifndef ORES_ORE_DOMAIN_COMMODITY_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_COMMODITY_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Result of a forward mapping from ORE XSD to the ORES commodity
 * domain type.
 */
struct commodity_mapping_result {
    ores::trading::domain::commodity_instrument instrument;
};

/**
 * @brief Maps ORE XSD commodity trade types to ORES domain types and back.
 *
 * Handles:
 *   - CommodityForward        (commodityForwardData)
 *   - CommodityOption         (commodityOptionData)
 *   - CommoditySwap           (swapData — commodity legs)
 *   - CommoditySwaption       (commoditySwaptionData)
 *   - CommodityVarianceSwap   (varianceSwapData — shared with equity)
 *   - CommodityAveragePriceOption (commodityAveragePriceOptionData)
 *   - CommodityOptionStrip    (commodityOptionStripData)
 *
 * Forward mapping captures economic fields stored in the ORES relational
 * model. Reverse mapping reconstructs ORE XSD types from ORES domain fields.
 */
class commodity_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.commodity_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    // Forward mappings
    static commodity_mapping_result forward_commodity_forward(const trade& t);
    static commodity_mapping_result forward_commodity_option(const trade& t);
    static commodity_mapping_result forward_commodity_swap(const trade& t);
    static commodity_mapping_result forward_commodity_swaption(const trade& t);
    static commodity_mapping_result forward_commodity_variance_swap(
        const trade& t);
    static commodity_mapping_result forward_commodity_apo(const trade& t);
    static commodity_mapping_result forward_commodity_option_strip(
        const trade& t);

    // Reverse mappings
    static trade reverse_commodity_forward(
        const ores::trading::domain::commodity_instrument& instr);
    static trade reverse_commodity_option(
        const ores::trading::domain::commodity_instrument& instr);
    static trade reverse_commodity_swap(
        const ores::trading::domain::commodity_instrument& instr);
    static trade reverse_commodity_swaption(
        const ores::trading::domain::commodity_instrument& instr);
    static trade reverse_commodity_variance_swap(
        const ores::trading::domain::commodity_instrument& instr);
    static trade reverse_commodity_apo(
        const ores::trading::domain::commodity_instrument& instr);
    static trade reverse_commodity_option_strip(
        const ores::trading::domain::commodity_instrument& instr);
};

}

#endif
