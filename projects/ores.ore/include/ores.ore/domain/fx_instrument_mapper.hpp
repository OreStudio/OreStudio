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
#ifndef ORES_ORE_DOMAIN_FX_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_FX_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/fx_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Result of a forward mapping from ORE XSD to the ORES FX domain type.
 */
struct fx_mapping_result {
    ores::trading::domain::fx_instrument instrument;
};

/**
 * @brief Maps ORE XSD FX trade types to ORES domain types and back.
 *
 * Handles:
 *   - FxForward  (FxForwardData)
 *   - FxSwap     (FxSwapData)   — near leg mapped; far amounts noted as gap
 *   - FxOption   (FxOptionData) — vanilla European/American only
 *
 * Forward mapping (ORE XSD → ORES domain) captures economic fields stored
 * in the ORES relational model. Fields not yet modelled are silently dropped;
 * the coverage gap is reported by ore_coverage_check.py (Thing 2).
 *
 * Reverse mapping (ORES domain → ORE XSD) reconstructs ORE types from ORES
 * domain for the fields captured by the forward mapping.
 */
class fx_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.fx_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Forward-maps a FxForward trade to ORES domain types.
     */
    static fx_mapping_result forward_fx_forward(const trade& t);

    /**
     * @brief Forward-maps a FxSwap trade to ORES domain types.
     *
     * Maps the near leg. Far leg amounts are a known coverage gap.
     */
    static fx_mapping_result forward_fx_swap(const trade& t);

    /**
     * @brief Forward-maps a FxOption trade to ORES domain types.
     */
    static fx_mapping_result forward_fx_option(const trade& t);

    /**
     * @brief Reverse-maps ORES domain types back to a FxForward ORE XSD trade.
     */
    static trade reverse_fx_forward(
        const ores::trading::domain::fx_instrument& instr);

    /**
     * @brief Reverse-maps ORES domain types back to a FxSwap ORE XSD trade.
     */
    static trade reverse_fx_swap(
        const ores::trading::domain::fx_instrument& instr);

    /**
     * @brief Reverse-maps ORES domain types back to a FxOption ORE XSD trade.
     */
    static trade reverse_fx_option(
        const ores::trading::domain::fx_instrument& instr);
};

}

#endif
