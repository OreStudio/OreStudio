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
#ifndef ORES_ORE_CORE_DOMAIN_EQUITY_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_EQUITY_INSTRUMENT_MAPPER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.trading.api/domain/equity_accumulator_instrument.hpp"
#include "ores.trading.api/domain/equity_asian_option_instrument.hpp"
#include "ores.trading.api/domain/equity_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/equity_digital_option_instrument.hpp"
#include "ores.trading.api/domain/equity_forward_instrument.hpp"
#include "ores.trading.api/domain/equity_instrument_variant.hpp"
#include "ores.trading.api/domain/equity_option_instrument.hpp"
#include "ores.trading.api/domain/equity_position_instrument.hpp"
#include "ores.trading.api/domain/equity_swap_instrument.hpp"
#include "ores.trading.api/domain/equity_variance_swap_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Maps ORE XSD equity trade types to ORES domain types and back.
 *
 * Handles:
 *   Phase 4 (vanilla):
 *     - EquityOption
 *     - EquityForward
 *     - EquitySwap
 *     - EquityVarianceSwap
 *     - EquityBarrierOption
 *     - EquityAsianOption
 *     - EquityDigitalOption
 *     - EquityTouchOption
 *     - EquityOutperformanceOption
 *
 *   Phase 5 (exotic):
 *     - EquityAccumulator
 *     - EquityTaRF
 *     - EquityCliquetOption
 *     - EquityWorstOfBasketSwap
 */
class ORES_ORE_CORE_EXPORT equity_instrument_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.equity_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    // Helpers shared across types
    static std::string extract_underlying_name(const underlyingTypes_group_t& u);
    static std::string extract_option_type(const optionData& od);
    static std::string extract_exercise_style(const optionData& od);
    static std::string first_exercise_date(const optionData& od);
    static double extract_strike(const strikeGroup_group_t& sg);
    static std::string barrier_type_str(const barrierData& bd);
    static double first_barrier_level(const barrierData& bd);
    static double second_barrier_level(const barrierData& bd);
    static std::string underlyings_to_json(const underlyings& us);

public:
    // Phase 4 — forward
    static trading::domain::equity_instrument_variant forward_equity_option(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_forward(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_swap(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_variance_swap(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_barrier_option(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_asian_option(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_digital_option(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_touch_option(const trade& t);
    static trading::domain::equity_instrument_variant
    forward_equity_outperformance_option(const trade& t);

    // Phase 10 — forward (barrier variant extensions)
    static trading::domain::equity_instrument_variant
    forward_equity_double_barrier_option(const trade& t);
    static trading::domain::equity_instrument_variant
    forward_equity_european_barrier_option(const trade& t);

    // Phase 5 — forward
    static trading::domain::equity_instrument_variant forward_equity_accumulator(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_tarf(const trade& t);
    static trading::domain::equity_instrument_variant forward_equity_cliquet_option(const trade& t);
    static trading::domain::equity_instrument_variant
    forward_equity_worst_of_basket_swap(const trade& t);

    // Phase 4 — reverse
    static trade
    reverse_equity_option(const ores::trading::domain::equity_option_instrument& instr);
    static trade
    reverse_equity_forward(const ores::trading::domain::equity_forward_instrument& instr);
    static trade reverse_equity_swap(const ores::trading::domain::equity_swap_instrument& instr);
    static trade reverse_equity_variance_swap(
        const ores::trading::domain::equity_variance_swap_instrument& instr);
    static trade reverse_equity_barrier_option(
        const ores::trading::domain::equity_barrier_option_instrument& instr);
    static trade
    reverse_equity_asian_option(const ores::trading::domain::equity_asian_option_instrument& instr);
    static trade reverse_equity_digital_option(
        const ores::trading::domain::equity_digital_option_instrument& instr);
    static trade reverse_equity_touch_option(
        const ores::trading::domain::equity_digital_option_instrument& instr);
    static trade reverse_equity_outperformance_option(
        const ores::trading::domain::equity_option_instrument& instr);

    // Phase 10 — reverse (barrier variant extensions)
    static trade reverse_equity_double_barrier_option(
        const ores::trading::domain::equity_barrier_option_instrument& instr);
    static trade reverse_equity_european_barrier_option(
        const ores::trading::domain::equity_barrier_option_instrument& instr);

    // Phase 5 — reverse
    static trade
    reverse_equity_accumulator(const ores::trading::domain::equity_accumulator_instrument& instr);
    static trade
    reverse_equity_tarf(const ores::trading::domain::equity_accumulator_instrument& instr);
    static trade
    reverse_equity_cliquet_option(const ores::trading::domain::equity_option_instrument& instr);
    static trade
    reverse_equity_worst_of_basket_swap(const ores::trading::domain::equity_swap_instrument& instr);
};


}

#endif
