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

#include <variant>
#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/fx_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument.hpp"
#include "ores.trading.api/domain/fx_digital_option_instrument.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument.hpp"
#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"

namespace ores::ore::domain {

/**
 * @brief Variant holding one of the seven per-type FX instrument domain objects.
 */
using fx_instrument_variant = std::variant<
    ores::trading::domain::fx_forward_instrument,
    ores::trading::domain::fx_vanilla_option_instrument,
    ores::trading::domain::fx_barrier_option_instrument,
    ores::trading::domain::fx_digital_option_instrument,
    ores::trading::domain::fx_asian_forward_instrument,
    ores::trading::domain::fx_accumulator_instrument,
    ores::trading::domain::fx_variance_swap_instrument
>;

/**
 * @brief Result of a forward mapping from ORE XSD to the ORES FX domain type.
 */
struct fx_mapping_result {
    fx_instrument_variant instrument;
};

/**
 * @brief Maps ORE XSD FX trade types to ORES domain types and back.
 *
 * Handles:
 *   - FxForward              → fx_forward_instrument
 *   - FxSwap                 → fx_forward_instrument (near leg; far leg is a gap)
 *   - FxOption               → fx_vanilla_option_instrument
 *   - FxBarrierOption        → fx_barrier_option_instrument
 *   - FxDoubleBarrierOption  → fx_barrier_option_instrument
 *   - FxEuropeanBarrierOption → fx_barrier_option_instrument
 *   - FxKIKOBarrierOption    → fx_barrier_option_instrument
 *   - FxGenericBarrierOption → fx_barrier_option_instrument
 *   - FxDigitalOption        → fx_digital_option_instrument
 *   - FxDigitalBarrierOption → fx_digital_option_instrument
 *   - FxTouchOption          → fx_digital_option_instrument
 *   - FxDoubleTouchOption    → fx_digital_option_instrument
 *   - FxAverageForward       → fx_asian_forward_instrument
 *   - FxTaRF                 → fx_asian_forward_instrument
 *   - FxAccumulator          → fx_accumulator_instrument
 *   - FxVarianceSwap         → fx_variance_swap_instrument
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
    // Forward mappings (ORE XSD → typed domain object)
    static fx_mapping_result forward_fx_forward(const trade& t);
    static fx_mapping_result forward_fx_swap(const trade& t);
    static fx_mapping_result forward_fx_option(const trade& t);
    static fx_mapping_result forward_fx_barrier_option(const trade& t);
    static fx_mapping_result forward_fx_double_barrier_option(const trade& t);
    static fx_mapping_result forward_fx_european_barrier_option(const trade& t);
    static fx_mapping_result forward_fx_kiko_barrier_option(const trade& t);
    static fx_mapping_result forward_fx_generic_barrier_option(const trade& t);
    static fx_mapping_result forward_fx_digital_option(const trade& t);
    static fx_mapping_result forward_fx_digital_barrier_option(const trade& t);
    static fx_mapping_result forward_fx_touch_option(const trade& t);
    static fx_mapping_result forward_fx_variance_swap(const trade& t);
    static fx_mapping_result forward_fx_average_forward(const trade& t);
    static fx_mapping_result forward_fx_accumulator(const trade& t);
    static fx_mapping_result forward_fx_tarf(const trade& t);

    // Reverse mappings (typed domain object → ORE XSD)
    static trade reverse_fx_forward(
        const ores::trading::domain::fx_forward_instrument& instr);
    static trade reverse_fx_swap(
        const ores::trading::domain::fx_forward_instrument& instr);
    static trade reverse_fx_option(
        const ores::trading::domain::fx_vanilla_option_instrument& instr);
    static trade reverse_fx_barrier_option(
        const ores::trading::domain::fx_barrier_option_instrument& instr);
    static trade reverse_fx_double_barrier_option(
        const ores::trading::domain::fx_barrier_option_instrument& instr);
    static trade reverse_fx_european_barrier_option(
        const ores::trading::domain::fx_barrier_option_instrument& instr);
    static trade reverse_fx_kiko_barrier_option(
        const ores::trading::domain::fx_barrier_option_instrument& instr);
    static trade reverse_fx_generic_barrier_option(
        const ores::trading::domain::fx_barrier_option_instrument& instr);
    static trade reverse_fx_digital_option(
        const ores::trading::domain::fx_digital_option_instrument& instr);
    static trade reverse_fx_digital_barrier_option(
        const ores::trading::domain::fx_digital_option_instrument& instr);
    static trade reverse_fx_touch_option(
        const ores::trading::domain::fx_digital_option_instrument& instr);
    static trade reverse_fx_variance_swap(
        const ores::trading::domain::fx_variance_swap_instrument& instr);
    static trade reverse_fx_average_forward(
        const ores::trading::domain::fx_asian_forward_instrument& instr);
    static trade reverse_fx_accumulator(
        const ores::trading::domain::fx_accumulator_instrument& instr);
    static trade reverse_fx_tarf(
        const ores::trading::domain::fx_asian_forward_instrument& instr);

private:
    static barrierData make_barrier(const std::string& type, double level);
};

}

#endif
