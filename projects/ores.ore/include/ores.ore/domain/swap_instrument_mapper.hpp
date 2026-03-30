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
#ifndef ORES_ORE_DOMAIN_SWAP_INSTRUMENT_MAPPER_HPP
#define ORES_ORE_DOMAIN_SWAP_INSTRUMENT_MAPPER_HPP

#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"

namespace ores::ore::domain {

/**
 * @brief Result of a forward mapping from ORE XSD to ORES domain types.
 */
struct swap_mapping_result {
    ores::trading::domain::instrument instrument;
    std::vector<ores::trading::domain::swap_leg> legs;
};

/**
 * @brief Maps ORE XSD swap/FRA/CapFloor trade types to ORES domain types and
 * back.
 *
 * Handles the following ORE trade types:
 *   - Swap (SwapData, CrossCurrencySwapData, InflationSwapData)
 *   - ForwardRateAgreement (ForwardRateAgreementData)
 *   - CapFloor (CapFloorData)
 *   - Swaption (SwaptionData — European and Bermudan)
 *   - CallableSwap (CallableSwapData)
 *   - FlexiSwap (FlexiSwapData — forward-only; domain has no tranche fields)
 *   - BalanceGuaranteedSwap (BalanceGuaranteedSwapData — forward-only)
 *
 * Forward mapping (ORE XSD → ORES domain) captures the economic fields that
 * the ORES relational model stores. Fields not yet modelled in ORES are
 * silently dropped; the coverage gap is reported by the Python coverage check
 * (Thing 2) and the mapper round-trip test (Thing 3).
 *
 * Reverse mapping (ORES domain → ORE XSD) reconstructs ORE types from the
 * ORES domain. The reconstruction is complete only for the fields captured by
 * the forward mapping.
 */
class swap_instrument_mapper {
private:
    inline static std::string_view logger_name =
        "ores.ore.domain.swap_instrument_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Forward-maps a Swap trade (SwapData) to ORES domain types.
     *
     * Also handles CrossCurrencySwapData and InflationSwapData which share the
     * same swapData XSD type.
     */
    static swap_mapping_result forward_swap(const trade& t);

    /**
     * @brief Forward-maps a ForwardRateAgreement trade to ORES domain types.
     */
    static swap_mapping_result forward_fra(const trade& t);

    /**
     * @brief Forward-maps a CapFloor trade to ORES domain types.
     */
    static swap_mapping_result forward_capfloor(const trade& t);

    /**
     * @brief Reverse-maps ORES domain types back to a Swap ORE XSD trade.
     */
    static trade reverse_swap(
        const ores::trading::domain::instrument& instr,
        const std::vector<ores::trading::domain::swap_leg>& legs);

    /**
     * @brief Reverse-maps ORES domain types back to a ForwardRateAgreement
     * ORE XSD trade.
     */
    static trade reverse_fra(
        const ores::trading::domain::instrument& instr,
        const std::vector<ores::trading::domain::swap_leg>& legs);

    /**
     * @brief Reverse-maps ORES domain types back to a CapFloor ORE XSD trade.
     */
    static trade reverse_capfloor(
        const ores::trading::domain::instrument& instr,
        const std::vector<ores::trading::domain::swap_leg>& legs);

    /**
     * @brief Forward-maps a Swaption trade (SwaptionData) to ORES domain types.
     *
     * Maps the first exercise date to instrument.start_date, leg data to
     * swap_legs, and the option style to instrument.description.
     */
    static swap_mapping_result forward_swaption(const trade& t);

    /**
     * @brief Reverse-maps ORES domain types back to a Swaption ORE XSD trade.
     */
    static trade reverse_swaption(
        const ores::trading::domain::instrument& instr,
        const std::vector<ores::trading::domain::swap_leg>& legs);

    /**
     * @brief Forward-maps a CallableSwap trade (CallableSwapData) to ORES
     * domain types.
     *
     * Exercise dates are serialised as a JSON array in
     * instrument.callable_dates_json.
     */
    static swap_mapping_result forward_callable_swap(const trade& t);

    /**
     * @brief Reverse-maps ORES domain types back to a CallableSwap ORE XSD
     * trade.
     */
    static trade reverse_callable_swap(
        const ores::trading::domain::instrument& instr,
        const std::vector<ores::trading::domain::swap_leg>& legs);

    /**
     * @brief Forward-maps a FlexiSwap trade (FlexiSwapData) to ORES domain
     * types.
     *
     * Only leg economics are captured. LowerNotionalBounds and Prepayment
     * schedule are not stored in the current domain model — these are coverage
     * gaps reported by the Python gap check.
     */
    static swap_mapping_result forward_flexi_swap(const trade& t);

    /**
     * @brief Forward-maps a BalanceGuaranteedSwap trade to ORES domain types.
     *
     * Only leg economics are captured. Tranche structure and ReferenceSecurity
     * are not stored in the current domain model.
     */
    static swap_mapping_result forward_balance_guaranteed_swap(const trade& t);

private:
    static ores::trading::domain::swap_leg map_leg(
        const legData& ld, int leg_number);

    static legData reverse_leg(
        const ores::trading::domain::swap_leg& sl,
        const ores::trading::domain::instrument& instr);

    static legData_Notionals_t make_notionals(double notional);
};

}

#endif
