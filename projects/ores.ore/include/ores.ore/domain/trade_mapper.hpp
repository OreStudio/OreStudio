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
#ifndef ORES_ORE_DOMAIN_TRADE_MAPPER_HPP
#define ORES_ORE_DOMAIN_TRADE_MAPPER_HPP

#include <variant>
#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.ore/domain/domain.hpp"
#include "ores.ore/domain/swap_instrument_mapper.hpp"
#include "ores.ore/domain/fx_instrument_mapper.hpp"
#include "ores.ore/domain/bond_instrument_mapper.hpp"
#include "ores.ore/domain/credit_instrument_mapper.hpp"
#include "ores.ore/domain/equity_instrument_mapper.hpp"
#include "ores.ore/domain/commodity_instrument_mapper.hpp"
#include "ores.ore/domain/scripted_instrument_mapper.hpp"
#include "ores.ore/domain/composite_instrument_mapper.hpp"

namespace ores::ore::domain {

/**
 * @brief Discriminated union of all possible instrument mapping results.
 *
 * std::monostate represents an unmapped trade (exotic, scripted, or a type
 * not yet covered). Each subsequent instrument phase appends a new member.
 */
using instrument_mapping_result = std::variant<
    std::monostate,       ///< unmapped / not yet supported
    swap_mapping_result,
    fx_mapping_result,
    bond_mapping_result,
    credit_mapping_result,
    equity_mapping_result,
    commodity_mapping_result,
    scripted_mapping_result,
    composite_mapping_result
>;

/**
 * @brief Maps ORE XML trade types to trading domain entities.
 *
 * Performs a partial mapping from the ORE XSD portfolio/trade structure to the
 * ORES trading domain. Fields that require external context to resolve (e.g.
 * book_id, counterparty_id, portfolio_id) are left as nil UUIDs and must be
 * populated by the calling code (typically a mapping dialog or import
 * configuration).
 *
 * Fields mapped directly:
 * - trade.id -> external_id
 * - trade.TradeType -> trade_type (enum to string)
 * - Envelope.NettingSetId -> netting_set_id
 * - lifecycle_event set to "New"
 * - Audit fields (modified_by, change_reason_code, change_commentary)
 *
 * Fields left as nil (require external mapping):
 * - book_id (ORE has no book concept)
 * - portfolio_id (ORE PortfolioIds are string labels, not ORES UUIDs)
 * - counterparty_id (ORE CounterParty is a string name, not an ORES UUID)
 * - party_id (derived from book_id in ORES)
 */
class trade_mapper {
private:
    inline static std::string_view logger_name = "ores.ore.domain.trade_mapper";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Maps a single ORE XSD trade to an ORES trading domain trade.
     */
    static trading::domain::trade map(const trade& v);

    /**
     * @brief Maps an ORE portfolio (collection of trades) to a vector of ORES
     * trading domain trades.
     */
    static std::vector<trading::domain::trade> map(const portfolio& v);

    /**
     * @brief Dispatches a swap-family trade to swap_instrument_mapper.
     *
     * Returns a populated result for Swap, CrossCurrencySwap, InflationSwap,
     * ForwardRateAgreement, and CapFloor. Returns empty for all other types.
     */
    static std::optional<swap_mapping_result>
    map_swap_instrument(const trade& v);

    /**
     * @brief Dispatches an FX-family trade to fx_instrument_mapper.
     *
     * Returns a populated result for FxForward, FxSwap, and FxOption.
     * Returns empty for all other types.
     */
    static std::optional<fx_mapping_result>
    map_fx_instrument(const trade& v);

    /**
     * @brief Dispatches a credit-family trade to credit_instrument_mapper.
     *
     * Returns a populated result for CreditDefaultSwap, IndexCreditDefaultSwap,
     * IndexCreditDefaultSwapOption, CreditLinkedSwap, SyntheticCDO, and
     * RiskParticipationAgreement. Returns empty for all other types.
     */
    static std::optional<credit_mapping_result>
    map_credit_instrument(const trade& v);

    /**
     * @brief Dispatches a bond-family trade to bond_instrument_mapper.
     *
     * Returns a populated result for Bond, ForwardBond, CallableBond,
     * and ConvertibleBond. Returns empty for all other types.
     */
    static std::optional<bond_mapping_result>
    map_bond_instrument(const trade& v);

    /**
     * @brief Dispatches an equity-family trade to equity_instrument_mapper.
     *
     * Returns a populated result for EquityOption, EquityForward, EquitySwap,
     * EquityVarianceSwap, EquityBarrierOption, EquityAsianOption,
     * EquityDigitalOption, EquityTouchOption, EquityOutperformanceOption,
     * EquityAccumulator, EquityTaRF, EquityCliquetOption,
     * and EquityWorstOfBasketSwap. Returns empty for all other types.
     */
    static std::optional<equity_mapping_result>
    map_equity_instrument(const trade& v);

    /**
     * @brief Dispatches a commodity-family trade to commodity_instrument_mapper.
     */
    static std::optional<commodity_mapping_result>
    map_commodity_instrument(const trade& v);

    /**
     * @brief Dispatches a scripted trade to scripted_instrument_mapper.
     */
    static std::optional<scripted_mapping_result>
    map_scripted_instrument(const trade& v);

    /**
     * @brief Dispatches a composite trade to composite_instrument_mapper.
     */
    static std::optional<composite_mapping_result>
    map_composite_instrument(const trade& v);

    /**
     * @brief Unified instrument dispatch: maps any supported ORE trade to its
     * instrument_mapping_result.
     *
     * Tries swap, FX, bond, credit, equity, commodity, scripted, composite in
     * order. Returns std::monostate for any unknown type.
     * Callers should use std::visit to handle the result.
     */
    static instrument_mapping_result map_instrument(const trade& v);
};

}

#endif
