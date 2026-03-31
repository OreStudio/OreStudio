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
#include "ores.ore/domain/trade_mapper.hpp"

#include <boost/uuid/nil_generator.hpp>
#include "ores.trading.api/domain/trade_json_io.hpp" // IWYU pragma: keep.
#include "ores.ore/domain/swap_instrument_mapper.hpp"
#include "ores.ore/domain/fx_instrument_mapper.hpp"
#include "ores.ore/domain/bond_instrument_mapper.hpp"
#include "ores.ore/domain/credit_instrument_mapper.hpp"
#include "ores.ore/domain/equity_instrument_mapper.hpp"

namespace ores::ore::domain {

using namespace ores::logging;

trading::domain::trade trade_mapper::map(const trade& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML trade: " << std::string(v.id);

    const auto nil = boost::uuids::nil_uuid();
    trading::domain::trade r;
    r.id = nil;
    r.party_id = nil;
    r.external_id = std::string(v.id);
    r.trade_type = to_string(v.TradeType);

    // Book, portfolio and counterparty require external mapping context.
    r.book_id = nil;
    r.portfolio_id = nil;

    // Extract envelope fields where available.
    if (v.Envelope) {
        if (v.Envelope->CounterParty) {
            // Store the counterparty name as the netting set ID fallback
            // context. The actual counterparty_id UUID must be resolved by the
            // calling code via a mapping dialog or lookup.
        }
        if (v.Envelope->nettingSetGroup) {
            if (v.Envelope->nettingSetGroup->NettingSetId) {
                r.netting_set_id =
                    std::string(*v.Envelope->nettingSetGroup->NettingSetId);
            }
        }
    }

    r.activity_type_code = "new_booking";
    r.status_id = boost::uuids::nil_uuid();
    r.modified_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML portfolio";

    BOOST_LOG_SEV(lg(), trace) << "Mapped trade. Result: " << r;
    return r;
}

std::vector<trading::domain::trade> trade_mapper::map(const portfolio& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML portfolio. Total trades: "
                               << v.Trade.size();

    std::vector<trading::domain::trade> r;
    r.reserve(v.Trade.size());
    std::ranges::transform(v.Trade, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });

    BOOST_LOG_SEV(lg(), trace) << "Mapped portfolio trades.";
    return r;
}

std::optional<swap_mapping_result>
trade_mapper::map_swap_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "Swap" || type == "CrossCurrencySwap" ||
            type == "InflationSwap")
        return swap_instrument_mapper::forward_swap(v);
    if (type == "ForwardRateAgreement")
        return swap_instrument_mapper::forward_fra(v);
    if (type == "CapFloor")
        return swap_instrument_mapper::forward_capfloor(v);
    if (type == "Swaption")
        return swap_instrument_mapper::forward_swaption(v);
    if (type == "CallableSwap")
        return swap_instrument_mapper::forward_callable_swap(v);
    if (type == "FlexiSwap")
        return swap_instrument_mapper::forward_flexi_swap(v);
    if (type == "BalanceGuaranteedSwap")
        return swap_instrument_mapper::forward_balance_guaranteed_swap(v);
    return std::nullopt;
}

std::optional<fx_mapping_result>
trade_mapper::map_fx_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "FxForward")
        return fx_instrument_mapper::forward_fx_forward(v);
    if (type == "FxSwap")
        return fx_instrument_mapper::forward_fx_swap(v);
    if (type == "FxOption")
        return fx_instrument_mapper::forward_fx_option(v);
    return std::nullopt;
}

std::optional<bond_mapping_result>
trade_mapper::map_bond_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "Bond")
        return bond_instrument_mapper::forward_bond(v);
    if (type == "ForwardBond")
        return bond_instrument_mapper::forward_forward_bond(v);
    if (type == "CallableBond")
        return bond_instrument_mapper::forward_callable_bond(v);
    if (type == "ConvertibleBond")
        return bond_instrument_mapper::forward_convertible_bond(v);
    if (type == "BondOption")
        return bond_instrument_mapper::forward_bond_option(v);
    if (type == "BondTRS")
        return bond_instrument_mapper::forward_bond_trs(v);
    return std::nullopt;
}

std::optional<credit_mapping_result>
trade_mapper::map_credit_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "CreditDefaultSwap")
        return credit_instrument_mapper::forward_cds(v);
    if (type == "IndexCreditDefaultSwap")
        return credit_instrument_mapper::forward_index_cds(v);
    if (type == "IndexCreditDefaultSwapOption")
        return credit_instrument_mapper::forward_index_cds_option(v);
    if (type == "CreditLinkedSwap")
        return credit_instrument_mapper::forward_credit_linked_swap(v);
    if (type == "SyntheticCDO")
        return credit_instrument_mapper::forward_synthetic_cdo(v);
    if (type == "RiskParticipationAgreement")
        return credit_instrument_mapper::forward_rpa(v);
    return std::nullopt;
}

std::optional<equity_mapping_result>
trade_mapper::map_equity_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "EquityOption")
        return equity_instrument_mapper::forward_equity_option(v);
    if (type == "EquityForward")
        return equity_instrument_mapper::forward_equity_forward(v);
    if (type == "EquitySwap")
        return equity_instrument_mapper::forward_equity_swap(v);
    if (type == "EquityVarianceSwap")
        return equity_instrument_mapper::forward_equity_variance_swap(v);
    if (type == "EquityBarrierOption")
        return equity_instrument_mapper::forward_equity_barrier_option(v);
    if (type == "EquityAsianOption")
        return equity_instrument_mapper::forward_equity_asian_option(v);
    if (type == "EquityDigitalOption")
        return equity_instrument_mapper::forward_equity_digital_option(v);
    if (type == "EquityTouchOption")
        return equity_instrument_mapper::forward_equity_touch_option(v);
    if (type == "EquityOutperformanceOption")
        return equity_instrument_mapper::forward_equity_outperformance_option(v);
    if (type == "EquityAccumulator")
        return equity_instrument_mapper::forward_equity_accumulator(v);
    if (type == "EquityTaRF")
        return equity_instrument_mapper::forward_equity_tarf(v);
    if (type == "EquityCliquetOption")
        return equity_instrument_mapper::forward_equity_cliquet_option(v);
    if (type == "EquityWorstOfBasketSwap")
        return equity_instrument_mapper::forward_equity_worst_of_basket_swap(v);
    return std::nullopt;
}

instrument_mapping_result trade_mapper::map_instrument(const trade& v) {
    if (auto r = map_swap_instrument(v))   return *r;
    if (auto r = map_fx_instrument(v))     return *r;
    if (auto r = map_bond_instrument(v))   return *r;
    if (auto r = map_credit_instrument(v)) return *r;
    if (auto r = map_equity_instrument(v)) return *r;
    return std::monostate{};
}

}
