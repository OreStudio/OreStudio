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
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.ore.core/domain/bond_instrument_mapper.hpp"
#include "ores.ore.core/domain/commodity_instrument_mapper.hpp"
#include "ores.ore.core/domain/composite_instrument_mapper.hpp"
#include "ores.ore.core/domain/credit_instrument_mapper.hpp"
#include "ores.ore.core/domain/equity_instrument_mapper.hpp"
#include "ores.ore.core/domain/fx_instrument_mapper.hpp"
#include "ores.ore.core/domain/scripted_instrument_mapper.hpp"
#include "ores.ore.core/domain/swap_instrument_mapper.hpp"
#include "ores.trading.api/domain/trade_json_io.hpp" // IWYU pragma: keep.
#include <boost/uuid/nil_generator.hpp>

namespace ores::ore::domain {

using namespace ores::logging;

trading::domain::trade trade_mapper::map(const trade& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML trade: " << std::string(v.id);

    const auto nil = boost::uuids::nil_uuid();
    trading::domain::trade r;
    r.identity.id = nil;
    r.identity.party_id = nil;
    r.identity.external_id = std::string(v.id);
    r.classification.trade_type = to_string(v.TradeType);

    // Book, portfolio and counterparty require external mapping context.
    r.parties.book_id = nil;
    r.parties.portfolio_id = nil;

    // The netting set id projects from the envelope, which stays the
    // carrier of record: it holds the element's presence as well as its
    // text, and the column holds only the text.
    const auto envelope = map_envelope(v);
    if (envelope)
        r.classification.netting_set_id = envelope->netting_set_id.value_or(std::string());

    r.classification.activity_type_code = "new_booking";
    r.classification.status_id = boost::uuids::nil_uuid();
    r.audit.modified_by = "ores";
    r.audit.change_reason_code = "system.external_data_import";
    r.audit.change_commentary = "Imported from ORE XML portfolio";

    BOOST_LOG_SEV(lg(), trace) << "Mapped trade. Result: " << r;
    return r;
}

std::vector<trading::domain::trade> trade_mapper::map(const portfolio& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML portfolio. Total trades: " << v.Trade.size();

    std::vector<trading::domain::trade> r;
    r.reserve(v.Trade.size());
    std::ranges::transform(v.Trade, std::back_inserter(r), [](const auto& ve) { return map(ve); });

    BOOST_LOG_SEV(lg(), trace) << "Mapped portfolio trades.";
    return r;
}

std::optional<trading::domain::swap_instrument_data>
trade_mapper::map_swap_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "Swap" || type == "CrossCurrencySwap")
        return swap_instrument_mapper::forward_swap(v);
    if (type == "InflationSwap")
        return swap_instrument_mapper::forward_inflation_swap(v);
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

std::optional<trading::domain::fx_instrument_variant>
trade_mapper::map_fx_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "FxForward")
        return fx_instrument_mapper::forward_fx_forward(v);
    if (type == "FxSwap")
        return fx_instrument_mapper::forward_fx_swap(v);
    if (type == "FxOption")
        return fx_instrument_mapper::forward_fx_option(v);
    if (type == "FxBarrierOption")
        return fx_instrument_mapper::forward_fx_barrier_option(v);
    if (type == "FxDigitalOption")
        return fx_instrument_mapper::forward_fx_digital_option(v);
    if (type == "FxDigitalBarrierOption")
        return fx_instrument_mapper::forward_fx_digital_barrier_option(v);
    if (type == "FxTouchOption" || type == "FxDoubleTouchOption")
        return fx_instrument_mapper::forward_fx_touch_option(v);
    if (type == "FxVarianceSwap")
        return fx_instrument_mapper::forward_fx_variance_swap(v);
    if (type == "FxAverageForward")
        return fx_instrument_mapper::forward_fx_average_forward(v);
    if (type == "FxAccumulator")
        return fx_instrument_mapper::forward_fx_accumulator(v);
    if (type == "FxTaRF")
        return fx_instrument_mapper::forward_fx_tarf(v);
    if (type == "FxGenericBarrierOption")
        return fx_instrument_mapper::forward_fx_generic_barrier_option(v);
    if (type == "FxDoubleBarrierOption")
        return fx_instrument_mapper::forward_fx_double_barrier_option(v);
    if (type == "FxEuropeanBarrierOption")
        return fx_instrument_mapper::forward_fx_european_barrier_option(v);
    if (type == "FxKIKOBarrierOption")
        return fx_instrument_mapper::forward_fx_kiko_barrier_option(v);
    return std::nullopt;
}

std::optional<trading::domain::bond_instrument_data>
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
    if (type == "BondRepo")
        return bond_instrument_mapper::forward_bond_repo(v);
    if (type == "BondFuture")
        return bond_instrument_mapper::forward_bond_future(v);
    if (type == "Ascot")
        return bond_instrument_mapper::forward_ascot(v);
    return std::nullopt;
}

std::optional<trading::domain::credit_instrument>
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

std::optional<trading::domain::equity_instrument_variant>
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
    if (type == "EquityDoubleBarrierOption")
        return equity_instrument_mapper::forward_equity_double_barrier_option(v);
    if (type == "EquityEuropeanBarrierOption")
        return equity_instrument_mapper::forward_equity_european_barrier_option(v);
    return std::nullopt;
}

std::optional<trading::domain::commodity_instrument>
trade_mapper::map_commodity_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "CommodityForward")
        return commodity_instrument_mapper::forward_commodity_forward(v);
    if (type == "CommodityOption")
        return commodity_instrument_mapper::forward_commodity_option(v);
    if (type == "CommoditySwap")
        return commodity_instrument_mapper::forward_commodity_swap(v);
    if (type == "CommoditySwaption")
        return commodity_instrument_mapper::forward_commodity_swaption(v);
    if (type == "CommodityVarianceSwap")
        return commodity_instrument_mapper::forward_commodity_variance_swap(v);
    if (type == "CommodityAveragePriceOption")
        return commodity_instrument_mapper::forward_commodity_apo(v);
    if (type == "CommodityOptionStrip")
        return commodity_instrument_mapper::forward_commodity_option_strip(v);
    return std::nullopt;
}

std::optional<trading::domain::scripted_instrument>
trade_mapper::map_scripted_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "ScriptedTrade")
        return scripted_instrument_mapper::forward_scripted_trade(v);
    if (type == "DoubleDigitalOption")
        return scripted_instrument_mapper::forward_double_digital_option(v);
    if (type == "PerformanceOption_01")
        return scripted_instrument_mapper::forward_performance_option_01(v);
    if (type == "KnockOutSwap")
        return scripted_instrument_mapper::forward_knock_out_swap(v);
    return std::nullopt;
}

std::optional<trading::domain::composite_instrument_data>
trade_mapper::map_composite_instrument(const trade& v) {
    const std::string type = to_string(v.TradeType);
    if (type == "CompositeTrade")
        return composite_instrument_mapper::forward_composite_trade(v);
    if (type == "TotalReturnSwap")
        return composite_instrument_mapper::forward_total_return_swap(v);
    if (type == "ContractForDifference")
        return composite_instrument_mapper::forward_contract_for_difference(v);
    return std::nullopt;
}

std::optional<trading::domain::trade_envelope_data> trade_mapper::map_envelope(const trade& v) {
    if (!v.Envelope)
        return std::nullopt;

    trading::domain::trade_envelope_data r;
    const auto& e = *v.Envelope;

    if (e.CounterParty)
        r.counter_party = std::string(*e.CounterParty);

    if (e.nettingSetGroup && e.nettingSetGroup->NettingSetId)
        r.netting_set_id = std::string(*e.nettingSetGroup->NettingSetId);

    if (e.PortfolioIds) {
        std::vector<std::string> ids;
        ids.reserve(e.PortfolioIds->PortfolioId.size());
        for (const auto& id : e.PortfolioIds->PortfolioId)
            ids.emplace_back(id);
        r.portfolio_ids = std::move(ids);
    }

    if (e.AdditionalFields) {
        std::vector<trading::domain::trade_envelope_field> fields;
        fields.reserve(e.AdditionalFields->other_elements.size());
        for (const auto& f : e.AdditionalFields->other_elements)
            fields.push_back({f.name, f.value});
        r.additional_fields = std::move(fields);
    }

    return r;
}

envelope trade_mapper::reverse_envelope(const trading::domain::trade_envelope_data& v) {
    envelope r;

    if (v.counter_party) {
        domain::envelope_CounterParty_t cp;
        static_cast<std::string&>(cp) = *v.counter_party;
        r.CounterParty = cp;
    }

    if (v.netting_set_id) {
        domain::_NettingSetId_t nsid;
        static_cast<std::string&>(nsid) = *v.netting_set_id;
        domain::nettingSetGroup_group_t nsg;
        nsg.NettingSetId = nsid;
        r.nettingSetGroup = nsg;
    }

    if (v.portfolio_ids) {
        domain::envelope_PortfolioIds_t ids;
        for (const auto& id : *v.portfolio_ids) {
            domain::envelope_PortfolioIds_t_PortfolioId_t pid;
            static_cast<std::string&>(pid) = id;
            ids.PortfolioId.push_back(std::move(pid));
        }
        r.PortfolioIds = std::move(ids);
    }

    if (v.additional_fields) {
        domain::envelope_AdditionalFields_t fields;
        for (const auto& f : *v.additional_fields)
            fields.other_elements.push_back(xsd::any_element{f.name, f.value});
        r.AdditionalFields = std::move(fields);
    }

    return r;
}

trading::domain::trade_instrument trade_mapper::map_instrument(const trade& v) {
    if (auto r = map_swap_instrument(v))
        return *r;
    if (auto r = map_fx_instrument(v))
        return *r;
    if (auto r = map_bond_instrument(v))
        return *r;
    if (auto r = map_credit_instrument(v))
        return *r;
    if (auto r = map_equity_instrument(v))
        return *r;
    if (auto r = map_commodity_instrument(v))
        return *r;
    if (auto r = map_scripted_instrument(v))
        return *r;
    if (auto r = map_composite_instrument(v))
        return *r;
    return std::monostate{};
}

}
