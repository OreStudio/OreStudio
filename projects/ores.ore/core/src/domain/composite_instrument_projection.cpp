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
#include "ores.ore.core/domain/leg_block_projection.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

using trading::domain::composite_instrument;
using trading::domain::composite_instrument_data;

/// The actor and reason every imported row states.
constexpr std::string_view composite_instrument_projection_actor = "ores";
constexpr std::string_view composite_instrument_projection_change_reason =
    "system.external_data_import";
constexpr std::string_view composite_instrument_projection_change_commentary =
    "Imported from ORE XML";

/**
 * @brief The trade types the family states, which are the ones the old
 * dispatcher took.
 */
constexpr bool is_composite_trade_type(std::string_view type) {
    return type == "CompositeTrade" || type == "TotalReturnSwap" || type == "ContractForDifference";
}

auto make_instrument(std::string_view trade_type_code) -> composite_instrument {
    composite_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = std::string(composite_instrument_projection_actor);
    r.audit.performed_by = std::string(composite_instrument_projection_actor);
    r.audit.change_reason_code = std::string(composite_instrument_projection_change_reason);
    r.audit.change_commentary = std::string(composite_instrument_projection_change_commentary);
    return r;
}

/**
 * @brief The first row of the substitution group an underlying basket states.
 *
 * The generator flattened each substitution group into one member per kind, so
 * a group's order across the kinds is not recorded. A document states one kind
 * under a given element, so reading the kinds in sequence finds the one the
 * document wrote.
 */
auto underlying_type(const xml::record_reader& underlying) -> std::optional<std::string> {
    if (const auto trades = underlying.children("Trade"); !trades.empty())
        return trades.front().spelling("TradeType");
    if (const auto sub_trades = underlying.children("SubTrade"); !sub_trades.empty())
        return sub_trades.front().spelling("SubTradeType");
    if (const auto derivatives = underlying.children("Derivative"); !derivatives.empty()) {
        const auto& derivative = derivatives.front();
        if (const auto trade = derivative.child("Trade"))
            return trade->spelling("TradeType");
        if (const auto sub_trade = derivative.child("SubTrade"))
            return sub_trade->spelling("SubTradeType");
    }
    return std::nullopt;
}

/// Text a member states, or nothing.
auto text_if(const xml::record_reader& r, std::string_view name) -> std::optional<std::string> {
    return r.has(name) ? r.text(name) : std::nullopt;
}

auto spelling_if(const xml::record_reader& r, std::string_view name) -> std::optional<std::string> {
    return r.has(name) ? r.spelling(name) : std::nullopt;
}

/**
 * @brief Reads a total return swap's or a contract for difference's own
 * terms: the return leg, the funding legs and a portfolio index underlying.
 *
 * The underlying trades are trades of their own, which the portfolio
 * projection maps and joins; this reads what the swap states about them.
 */
void read_total_return(composite_instrument_data& r, const xml::record_reader& data) {
    auto& row = r.instrument;
    if (const auto underlying = data.child("UnderlyingData")) {
        if (const auto indices = underlying->children("PortfolioIndexTradeData");
            !indices.empty()) {
            const auto& index = indices.front();
            row.underlying_basket_name = text_if(index, "BasketName");
            row.underlying_index_quantity = text_if(index, "IndexQuantity");
            row.underlying_price_is_per_unit = text_if(index, "PriceIsPerUnit");
        }
    }
    if (const auto ret = data.child("ReturnData")) {
        row.return_payer = text_if(*ret, "Payer");
        row.return_currency = spelling_if(*ret, "Currency");
        row.return_observation_lag = text_if(*ret, "ObservationLag");
        row.return_observation_convention = spelling_if(*ret, "ObservationConvention");
        row.return_observation_calendar = text_if(*ret, "ObservationCalendar");
        row.return_payment_lag = text_if(*ret, "PaymentLag");
        row.return_payment_convention = spelling_if(*ret, "PaymentConvention");
        row.return_payment_calendar = text_if(*ret, "PaymentCalendar");
        row.return_initial_price = ret->decimal("InitialPrice");
        row.return_initial_price_currency = text_if(*ret, "InitialPriceCurrency");
        row.return_pay_underlying_cash_flows_immediately =
            text_if(*ret, "PayUnderlyingCashFlowsImmediately");
        if (const auto terms = ret->child("FXTerms")) {
            std::string indices;
            for (const auto& index : terms->values("FXIndex"))
                indices += (indices.empty() ? "" : " ") + index;
            if (!indices.empty())
                row.return_fx_index = indices;
        }
        if (const auto schedule = ret->child("ScheduleData"))
            option_block::read_schedule(r, *schedule, "trs", 1, "return_schedule");
        if (const auto dates = ret->child("PaymentDates"))
            option_block::read_date_list(
                r, *dates, "PaymentDate", "trs", 1, "return_payment_dates");
    }
    if (const auto funding = data.child("FundingData")) {
        std::string types;
        for (const auto& type : funding->spellings("NotionalType"))
            types += (types.empty() ? "" : " ") + type;
        if (!types.empty())
            row.funding_notional_types = types;
        leg_block::append_legs(r, *funding);
    }
}

void write_total_return(xml::record_writer& trade,
                        std::string_view element,
                        const composite_instrument_data& v) {
    const auto& row = v.instrument;
    // A row stored before the swap's terms were kept states none, and the
    // schema requires a return leg, so such a row writes no data element.
    if (!row.return_currency)
        return;
    auto data = trade.child(element);
    if (row.underlying_basket_name || row.underlying_index_quantity ||
        row.underlying_price_is_per_unit) {
        auto index = data.child("UnderlyingData").append("PortfolioIndexTradeData");
        if (row.underlying_basket_name)
            index.set_text("BasketName", *row.underlying_basket_name);
        if (row.underlying_index_quantity)
            index.set_text("IndexQuantity", *row.underlying_index_quantity);
        if (row.underlying_price_is_per_unit)
            index.set_text("PriceIsPerUnit", *row.underlying_price_is_per_unit);
    }
    auto ret = data.child("ReturnData");
    const auto text = [](xml::record_writer& w, std::string_view name, const auto& value) {
        if (value)
            w.set_text(name, *value);
    };
    const auto spelled = [](xml::record_writer& w, std::string_view name, const auto& value) {
        if (value)
            w.set_spelling(name, *value, name);
    };
    text(ret, "Payer", row.return_payer);
    spelled(ret, "Currency", row.return_currency);
    option_block::write_stored_schedule(ret.child("ScheduleData"), v, "trs", 1, "return_schedule");
    text(ret, "ObservationLag", row.return_observation_lag);
    spelled(ret, "ObservationConvention", row.return_observation_convention);
    text(ret, "ObservationCalendar", row.return_observation_calendar);
    text(ret, "PaymentLag", row.return_payment_lag);
    spelled(ret, "PaymentConvention", row.return_payment_convention);
    text(ret, "PaymentCalendar", row.return_payment_calendar);
    option_block::write_date_list(
        ret, "PaymentDates", "PaymentDate", v, "trs", 1, "return_payment_dates");
    if (row.return_initial_price)
        ret.set_decimal("InitialPrice", *row.return_initial_price);
    text(ret, "InitialPriceCurrency", row.return_initial_price_currency);
    if (row.return_fx_index) {
        auto terms = ret.child("FXTerms");
        std::string_view indices = *row.return_fx_index;
        while (!indices.empty()) {
            const auto space = indices.find(' ');
            terms.append_value("FXIndex", indices.substr(0, space));
            if (space == std::string_view::npos)
                break;
            indices.remove_prefix(space + 1);
        }
    }
    text(
        ret, "PayUnderlyingCashFlowsImmediately", row.return_pay_underlying_cash_flows_immediately);

    if (!v.legs.empty() || row.funding_notional_types) {
        auto funding = data.child("FundingData");
        if (row.funding_notional_types) {
            std::string_view types = *row.funding_notional_types;
            while (!types.empty()) {
                const auto space = types.find(' ');
                funding.append_spelling("NotionalType", types.substr(0, space), "notional type");
                if (space == std::string_view::npos)
                    break;
                types.remove_prefix(space + 1);
            }
        }
        leg_block::write_legs(funding, std::nullopt, std::nullopt, v);
    }
}

auto write_composite_trade(xml::record_writer& trade, const composite_instrument& r) {
    auto data = trade.child("CompositeTradeData");
    // The schema requires a currency. A row stored before the composite kept
    // its own states none, and USD is the placeholder the old mapper wrote.
    data.set_spelling("Currency", r.currency.value_or("USD"), "currency");
    if (r.notional_calculation)
        data.set_spelling("NotionalCalculation", *r.notional_calculation, "notional calculation");
    if (r.notional_override)
        data.set_decimal("NotionalOverride", *r.notional_override);
    if (r.portfolio_basket)
        data.set_text("PortfolioBasket", *r.portfolio_basket);
    if (!r.description.empty())
        data.set_text("BasketName", r.description);
    if (r.index_quantity)
        data.set_decimal("IndexQuantity", *r.index_quantity);
}

}

auto read_composite_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_composite_trade_type(trade_type))
        return std::nullopt;

    composite_instrument_data r{make_instrument(trade_type)};

    if (trade_type == "CompositeTrade") {
        if (const auto data = trade.child("CompositeTradeData")) {
            auto& row = r.instrument;
            if (const auto name = data->text("BasketName"))
                row.description = *name;
            row.currency = data->spelling("Currency");
            row.notional_calculation = data->spelling("NotionalCalculation");
            row.notional_override = data->decimal("NotionalOverride");
            row.portfolio_basket = data->text("PortfolioBasket");
            row.index_quantity = data->decimal("IndexQuantity");
        }
    } else {
        const auto element =
            trade_type == "TotalReturnSwap" ? "TotalReturnSwapData" : "ContractForDifferenceData";
        if (const auto data = trade.child(element)) {
            if (const auto underlying = data->child("UnderlyingData")) {
                if (const auto type = underlying_type(*underlying))
                    r.instrument.description = *type;
            }
            read_total_return(r, *data);
        }
    }

    return trading::domain::trade_instrument{std::move(r)};
}

auto write_composite_instrument(xml::record_writer& trade,
                                std::string_view trade_type,
                                const trading::domain::trade_instrument& instrument,
                                const ores::marketdata::projection::ore_projection&) -> bool {
    const auto* r = std::get_if<composite_instrument_data>(&instrument);
    if (r == nullptr || !is_composite_trade_type(trade_type))
        return false;

    if (trade_type == "CompositeTrade")
        write_composite_trade(trade, r->instrument);
    else
        write_total_return(trade,
                           trade_type == "TotalReturnSwap" ? "TotalReturnSwapData" :
                                                             "ContractForDifferenceData",
                           *r);
    return true;
}

}
