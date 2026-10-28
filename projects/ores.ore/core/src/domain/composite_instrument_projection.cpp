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
using trading::domain::composite_leg;

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

auto make_leg(std::string_view id,
              const std::optional<std::string>& type,
              int sequence) -> composite_leg {
    composite_leg leg;
    leg.identity.leg_sequence = sequence;
    leg.constituent_trade_id = id.empty() ? type.value_or("") : std::string(id);
    leg.audit.modified_by = std::string(composite_instrument_projection_actor);
    leg.audit.performed_by = std::string(composite_instrument_projection_actor);
    leg.audit.change_reason_code = std::string(composite_instrument_projection_change_reason);
    leg.audit.change_commentary = std::string(composite_instrument_projection_change_commentary);
    return leg;
}

auto composite_instrument_projection_read_leg(xml::record_reader component,
                                              std::string_view type_name,
                                              int sequence) -> composite_leg {
    return make_leg(component.text("id").value_or(""), component.spelling(type_name), sequence);
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

auto read_components(const xml::record_reader& data) -> std::vector<composite_leg> {
    std::vector<composite_leg> legs;
    const auto components = data.child("Components");
    if (!components)
        return legs;

    int sequence = 1;
    // The two kinds are separate lists. No ORE document mixes them under one
    // Components element, so reading one list after the other keeps the order
    // the document wrote.
    for (const auto& component : components->children("Trade"))
        legs.push_back(
            composite_instrument_projection_read_leg(component, "TradeType", sequence++));
    for (const auto& component : components->children("SubTrade"))
        legs.push_back(
            composite_instrument_projection_read_leg(component, "SubTradeType", sequence++));
    return legs;
}

auto write_composite_trade(xml::record_writer& trade, const composite_instrument& r) {
    auto data = trade.child("CompositeTradeData");
    // compositeTradeData requires a Currency element, and the composite
    // instrument carries no currency: the basket's constituents state their
    // own. USD is the structural placeholder the old mapper wrote.
    data.set_spelling("Currency", "USD", "currency");
    if (!r.description.empty())
        data.set_text("BasketName", r.description);
}

}

auto read_composite_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_composite_trade_type(trade_type))
        return std::nullopt;

    composite_instrument_data r{make_instrument(trade_type), {}};

    if (trade_type == "CompositeTrade") {
        if (const auto data = trade.child("CompositeTradeData")) {
            if (const auto name = data->text("BasketName"))
                r.instrument.description = *name;
            r.legs = read_components(*data);
        }
    } else {
        const auto element =
            trade_type == "TotalReturnSwap" ? "TotalReturnSwapData" : "ContractForDifferenceData";
        if (const auto data = trade.child(element)) {
            if (const auto underlying = data->child("UnderlyingData")) {
                if (const auto type = underlying_type(*underlying))
                    r.instrument.description = *type;
            }
        }
    }

    return trading::domain::trade_instrument{std::move(r)};
}

auto write_composite_instrument(xml::record_writer& trade,
                                std::string_view trade_type,
                                const trading::domain::trade_instrument& instrument,
                                const portfolio_reference_curves&) -> bool {
    const auto* r = std::get_if<composite_instrument_data>(&instrument);
    if (r == nullptr || !is_composite_trade_type(trade_type))
        return false;

    if (trade_type == "CompositeTrade")
        write_composite_trade(trade, r->instrument);
    // A total return swap and a contract for difference state an underlying and
    // a return leg, and the row holds neither: it keeps only the underlying's
    // type, which the schema cannot state without the leg beside it. Both
    // elements are optional, so the trade carries its type and envelope alone.
    return true;
}

}
