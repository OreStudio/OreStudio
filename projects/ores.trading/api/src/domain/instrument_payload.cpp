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
#include "ores.trading.api/domain/instrument_payload.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <optional>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <string_view>
#include <utility>
#include <variant>

namespace ores::trading::domain {
namespace {

/**
 * The leaf name carried on the wire. It is the plain C++ type name, so it does
 * not depend on how a compiler spells a qualified name.
 */
template <typename Leaf>
constexpr std::string_view leaf_name() {
#define ORES_TRADING_LEAF_NAME(T)          \
    if constexpr (std::is_same_v<Leaf, T>) \
        return #T;
    ORES_TRADING_LEAF_NAME(fra_instrument)
    ORES_TRADING_LEAF_NAME(vanilla_swap_instrument)
    ORES_TRADING_LEAF_NAME(cap_floor_instrument)
    ORES_TRADING_LEAF_NAME(swaption_instrument)
    ORES_TRADING_LEAF_NAME(balance_guaranteed_swap_instrument)
    ORES_TRADING_LEAF_NAME(callable_swap_instrument)
    ORES_TRADING_LEAF_NAME(knock_out_swap_instrument)
    ORES_TRADING_LEAF_NAME(inflation_swap_instrument)
    ORES_TRADING_LEAF_NAME(rpa_instrument)
    ORES_TRADING_LEAF_NAME(fx_forward_instrument)
    ORES_TRADING_LEAF_NAME(fx_vanilla_option_instrument)
    ORES_TRADING_LEAF_NAME(fx_barrier_option_instrument)
    ORES_TRADING_LEAF_NAME(fx_digital_option_instrument)
    ORES_TRADING_LEAF_NAME(fx_asian_forward_instrument)
    ORES_TRADING_LEAF_NAME(fx_accumulator_instrument)
    ORES_TRADING_LEAF_NAME(fx_variance_swap_instrument)
    ORES_TRADING_LEAF_NAME(equity_option_instrument)
    ORES_TRADING_LEAF_NAME(equity_digital_option_instrument)
    ORES_TRADING_LEAF_NAME(equity_barrier_option_instrument)
    ORES_TRADING_LEAF_NAME(equity_asian_option_instrument)
    ORES_TRADING_LEAF_NAME(equity_forward_instrument)
    ORES_TRADING_LEAF_NAME(equity_variance_swap_instrument)
    ORES_TRADING_LEAF_NAME(equity_swap_instrument)
    ORES_TRADING_LEAF_NAME(equity_accumulator_instrument)
    ORES_TRADING_LEAF_NAME(equity_position_instrument)
    ORES_TRADING_LEAF_NAME(bond_instrument_data)
    ORES_TRADING_LEAF_NAME(credit_instrument)
    ORES_TRADING_LEAF_NAME(commodity_instrument)
    ORES_TRADING_LEAF_NAME(composite_instrument)
    ORES_TRADING_LEAF_NAME(scripted_instrument)
#undef ORES_TRADING_LEAF_NAME
    return {};
}

/**
 * A leg-less leaf: the leaf is written on its own.
 */
template <typename Leaf>
void encode_leaf(const Leaf& leaf, instrument_payload& out) {
    out.type = std::string(leaf_name<Leaf>());
    out.body = rfl::json::write(leaf);
}

/**
 * A leaf inside its family variant: the leaf is written on its own, so no
 * variant reaches the wire.
 */
template <typename Variant>
void encode_flat(const Variant& variant, instrument_payload& out) {
    std::visit([&](const auto& leaf) { encode_leaf(leaf, out); }, variant);
}

/**
 * A leaf inside a leg-carrying family: the leaf and its legs are written as one
 * struct, which is the family struct with the variant removed.
 */
template <typename Leaf, typename Leg>
void encode_leaf_with_legs(const Leaf& leaf,
                           const std::vector<Leg>& legs,
                           instrument_payload& out) {
    out.type = std::string(leaf_name<Leaf>());
    out.body = rfl::json::write(with_legs<Leaf, Leg>{leaf, legs});
}

/**
 * The wire shape of a rates leaf: the leaf, its legs and its call date
 * schedule. It is the family struct with the variant removed, so a reader
 * rebuilds the family carrier from it without knowing the leaf's type.
 */
template <typename Leaf>
struct swap_leaf_payload {
    Leaf instrument;
    std::vector<swap_leg> legs;
    std::vector<callable_swap_call_date> call_dates;
};

template <typename Leaf>
void encode_swap_leaf(const Leaf& leaf,
                      const std::vector<swap_leg>& legs,
                      const std::vector<callable_swap_call_date>& call_dates,
                      instrument_payload& out) {
    out.type = std::string(leaf_name<Leaf>());
    out.body = rfl::json::write(swap_leaf_payload<Leaf>{leaf, legs, call_dates});
}

template <typename Variant>
void encode_swap(const Variant& variant,
                 const std::vector<swap_leg>& legs,
                 const std::vector<callable_swap_call_date>& call_dates,
                 instrument_payload& out) {
    std::visit([&](const auto& leaf) { encode_swap_leaf(leaf, legs, call_dates, out); }, variant);
}

template <typename T>
std::optional<T> read_as(const instrument_payload& payload) {
    if (auto r = rfl::json::read<T>(payload.body))
        return std::move(*r);
    return std::nullopt;
}

template <typename Variant, std::size_t I>
void try_one_flat(const instrument_payload& payload, std::optional<Variant>& found) {
    using Leaf = std::variant_alternative_t<I, Variant>;
    if (found || payload.type != leaf_name<Leaf>())
        return;
    if (auto r = read_as<Leaf>(payload))
        found = Variant(std::move(*r));
}

template <typename Variant, std::size_t... Is>
void try_each_flat(const instrument_payload& payload,
                   std::optional<Variant>& found,
                   std::index_sequence<Is...>) {
    (try_one_flat<Variant, Is>(payload, found), ...);
}

template <typename Variant>
std::optional<Variant> decode_flat(const instrument_payload& payload) {
    std::optional<Variant> found;
    try_each_flat<Variant>(
        payload, found, std::make_index_sequence<std::variant_size_v<Variant>>{});
    return found;
}

template <typename Leaf, typename Leg>
std::optional<with_legs<Leaf, Leg>> decode_leaf_with_legs(const instrument_payload& payload) {
    if (payload.type != leaf_name<Leaf>())
        return std::nullopt;
    return read_as<with_legs<Leaf, Leg>>(payload);
}

/**
 * The generic-lambda fold expression this replaces segfaults clang 19 and
 * AppleClang 16 inside TransformCXXFoldExpr, so the trial is spelled as the
 * same recursive helper pair decode_flat uses.
 */
template <typename Variant, std::size_t I>
void try_one_swap(const instrument_payload& payload,
                  std::optional<swap_instrument_data>& found) {
    using Leaf = std::variant_alternative_t<I, Variant>;
    if (found || payload.type != leaf_name<Leaf>())
        return;
    if (auto r = read_as<swap_leaf_payload<Leaf>>(payload))
        found = swap_instrument_data{Variant(std::move(r->instrument)),
                                     std::move(r->legs),
                                     std::move(r->call_dates)};
}

template <typename Variant, std::size_t... Is>
void try_each_swap(const instrument_payload& payload,
                   std::optional<swap_instrument_data>& found,
                   std::index_sequence<Is...>) {
    (try_one_swap<Variant, Is>(payload, found), ...);
}

template <typename Variant>
std::optional<swap_instrument_data> decode_swap(const instrument_payload& payload) {
    std::optional<swap_instrument_data> found;
    try_each_swap<Variant>(payload, found, std::make_index_sequence<std::variant_size_v<Variant>>{});
    return found;
}

/**
 * The wire shape of a commodity instrument with its basket: the instrument and
 * the constituents it owns, so a basket survives the payload.
 */
struct commodity_leaf_payload {
    commodity_instrument instrument;
    std::vector<commodity_basket_constituent> constituents;
};

std::optional<commodity_instrument_data> decode_commodity(const instrument_payload& payload) {
    if (payload.type != leaf_name<commodity_instrument>())
        return std::nullopt;
    if (auto r = read_as<commodity_leaf_payload>(payload))
        return commodity_instrument_data{std::move(r->instrument), std::move(r->constituents)};
    // A payload written before the basket became child rows carries the bare
    // instrument, so read it and leave the collection empty.
    if (auto r = read_as<commodity_instrument>(payload))
        return commodity_instrument_data{std::move(*r), {}};
    return std::nullopt;
}

template <typename Leaf>
std::optional<Leaf> decode_single(const instrument_payload& payload) {
    if (payload.type != leaf_name<Leaf>())
        return std::nullopt;
    return read_as<Leaf>(payload);
}

}

instrument_payload encode_instrument(const trade_instrument& instrument) {
    instrument_payload out;
    std::visit(
        [&](const auto& leaf) {
            using T = std::decay_t<decltype(leaf)>;
            if constexpr (std::is_same_v<T, std::monostate>) {
                return;
            } else if constexpr (std::is_same_v<T, swap_instrument_data>) {
                encode_swap(leaf.instrument, leaf.legs, leaf.call_dates, out);
            } else if constexpr (std::is_same_v<T, commodity_instrument_data>) {
                out.type = std::string(leaf_name<commodity_instrument>());
                out.body = rfl::json::write(
                    commodity_leaf_payload{leaf.instrument, leaf.constituents});
            } else if constexpr (std::is_same_v<T, composite_instrument_data>) {
                encode_leaf_with_legs(leaf.instrument, leaf.legs, out);
            } else if constexpr (std::is_same_v<T, fx_instrument_variant> ||
                                 std::is_same_v<T, equity_instrument_variant>) {
                encode_flat(leaf, out);
            } else {
                encode_leaf(leaf, out);
            }
        },
        instrument);
    return out;
}

trade_instrument decode_instrument(const instrument_payload& payload) {
    if (payload.type.empty())
        return trade_instrument{std::monostate{}};

    if (auto v = decode_swap<rates_instrument_variant>(payload))
        return trade_instrument{std::move(*v)};
    if (auto v = decode_flat<fx_instrument_variant>(payload))
        return trade_instrument{std::move(*v)};
    if (auto v = decode_flat<equity_instrument_variant>(payload))
        return trade_instrument{std::move(*v)};
    if (auto v = decode_leaf_with_legs<composite_instrument, composite_leg>(payload))
        return trade_instrument{std::move(*v)};
    if (auto v = decode_single<bond_instrument_data>(payload))
        return trade_instrument{std::move(*v)};
    if (auto v = decode_single<credit_instrument>(payload))
        return trade_instrument{std::move(*v)};
    if (auto v = decode_commodity(payload))
        return trade_instrument{std::move(*v)};
    if (auto v = decode_single<scripted_instrument>(payload))
        return trade_instrument{std::move(*v)};

    return trade_instrument{std::monostate{}};
}

}
