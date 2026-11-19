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
#include "ores.ore.core/domain/decimal_projection.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/domain/underlying_block_projection.hpp"
#include "ores.ore.core/domain/volatility_product_projection.hpp"
#include <algorithm>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

/*
 * The multi-asset family: Autocallable_01, DoubleDigitalOption,
 * EuropeanOptionBarrier and PerformanceOption_01. Each states its own element,
 * and its underlyings may be equities, FX rates or commodities, so the family
 * holds one array per product and the rows the products share.
 */

namespace ores::ore::domain {
namespace {

using ores::utility::decimal::decimal;
using trading::domain::autocallable_instrument;
using trading::domain::double_digital_option_instrument;
using trading::domain::european_option_barrier_instrument;
using trading::domain::instrument_value;
using trading::domain::multi_asset_instrument_data;
using trading::domain::performance_option_instrument;
using volatility_product::read_date;
using volatility_product::read_decimal;
using volatility_product::write_date;
using volatility_product::write_decimal;
using xml::record_reader;
using xml::record_writer;

constexpr std::string_view multi_asset_actor = "ores";
constexpr std::string_view multi_asset_change_reason = "system.external_data_import";
constexpr std::string_view multi_asset_change_commentary = "Imported from ORE XML";

bool is_multi_asset_trade_type(std::string_view type) {
    return type == "Autocallable_01" || type == "DoubleDigitalOption" ||
           type == "EuropeanOptionBarrier" || type == "PerformanceOption_01";
}

/// The element a product states, which ORE spells without the variant's underscore.
std::string element_of(std::string_view trade_type) {
    if (trade_type == "Autocallable_01")
        return "Autocallable01Data";
    if (trade_type == "PerformanceOption_01")
        return "PerformanceOption01Data";
    return std::string(trade_type) + "Data";
}

template <typename Row>
Row multi_asset_make_row(std::string_view trade_type_code) {
    Row r{};
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = std::string(multi_asset_actor);
    r.audit.performed_by = std::string(multi_asset_actor);
    r.audit.change_reason_code = std::string(multi_asset_change_reason);
    r.audit.change_commentary = std::string(multi_asset_change_commentary);
    return r;
}

std::string text_or_empty(const record_reader& data, std::string_view element) {
    return scalar_terms::read_text(data, element).value_or("");
}

/// The Underlying blocks each product states, and the role each row keeps.
constexpr underlying_block::underlying_holder multi_asset_underlying_holders[] = {
    {"Autocallable_01", "", "Underlying", "underlying"},
    {"DoubleDigitalOption", "", "Underlying1", "underlying1"},
    {"DoubleDigitalOption", "", "Underlying2", "underlying2"},
    {"DoubleDigitalOption", "", "Underlying3", "underlying3"},
    {"DoubleDigitalOption", "", "Underlying4", "underlying4"},
    {"EuropeanOptionBarrier", "", "OptionUnderlying", "optionunderlying"},
    {"EuropeanOptionBarrier", "", "BarrierUnderlying", "barrierunderlying"},
    {"PerformanceOption_01", "Underlyings", "Underlying", "underlying"},
};

/// The schedules a product states on its own element.
constexpr option_block::product_schedule multi_asset_product_schedules[] = {
    {"EuropeanOptionBarrier", "BarrierSchedule", "barrier_schedule"},
};

/// A schedule a product states inside a wrapper element, as ScheduleData.
struct wrapped_schedule {
    std::string_view trade_type;
    std::string_view wrapper;
    std::string_view role;
};

constexpr wrapped_schedule multi_asset_wrapped_schedules[] = {
    {"Autocallable_01", "FixingDates", "fixing_schedule"},
    {"Autocallable_01", "SettlementDates", "settlement_schedule"},
};

/// A list of plain numbers a product states, and the role each value row keeps.
struct value_list {
    std::string_view trade_type;
    std::string_view element;
    std::string_view leaf;
    std::string_view role;
};

constexpr value_list multi_asset_value_lists[] = {
    {"Autocallable_01", "AccumulationFactors", "Factor", "accumulation_factor"},
    {"PerformanceOption_01", "StrikePrices", "StrikePrice", "strike_price"},
};

auto read_autocallable(const record_reader& data) -> autocallable_instrument {
    auto r = multi_asset_make_row<autocallable_instrument>("Autocallable_01");
    r.notional_amount = read_decimal(data, "NotionalAmount").value_or(decimal{});
    r.determination_level = read_decimal(data, "DeterminationLevel").value_or(decimal{});
    r.trigger_level = read_decimal(data, "TriggerLevel").value_or(decimal{});
    r.long_short = text_or_empty(data, "Position");
    r.pay_currency = text_or_empty(data, "PayCcy");
    r.cap = read_decimal(data, "Cap").value_or(decimal{});
    return r;
}

void write_autocallable(record_writer& data, const autocallable_instrument& v) {
    write_decimal(data, "NotionalAmount", v.notional_amount);
    write_decimal(data, "DeterminationLevel", v.determination_level);
    write_decimal(data, "TriggerLevel", v.trigger_level);
    scalar_terms::write_text(data, "Position", v.long_short);
    scalar_terms::write_text(data, "PayCcy", v.pay_currency);
    write_decimal(data, "Cap", v.cap);
}

auto read_double_digital(const record_reader& data) -> double_digital_option_instrument {
    auto r = multi_asset_make_row<double_digital_option_instrument>("DoubleDigitalOption");
    r.expiry = read_date(data, "Expiry");
    r.settlement = read_date(data, "Settlement");
    r.binary_payout = read_decimal(data, "BinaryPayout").value_or(decimal{});
    r.binary_level_1 = read_decimal(data, "BinaryLevel1").value_or(decimal{});
    r.binary_level_2 = read_decimal(data, "BinaryLevel2").value_or(decimal{});
    r.binary_level_upper_1 = read_decimal(data, "BinaryLevelUpper1");
    r.binary_level_upper_2 = read_decimal(data, "BinaryLevelUpper2");
    r.type_1 = text_or_empty(data, "Type1");
    r.type_2 = text_or_empty(data, "Type2");
    r.long_short = text_or_empty(data, "Position");
    r.name_1 = scalar_terms::read_text(data, "Name1");
    r.name_2 = scalar_terms::read_text(data, "Name2");
    r.pay_currency = text_or_empty(data, "PayCcy");
    return r;
}

void write_double_digital(record_writer& data, const double_digital_option_instrument& v) {
    write_date(data, "Expiry", v.expiry);
    write_date(data, "Settlement", v.settlement);
    write_decimal(data, "BinaryPayout", v.binary_payout);
    write_decimal(data, "BinaryLevel1", v.binary_level_1);
    write_decimal(data, "BinaryLevel2", v.binary_level_2);
    write_decimal(data, "BinaryLevelUpper1", v.binary_level_upper_1);
    write_decimal(data, "BinaryLevelUpper2", v.binary_level_upper_2);
    scalar_terms::write_text(data, "Type1", v.type_1);
    scalar_terms::write_text(data, "Type2", v.type_2);
    scalar_terms::write_text(data, "Position", v.long_short);
    if (v.name_1)
        scalar_terms::write_text(data, "Name1", *v.name_1);
    if (v.name_2)
        scalar_terms::write_text(data, "Name2", *v.name_2);
    scalar_terms::write_text(data, "PayCcy", v.pay_currency);
}

auto read_european_option_barrier(const record_reader& data) -> european_option_barrier_instrument {
    auto r = multi_asset_make_row<european_option_barrier_instrument>("EuropeanOptionBarrier");
    r.quantity = decimal_as_double(data, "Quantity").value_or(0.0);
    r.option_type = text_or_empty(data, "PutCall");
    r.long_short = text_or_empty(data, "LongShort");
    r.strike = read_decimal(data, "Strike").value_or(decimal{});
    r.premium_amount = read_decimal(data, "PremiumAmount").value_or(decimal{});
    r.premium_currency = text_or_empty(data, "PremiumCurrency");
    r.premium_date = read_date(data, "PremiumDate");
    r.option_expiry = read_date(data, "OptionExpiry");
    r.barrier_level = read_decimal(data, "BarrierLevel").value_or(decimal{});
    r.barrier_type = text_or_empty(data, "BarrierType");
    r.barrier_style = text_or_empty(data, "BarrierStyle");
    r.settlement_date = read_date(data, "SettlementDate");
    r.pay_currency = text_or_empty(data, "PayCcy");
    return r;
}

void write_european_option_barrier(record_writer& data,
                                   const european_option_barrier_instrument& v) {
    set_decimal_from_double(data, "Quantity", v.quantity);
    scalar_terms::write_text(data, "PutCall", v.option_type);
    scalar_terms::write_text(data, "LongShort", v.long_short);
    write_decimal(data, "Strike", v.strike);
    write_decimal(data, "PremiumAmount", v.premium_amount);
    scalar_terms::write_text(data, "PremiumCurrency", v.premium_currency);
    write_date(data, "PremiumDate", v.premium_date);
    write_date(data, "OptionExpiry", v.option_expiry);
    write_decimal(data, "BarrierLevel", v.barrier_level);
    scalar_terms::write_text(data, "BarrierType", v.barrier_type);
    scalar_terms::write_text(data, "BarrierStyle", v.barrier_style);
    write_date(data, "SettlementDate", v.settlement_date);
    scalar_terms::write_text(data, "PayCcy", v.pay_currency);
}

auto read_performance_option(const record_reader& data) -> performance_option_instrument {
    auto r = multi_asset_make_row<performance_option_instrument>("PerformanceOption_01");
    r.notional_amount = read_decimal(data, "NotionalAmount").value_or(decimal{});
    r.participation_rate = decimal_as_double(data, "ParticipationRate").value_or(0.0);
    r.valuation_date = read_date(data, "ValuationDate");
    r.settlement_date = read_date(data, "SettlementDate");
    r.strike = read_decimal(data, "Strike").value_or(decimal{});
    if (const auto included = scalar_terms::read_text(data, "StrikeIncluded"))
        r.strike_included = option_block::is_flag_on(*included);
    r.long_short = text_or_empty(data, "Position");
    r.pay_currency = text_or_empty(data, "PayCcy");
    return r;
}

void write_performance_option(record_writer& data, const performance_option_instrument& v) {
    write_decimal(data, "NotionalAmount", v.notional_amount);
    set_decimal_from_double(data, "ParticipationRate", v.participation_rate);
    write_date(data, "ValuationDate", v.valuation_date);
    write_date(data, "SettlementDate", v.settlement_date);
    write_decimal(data, "Strike", v.strike);
    if (v.strike_included)
        scalar_terms::write_text(
            data, "StrikeIncluded", std::string(option_block::flag_text(*v.strike_included)));
    scalar_terms::write_text(data, "Position", v.long_short);
    scalar_terms::write_text(data, "PayCcy", v.pay_currency);
}

/// Reads each value of the product's lists into a value row under the list's role.
void read_values(multi_asset_instrument_data& carrier,
                 const record_reader& product,
                 std::string_view trade_type) {
    for (const auto& list : multi_asset_value_lists) {
        if (list.trade_type != trade_type)
            continue;
        if (!product.declares(list.element))
            continue;
        const auto rows = product.child(list.element);
        if (!rows)
            continue;
        int sequence = 0;
        for (const auto& value : rows->decimals(list.leaf)) {
            instrument_value row;
            row.value_role = std::string(list.role);
            row.sequence_number = ++sequence;
            row.value = value;
            carrier.values.push_back(std::move(row));
        }
    }
}

/// Writes each list the rows keep for the product, in the rows' order.
void write_values(record_writer& product,
                  const multi_asset_instrument_data& carrier,
                  std::string_view trade_type) {
    for (const auto& list : multi_asset_value_lists) {
        if (list.trade_type != trade_type)
            continue;
        std::vector<const instrument_value*> rows;
        for (const auto& row : carrier.values)
            if (row.value_role == list.role)
                rows.push_back(&row);
        if (rows.empty())
            continue;
        std::ranges::sort(rows, {}, &instrument_value::sequence_number);
        auto element = product.child(list.element);
        for (const auto* row : rows)
            element.append_decimal(list.leaf, row->value);
    }
}

}

auto read_multi_asset_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_multi_asset_trade_type(trade_type))
        return std::nullopt;

    multi_asset_instrument_data data;
    const auto product = trade.child(element_of(trade_type));
    if (product) {
        if (trade_type == "Autocallable_01")
            data.autocallables.push_back(read_autocallable(*product));
        else if (trade_type == "DoubleDigitalOption")
            data.double_digital_options.push_back(read_double_digital(*product));
        else if (trade_type == "EuropeanOptionBarrier")
            data.european_option_barriers.push_back(read_european_option_barrier(*product));
        else
            data.performance_options.push_back(read_performance_option(*product));

        read_values(data, *product, trade_type);
        if (product->declares("Premiums"))
            option_block::read_premiums(data, *product);
        for (const auto& entry : multi_asset_wrapped_schedules)
            if (entry.trade_type == trade_type)
                if (product->declares(entry.wrapper))
                    if (const auto wrapper = product->child(entry.wrapper))
                        if (const auto schedule = wrapper->child("ScheduleData"))
                            option_block::read_schedule(data, *schedule, "option", 1, entry.role);
        for (const auto& holder : multi_asset_underlying_holders) {
            if (holder.trade_type != trade_type)
                continue;
            if (holder.holder.empty()) {
                underlying_block::read_underlyings(data, *product, holder.element, holder.role);
            } else if (product->declares(holder.holder)) {
                if (const auto container = product->child(holder.holder))
                    underlying_block::read_underlyings(
                        data, *container, holder.element, holder.role);
            }
        }
    }
    option_block::read_product_schedules(data, trade, trade_type, multi_asset_product_schedules);
    return trading::domain::trade_instrument{std::move(data)};
}

auto write_multi_asset_instrument(xml::record_writer& trade,
                                  std::string_view trade_type,
                                  const trading::domain::trade_instrument& instrument,
                                  const ores::marketdata::projection::ore_projection&) -> bool {
    const auto* v = std::get_if<multi_asset_instrument_data>(&instrument);
    if (v == nullptr || !is_multi_asset_trade_type(trade_type))
        return false;

    const bool holds_row =
        (trade_type == "Autocallable_01" && !v->autocallables.empty()) ||
        (trade_type == "DoubleDigitalOption" && !v->double_digital_options.empty()) ||
        (trade_type == "EuropeanOptionBarrier" && !v->european_option_barriers.empty()) ||
        (trade_type == "PerformanceOption_01" && !v->performance_options.empty());
    if (!holds_row)
        return false;

    auto data = trade.child(element_of(trade_type));
    if (trade_type == "Autocallable_01")
        write_autocallable(data, v->autocallables.front());
    else if (trade_type == "DoubleDigitalOption")
        write_double_digital(data, v->double_digital_options.front());
    else if (trade_type == "EuropeanOptionBarrier")
        write_european_option_barrier(data, v->european_option_barriers.front());
    else
        write_performance_option(data, v->performance_options.front());

    for (const auto& holder : multi_asset_underlying_holders) {
        if (holder.trade_type != trade_type)
            continue;
        if (holder.holder.empty()) {
            underlying_block::write_underlyings(data, holder.element, *v, holder.role);
        } else if (!underlying_block::underlyings_of(*v, holder.role).empty()) {
            auto container = data.child(holder.holder);
            underlying_block::write_underlyings(container, holder.element, *v, holder.role);
        }
    }
    write_values(data, *v, trade_type);
    if (data.declares("Premiums"))
        option_block::write_premiums(data, *v);
    for (const auto& entry : multi_asset_wrapped_schedules)
        if (entry.trade_type == trade_type && option_block::has_product_schedule(*v, entry.role)) {
            auto wrapper = data.child(entry.wrapper);
            option_block::write_product_schedule(wrapper, *v, "ScheduleData", entry.role);
        }
    option_block::write_product_schedules(data, *v, trade_type, multi_asset_product_schedules);
    return true;
}

}
