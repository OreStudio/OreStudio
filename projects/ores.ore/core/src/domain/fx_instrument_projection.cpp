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
#include "ores.platform/time/datetime.hpp"
#include <algorithm>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

using ores::utility::decimal::decimal;

using trading::domain::fx_accumulator_instrument;
using trading::domain::fx_asian_forward_instrument;
using trading::domain::fx_barrier_option_instrument;
using trading::domain::fx_digital_option_instrument;
using trading::domain::fx_forward_instrument;
using trading::domain::fx_instrument_data;
using trading::domain::fx_vanilla_option_instrument;
using trading::domain::fx_variance_swap_instrument;
using trading::domain::range_bound;

using xml::record_reader;
using xml::record_writer;

// ---------------------------------------------------------------------------
// The vocabulary both directions share
// ---------------------------------------------------------------------------

/// The actor and reason every imported row states.
constexpr std::string_view actor = "ores";
constexpr std::string_view change_reason = "system.external_data_import";
constexpr std::string_view change_commentary = "Imported from ORE XML";

/// The trade types the family states, which are the ones the old dispatcher took.
constexpr bool is_fx_trade_type(std::string_view type) {
    return type == "FxForward" || type == "FxSwap" || type == "FxOption" ||
           type == "FxBarrierOption" || type == "FxDigitalOption" ||
           type == "FxDigitalBarrierOption" || type == "FxTouchOption" ||
           type == "FxDoubleTouchOption" || type == "FxVarianceSwap" ||
           type == "FxAverageForward" || type == "FxAccumulator" || type == "FxTaRF" ||
           type == "FxGenericBarrierOption" || type == "FxDoubleBarrierOption" ||
           type == "FxEuropeanBarrierOption" || type == "FxKIKOBarrierOption";
}

template <typename Row>
void stamp_audit(Row& row) {
    row.audit.modified_by = std::string(actor);
    row.audit.performed_by = std::string(actor);
    row.audit.change_reason_code = std::string(change_reason);
    row.audit.change_commentary = std::string(change_commentary);
}

void stamp_range_bound(range_bound& row) {
    row.modified_by = std::string(actor);
    row.performed_by = std::string(actor);
    row.change_reason_code = std::string(change_reason);
    row.change_commentary = std::string(change_commentary);
}

template <typename Row>
auto make_row(std::string_view trade_type_code) -> Row {
    Row r;
    r.identity.trade_type_code = std::string(trade_type_code);
    stamp_audit(r);
    return r;
}

// ---------------------------------------------------------------------------
// Reading: the values, dates and spellings a row states
// ---------------------------------------------------------------------------

auto to_domain_date(const std::string& text) -> std::chrono::year_month_day {
    if (text.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto to_optional_domain_date(const std::string& text)
    -> std::optional<std::chrono::year_month_day> {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto to_ore_date(const std::chrono::year_month_day& d) -> std::string {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

auto to_ore_date(const std::optional<std::chrono::year_month_day>& d) -> std::string {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

auto text_of(const record_reader& r, std::string_view name) -> std::string {
    if (const auto value = r.text(name))
        return *value;
    return {};
}

auto spelling_of(const record_reader& r, std::string_view name) -> std::string {
    if (const auto value = r.spelling(name))
        return *value;
    return {};
}

/// A schema decimal element as the domain's exact decimal, zero when absent.
auto decimal_of(const record_reader& r, std::string_view name) -> decimal {
    if (const auto value = r.decimal(name))
        return *value;
    return decimal{};
}

auto optional_decimal_of(const record_reader& r, std::string_view name) -> std::optional<decimal> {
    return r.decimal(name);
}

/// An absent or empty ORE date maps to an invalid calendar date, which renders
/// back to an empty string, so a missing date round-trips as missing.
auto expiry_date_of(const std::optional<record_reader>& option) -> std::chrono::year_month_day {
    if (!option)
        return {};
    const auto dates = option->child("ExerciseDates");
    if (!dates)
        return {};
    const auto values = dates->values("ExerciseDate");
    if (values.empty())
        return {};
    return to_domain_date(values.front());
}

auto option_type_of(const std::optional<record_reader>& option) -> std::string {
    if (!option)
        return {};
    return text_of(*option, "OptionType");
}

/**
 * @brief Whether a barrier style names the direction it engages.
 *
 * An Up style or a FixingCap engages the upper level, a Down style or a
 * FixingFloor the lower one. The styles that name no direction keep the level
 * in the lower column, which is the shape their readers expect.
 */
auto is_up_barrier_style(const std::string& type) -> bool {
    return type == "UpAndOut" || type == "UpAndIn" || type == "FixingCap";
}

auto levels_of(const record_reader& barrier) -> std::vector<decimal> {
    const auto levels = barrier.child("Levels");
    if (!levels)
        return {};
    return levels->decimals("Level");
}

// BarrierData carries one level for a single barrier and two for a double
// barrier, lower first.
void fill_barrier(const record_reader& barrier,
                  std::string& barrier_type,
                  std::optional<decimal>& lower,
                  std::optional<decimal>& upper) {
    barrier_type = spelling_of(barrier, "Type");
    const auto levels = levels_of(barrier);
    if (levels.size() > 1) {
        lower = levels.front();
        upper = levels[1];
        return;
    }
    if (levels.empty())
        return;
    if (is_up_barrier_style(barrier_type))
        upper = levels.front();
    else
        lower = levels.front();
}

// A KIKO or generic barrier states a level per side, so the pair stays
// positional here; the directional rule is the one fill_barrier states.
void fill_barrier_pair(const record_reader& barriers,
                       std::string& barrier_type,
                       std::optional<decimal>& lower,
                       std::optional<decimal>& upper) {
    const auto rows = barriers.children("BarrierData");
    if (rows.empty())
        return;
    barrier_type = spelling_of(rows.front(), "Type");
    const auto first = levels_of(rows.front());
    if (!first.empty())
        lower = first.front();
    if (rows.size() > 1) {
        const auto second = levels_of(rows[1]);
        if (!second.empty())
            upper = second.front();
    }
}

/// The underlying a product names, either as a name alone or as a whole entry.
auto underlying_code_of(const record_reader& data) -> std::string {
    if (const auto name = data.text("Name"))
        return *name;
    if (const auto underlying = data.child("Underlying"))
        return text_of(*underlying, "Name");
    return {};
}

void read_range_bounds(fx_instrument_data& result, const record_reader& data) {
    const auto bounds = data.child("RangeBounds");
    if (!bounds)
        return;
    int sequence_number = 0;
    for (const auto& rb : bounds->children("RangeBound")) {
        range_bound row;
        row.sequence_number = ++sequence_number;
        row.start_date = to_optional_domain_date(text_of(rb, "startDate"));
        row.range_from = optional_decimal_of(rb, "RangeFrom");
        row.range_to = optional_decimal_of(rb, "RangeTo");
        row.leverage = decimal_as_double(rb, "Leverage");
        row.strike = optional_decimal_of(rb, "Strike");
        row.strike_adjustment = optional_decimal_of(rb, "StrikeAdjustment");
        stamp_range_bound(row);
        result.range_bounds.push_back(std::move(row));
    }
}

// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

auto read_fx_forward(const record_reader& trade, std::string_view trade_type)
    -> fx_forward_instrument {
    auto r = make_row<fx_forward_instrument>(trade_type);
    const auto data = trade.child("FxForwardData");
    if (!data)
        return r;
    r.value_date = to_domain_date(text_of(*data, "ValueDate"));
    r.bought_currency = spelling_of(*data, "BoughtCurrency");
    r.bought_amount = decimal_of(*data, "BoughtAmount");
    r.sold_currency = spelling_of(*data, "SoldCurrency");
    r.sold_amount = decimal_of(*data, "SoldAmount");
    r.settlement = spelling_of(*data, "Settlement");
    return r;
}

auto read_fx_swap(const record_reader& trade) -> fx_forward_instrument {
    auto r = make_row<fx_forward_instrument>("FxSwap");
    const auto data = trade.child("FxSwapData");
    if (!data)
        return r;
    // Near leg only; the far leg is the mapper's documented coverage gap.
    r.value_date = to_domain_date(text_of(*data, "NearDate"));
    r.bought_currency = spelling_of(*data, "NearBoughtCurrency");
    r.bought_amount = decimal_of(*data, "NearBoughtAmount");
    r.sold_currency = spelling_of(*data, "NearSoldCurrency");
    r.sold_amount = decimal_of(*data, "NearSoldAmount");
    r.settlement = spelling_of(*data, "Settlement");
    return r;
}

auto read_fx_option(const record_reader& trade) -> fx_vanilla_option_instrument {
    auto r = make_row<fx_vanilla_option_instrument>("FxOption");
    const auto data = trade.child("FxOptionData");
    if (!data)
        return r;
    r.bought_currency = spelling_of(*data, "BoughtCurrency");
    r.bought_amount = decimal_of(*data, "BoughtAmount");
    r.sold_currency = spelling_of(*data, "SoldCurrency");
    r.sold_amount = optional_decimal_of(*data, "SoldAmount").value_or(decimal{});

    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.settlement = option ? spelling_of(*option, "Settlement") : std::string{};
    r.exercise_style = "European";
    r.expiry_date = expiry_date_of(option);
    return r;
}

// The four barrier products that state a single BarrierData each share one
// shape; the element that carries it is the only difference between them.
auto read_barrier_product(const record_reader& trade,
                          std::string_view element,
                          std::string_view trade_type) -> fx_barrier_option_instrument {
    auto r = make_row<fx_barrier_option_instrument>(trade_type);
    const auto data = trade.child(element);
    if (!data)
        return r;
    r.bought_currency = spelling_of(*data, "BoughtCurrency");
    r.bought_amount = decimal_of(*data, "BoughtAmount");
    r.sold_currency = spelling_of(*data, "SoldCurrency");
    r.sold_amount = decimal_of(*data, "SoldAmount");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    if (const auto barrier = data->child("BarrierData"))
        fill_barrier(*barrier, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

auto read_fx_kiko_barrier_option(const record_reader& trade) -> fx_barrier_option_instrument {
    auto r = make_row<fx_barrier_option_instrument>("FxKIKOBarrierOption");
    const auto data = trade.child("FxKIKOBarrierOptionData");
    if (!data)
        return r;
    r.bought_currency = spelling_of(*data, "BoughtCurrency");
    r.bought_amount = decimal_of(*data, "BoughtAmount");
    r.sold_currency = spelling_of(*data, "SoldCurrency");
    r.sold_amount = decimal_of(*data, "SoldAmount");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    if (const auto barriers = data->child("Barriers"))
        fill_barrier_pair(*barriers, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

auto read_fx_generic_barrier_option(const record_reader& trade) -> fx_barrier_option_instrument {
    auto r = make_row<fx_barrier_option_instrument>("FxGenericBarrierOption");
    const auto data = trade.child("FxGenericBarrierOptionData");
    if (!data)
        return r;
    r.underlying_code = underlying_code_of(*data);
    r.bought_currency = spelling_of(*data, "PayCurrency");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    if (const auto barriers = data->child("Barriers"))
        fill_barrier_pair(*barriers, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

auto read_fx_digital_fields(const record_reader& data, fx_digital_option_instrument& r) -> void {
    r.foreign_currency = spelling_of(data, "ForeignCurrency");
    r.domestic_currency = spelling_of(data, "DomesticCurrency");
    // The payoff defaults to the foreign currency, which is the one the schema
    // states it in when the document names none.
    r.payoff_currency = r.foreign_currency;
    r.strike = decimal_of(data, "Strike");
    r.payoff_amount = decimal_of(data, "PayoffAmount");
    const auto option = data.child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
}

auto read_fx_digital_option(const record_reader& trade, std::string_view trade_type)
    -> fx_digital_option_instrument {
    auto r = make_row<fx_digital_option_instrument>(trade_type);
    r.long_short = "Long";
    if (const auto data = trade.child("FxDigitalOptionData"))
        read_fx_digital_fields(*data, r);
    return r;
}

auto read_fx_digital_barrier_option(const record_reader& trade) -> fx_digital_option_instrument {
    auto r = make_row<fx_digital_option_instrument>("FxDigitalBarrierOption");
    r.long_short = "Long";
    const auto data = trade.child("FxDigitalBarrierOptionData");
    if (!data)
        return r;
    read_fx_digital_fields(*data, r);
    if (const auto barrier = data->child("BarrierData")) {
        const auto levels = levels_of(*barrier);
        if (!levels.empty()) {
            r.barrier_type = spelling_of(*barrier, "Type");
            // One level, placed by the style's direction, so the digital table
            // and the barrier table state a level the same way.
            if (is_up_barrier_style(r.barrier_type))
                r.upper_barrier = levels.front();
            else
                r.lower_barrier = levels.front();
        }
    }
    return r;
}

auto read_fx_touch_option(const record_reader& trade, std::string_view trade_type)
    -> fx_digital_option_instrument {
    auto r = make_row<fx_digital_option_instrument>(trade_type);
    r.long_short = "Long";

    // The two touch products state the same data under an element each, so the
    // trade type chooses the element rather than the element choosing the type.
    const auto data = trade_type == "FxDoubleTouchOption" ? trade.child("FxDoubleTouchOptionData") :
                                                            trade.child("FxTouchOptionData");
    if (!data)
        return r;

    r.foreign_currency = spelling_of(*data, "ForeignCurrency");
    r.domestic_currency = spelling_of(*data, "DomesticCurrency");
    r.payoff_currency = spelling_of(*data, "PayoffCurrency");
    r.payoff_amount = decimal_of(*data, "PayoffAmount");
    r.expiry_date = expiry_date_of(data->child("OptionData"));
    if (const auto barrier = data->child("BarrierData"))
        fill_barrier(*barrier, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

auto read_fx_variance_swap(const record_reader& trade) -> fx_variance_swap_instrument {
    auto r = make_row<fx_variance_swap_instrument>("FxVarianceSwap");
    r.long_short = "Long";
    const auto data = trade.child("FxVarianceSwapData");
    if (!data)
        return r;
    r.underlying_code = underlying_code_of(*data);
    r.start_date = to_domain_date(text_of(*data, "StartDate"));
    r.end_date = to_domain_date(text_of(*data, "EndDate"));
    r.strike = decimal_as_double(*data, "Strike").value_or(0.0);
    r.notional = decimal_of(*data, "Notional");
    r.currency = spelling_of(*data, "Currency");
    r.moment_type = "Variance";
    return r;
}

auto read_fx_average_forward(const record_reader& trade) -> fx_asian_forward_instrument {
    auto r = make_row<fx_asian_forward_instrument>("FxAverageForward");
    const auto data = trade.child("FxAverageForwardData");
    if (!data)
        return r;
    r.payment_date = to_optional_domain_date(text_of(*data, "PaymentDate"));
    r.reference_currency = spelling_of(*data, "ReferenceCurrency");
    r.reference_notional = optional_decimal_of(*data, "ReferenceNotional");
    r.settlement_currency = spelling_of(*data, "SettlementCurrency");
    r.settlement_notional = optional_decimal_of(*data, "SettlementNotional");
    r.fx_index = text_of(*data, "FXIndex");
    r.long_short = "Long";
    return r;
}

auto read_fx_accumulator(fx_instrument_data& result, const record_reader& trade)
    -> fx_accumulator_instrument {
    auto r = make_row<fx_accumulator_instrument>("FxAccumulator");
    r.long_short = "Long";
    const auto data = trade.child("FxAccumulatorData");
    if (!data)
        return r;
    r.currency = spelling_of(*data, "Currency");
    if (const auto underlying = data->child("Underlying"))
        r.underlying_code = text_of(*underlying, "Name");
    r.fixing_amount = decimal_of(*data, "FixingAmount");
    if (const auto strike = data->decimal("Strike"))
        r.strike = *strike;
    r.start_date = to_domain_date(text_of(*data, "StartDate"));
    if (const auto barriers = data->child("Barriers"))
        for (const auto& bd : barriers->children("BarrierData")) {
            const auto btype = spelling_of(bd, "Type");
            if ((btype == "DownAndOut" || btype == "UpAndOut")) {
                const auto levels = levels_of(bd);
                if (!levels.empty()) {
                    r.knock_out_barrier = levels.front();
                    break;
                }
            }
        }
    read_range_bounds(result, *data);
    return r;
}

auto read_fx_tarf(const record_reader& trade) -> fx_asian_forward_instrument {
    auto r = make_row<fx_asian_forward_instrument>("FxTaRF");
    const auto data = trade.child("FxTaRFData");
    if (!data)
        return r;
    r.currency = spelling_of(*data, "Currency");
    if (const auto underlying = data->child("Underlying"))
        r.fx_index = text_of(*underlying, "Name");
    r.fixing_amount = optional_decimal_of(*data, "FixingAmount");
    if (const auto option = data->child("OptionData"))
        r.long_short = text_of(*option, "LongShort");
    r.strike = optional_decimal_of(*data, "Strike");
    if (const auto barriers = data->child("Barriers"))
        for (const auto& bd : barriers->children("BarrierData")) {
            const auto btype = spelling_of(bd, "Type");
            if (btype == "CumulatedProfitCap" || btype == "FixingCap") {
                const auto levels = levels_of(bd);
                if (!levels.empty()) {
                    r.target_amount = levels.front();
                    break;
                }
            }
        }
    return r;
}

// ---------------------------------------------------------------------------
// Writing: the shared shapes
// ---------------------------------------------------------------------------

void write_option_entry(record_writer option,
                        const std::string& option_type,
                        const std::chrono::year_month_day& expiry_date,
                        std::string_view long_short = "Long") {
    option.set_text("LongShort", long_short);
    if (!option_type.empty())
        option.set_text("OptionType", option_type);
    if (expiry_date.ok())
        option.child("ExerciseDates").append_value("ExerciseDate", to_ore_date(expiry_date));
}

void push_barrier(record_writer barrier, const std::string& type, const decimal& level) {
    barrier.set_spelling("Type", type, "barrier type");
    barrier.child("Levels").append_decimal("Level", level);
}

// The reverse of fill_barrier. An absent level is absent because the column is
// nullable, so the group starts from whichever level the row states.
void write_barrier_group(record_writer& parent,
                         const std::string& type,
                         const std::optional<decimal>& lower,
                         const std::optional<decimal>& upper) {
    const bool has_lower = lower.has_value();
    if (!has_lower && !upper.has_value())
        return;
    auto barrier = parent.child("BarrierData");
    push_barrier(barrier, type, has_lower ? *lower : *upper);
    if (has_lower && upper.has_value())
        push_barrier(barrier, type, *upper);
}

// The reverse of fill_barrier_pair: a two-sided product states a level per side
// in its own BarrierData, and the first one is the lower level's.
void write_barrier_pair(record_writer& parent, const fx_barrier_option_instrument& v) {
    const bool has_lower = !v.barrier_type.empty() && v.lower_barrier.has_value();
    if (!has_lower && !v.upper_barrier.has_value())
        return;
    auto barriers = parent.child("Barriers");
    if (has_lower)
        push_barrier(barriers.append("BarrierData"), v.barrier_type, *v.lower_barrier);
    if (v.upper_barrier.has_value())
        push_barrier(barriers.append("BarrierData"), v.barrier_type, *v.upper_barrier);
}

void write_settlement(record_writer& option, const std::string& settlement) {
    if (!settlement.empty())
        option.set_spelling("Settlement", settlement, "settlement type");
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

void write_fx_forward(record_writer& trade, const fx_forward_instrument& v) {
    auto data = trade.child("FxForwardData");
    data.set_text("ValueDate", to_ore_date(v.value_date));
    data.set_spelling("BoughtCurrency", v.bought_currency, "currency");
    data.set_decimal("BoughtAmount", v.bought_amount);
    data.set_spelling("SoldCurrency", v.sold_currency, "currency");
    data.set_decimal("SoldAmount", v.sold_amount);
    write_settlement(data, v.settlement);
}

void write_fx_swap(record_writer& trade, const fx_forward_instrument& v) {
    auto data = trade.child("FxSwapData");
    data.set_text("NearDate", to_ore_date(v.value_date));
    data.set_spelling("NearBoughtCurrency", v.bought_currency, "currency");
    data.set_decimal("NearBoughtAmount", v.bought_amount);
    data.set_spelling("NearSoldCurrency", v.sold_currency, "currency");
    data.set_decimal("NearSoldAmount", v.sold_amount);
    // The far leg is the mapper's documented coverage gap: the schema requires
    // it, so it round-trips as an empty date and two zero amounts.
    data.set_text("FarDate", std::string{});
    data.set_decimal("FarBoughtAmount", decimal{});
    data.set_decimal("FarSoldAmount", decimal{});
    write_settlement(data, v.settlement);
}

void write_fx_option(record_writer& trade, const fx_vanilla_option_instrument& v) {
    auto data = trade.child("FxOptionData");
    data.set_spelling("BoughtCurrency", v.bought_currency, "currency");
    data.set_decimal("BoughtAmount", v.bought_amount);
    data.set_spelling("SoldCurrency", v.sold_currency, "currency");
    if (!v.sold_amount.is_zero())
        data.set_decimal("SoldAmount", v.sold_amount);

    auto option = data.child("OptionData");
    write_option_entry(option, v.option_type, v.expiry_date);
    write_settlement(option, v.settlement);
}

void write_barrier_product(record_writer& trade,
                           std::string_view element,
                           const fx_barrier_option_instrument& v) {
    auto data = trade.child(element);
    data.set_spelling("BoughtCurrency", v.bought_currency, "currency");
    data.set_decimal("BoughtAmount", v.bought_amount);
    data.set_spelling("SoldCurrency", v.sold_currency, "currency");
    data.set_decimal("SoldAmount", v.sold_amount);
    auto option = data.child("OptionData");
    write_option_entry(option, v.option_type, v.expiry_date);
    write_settlement(option, v.settlement);
    if (!v.barrier_type.empty())
        write_barrier_group(data, v.barrier_type, v.lower_barrier, v.upper_barrier);
}

void write_fx_kiko_barrier_option(record_writer& trade, const fx_barrier_option_instrument& v) {
    auto data = trade.child("FxKIKOBarrierOptionData");
    data.set_spelling("BoughtCurrency", v.bought_currency, "currency");
    data.set_decimal("BoughtAmount", v.bought_amount);
    data.set_spelling("SoldCurrency", v.sold_currency, "currency");
    data.set_decimal("SoldAmount", v.sold_amount);
    write_option_entry(data.child("OptionData"), v.option_type, v.expiry_date);
    write_barrier_pair(data, v);
}

void write_fx_generic_barrier_option(record_writer& trade, const fx_barrier_option_instrument& v) {
    auto data = trade.child("FxGenericBarrierOptionData");
    if (!v.bought_currency.empty())
        data.set_spelling("PayCurrency", v.bought_currency, "currency");
    if (!v.underlying_code.empty())
        data.set_text("Name", v.underlying_code);
    write_option_entry(data.child("OptionData"), v.option_type, v.expiry_date);
    write_barrier_pair(data, v);
}

void write_fx_digital_option(record_writer& trade, const fx_digital_option_instrument& v) {
    auto data = trade.child("FxDigitalOptionData");
    data.set_spelling("ForeignCurrency", v.foreign_currency, "currency");
    data.set_spelling("DomesticCurrency", v.domestic_currency, "currency");
    data.set_decimal("Strike", v.strike.value_or(decimal{}));
    data.set_decimal("PayoffAmount", v.payoff_amount);
    write_option_entry(data.child("OptionData"), v.option_type, v.expiry_date, v.long_short);
}

void write_fx_digital_barrier_option(record_writer& trade, const fx_digital_option_instrument& v) {
    auto data = trade.child("FxDigitalBarrierOptionData");
    data.set_spelling("ForeignCurrency", v.foreign_currency, "currency");
    data.set_spelling("DomesticCurrency", v.domestic_currency, "currency");
    data.set_decimal("Strike", v.strike.value_or(decimal{}));
    data.set_decimal("PayoffAmount", v.payoff_amount);
    write_option_entry(data.child("OptionData"), v.option_type, v.expiry_date, v.long_short);
    if (!v.barrier_type.empty())
        write_barrier_group(data, v.barrier_type, v.lower_barrier, v.upper_barrier);
}

void write_fx_touch_option(record_writer& trade,
                           std::string_view trade_type,
                           const fx_digital_option_instrument& v) {
    const auto double_touch = trade_type == "FxDoubleTouchOption";
    auto data = trade.child(double_touch ? "FxDoubleTouchOptionData" : "FxTouchOptionData");
    if (!v.foreign_currency.empty())
        data.set_spelling("ForeignCurrency", v.foreign_currency, "currency");
    if (!v.domestic_currency.empty())
        data.set_spelling("DomesticCurrency", v.domestic_currency, "currency");
    if (!v.payoff_currency.empty())
        data.set_spelling("PayoffCurrency", v.payoff_currency, "currency");
    data.set_decimal("PayoffAmount", v.payoff_amount);
    // The schema gives a touch no option type: its direction is the option
    // block's LongShort and its side is the barrier's own style.
    write_option_entry(data.child("OptionData"), "", v.expiry_date, v.long_short);
    if (!v.barrier_type.empty())
        write_barrier_group(data, v.barrier_type, v.lower_barrier, v.upper_barrier);
}

void write_fx_variance_swap(record_writer& trade, const fx_variance_swap_instrument& v) {
    auto data = trade.child("FxVarianceSwapData");
    data.set_text("StartDate", to_ore_date(v.start_date));
    data.set_text("EndDate", to_ore_date(v.end_date));
    if (!v.currency.empty())
        data.set_spelling("Currency", v.currency, "currency");
    if (!v.underlying_code.empty())
        data.set_text("Name", v.underlying_code);
    data.set_text("LongShort", v.long_short);
    set_decimal_from_double(data, "Strike", v.strike);
    data.set_decimal("Notional", v.notional);
    // The document states the calendar it accrues under and the row does not,
    // so the mapper's own default is what the export writes.
    data.set_text("Calendar", "TARGET");
}

void write_fx_average_forward(record_writer& trade, const fx_asian_forward_instrument& v) {
    auto data = trade.child("FxAverageForwardData");
    data.set_text("PaymentDate", to_ore_date(v.payment_date));
    if (!v.reference_currency.empty())
        data.set_spelling("ReferenceCurrency", v.reference_currency, "currency");
    data.set_decimal("ReferenceNotional", v.reference_notional.value_or(decimal{}));
    if (!v.settlement_currency.empty())
        data.set_spelling("SettlementCurrency", v.settlement_currency, "currency");
    data.set_decimal("SettlementNotional", v.settlement_notional.value_or(decimal{}));
    data.set_text("FXIndex", v.fx_index);
    data.set_flag("FixedPayer", false);
    data.child("ObservationDates").append("Rules").set_text("Tenor", "1M");
}

void write_fx_accumulator(record_writer& trade,
                          const fx_accumulator_instrument& v,
                          const std::vector<range_bound>& rows) {
    // The document states at least one range bound, so the refusal runs before
    // the trade is written rather than after half of it is.
    if (rows.empty())
        throw std::runtime_error(
            "an FxAccumulator with no range bound cannot be written: the document needs at least"
            " one entry in RangeBounds");

    auto data = trade.child("FxAccumulatorData");
    if (!v.currency.empty())
        data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("FixingAmount", v.fixing_amount);
    if (v.strike != decimal{})
        data.set_decimal("Strike", v.strike);

    auto underlying = data.child("Underlying");
    underlying.set_text("Name", v.underlying_code);
    underlying.set_text("Type", "FX");
    data.child("OptionData").set_text("LongShort", v.long_short);
    if (v.start_date.ok())
        data.set_text("StartDate", to_ore_date(v.start_date));

    auto rule = data.child("ObservationDates").append("Rules");
    if (v.start_date.ok())
        rule.set_text("StartDate", to_ore_date(v.start_date));
    rule.set_text("Tenor", "1D");

    std::vector<const range_bound*> ordered;
    ordered.reserve(rows.size());
    for (const auto& row : rows)
        ordered.push_back(&row);
    std::sort(ordered.begin(), ordered.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    auto bounds = data.child("RangeBounds");
    for (const auto* row : ordered) {
        auto writer = bounds.append("RangeBound");
        if (row->start_date)
            writer.set_text("startDate", to_ore_date(*row->start_date));
        if (row->range_from)
            writer.set_decimal("RangeFrom", *row->range_from);
        if (row->range_to)
            writer.set_decimal("RangeTo", *row->range_to);
        if (row->leverage)
            set_decimal_from_double(writer, "Leverage", *row->leverage);
        if (row->strike)
            writer.set_decimal("Strike", *row->strike);
        if (row->strike_adjustment)
            writer.set_decimal("StrikeAdjustment", *row->strike_adjustment);
    }

    if (v.knock_out_barrier.has_value()) {
        auto barrier = data.child("Barriers").append("BarrierData");
        push_barrier(barrier, "UpAndOut", *v.knock_out_barrier);
    }
}

void write_fx_tarf(record_writer& trade, const fx_asian_forward_instrument& v) {
    auto data = trade.child("FxTaRFData");
    if (!v.currency.empty())
        data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("FixingAmount", v.fixing_amount.value_or(decimal{}));
    if (v.strike.has_value())
        data.set_decimal("Strike", *v.strike);

    auto underlying = data.child("Underlying");
    underlying.set_text("Name", v.fx_index);
    underlying.set_text("Type", "FX");
    // The mapper states the direction the document it writes needs rather than
    // the one the row carries.
    data.child("OptionData").set_text("LongShort", "Long");
    data.child("ScheduleData").append("Rules").set_text("Tenor", "1Y");

    if (v.target_amount.has_value())
        push_barrier(
            data.child("Barriers").append("BarrierData"), "CumulatedProfitCap", *v.target_amount);
}

}

auto read_fx_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_fx_trade_type(trade_type))
        return std::nullopt;

    fx_instrument_data data;
    if (trade_type == "FxForward") {
        data.forwards.push_back(read_fx_forward(trade, trade_type));
    } else if (trade_type == "FxSwap") {
        data.forwards.push_back(read_fx_swap(trade));
    } else if (trade_type == "FxOption") {
        data.vanilla_options.push_back(read_fx_option(trade));
    } else if (trade_type == "FxBarrierOption") {
        data.barrier_options.push_back(
            read_barrier_product(trade, "FxBarrierOptionData", trade_type));
    } else if (trade_type == "FxDigitalOption") {
        data.digital_options.push_back(read_fx_digital_option(trade, trade_type));
    } else if (trade_type == "FxDigitalBarrierOption") {
        data.digital_options.push_back(read_fx_digital_barrier_option(trade));
    } else if (trade_type == "FxTouchOption" || trade_type == "FxDoubleTouchOption") {
        data.digital_options.push_back(read_fx_touch_option(trade, trade_type));
    } else if (trade_type == "FxVarianceSwap") {
        data.variance_swaps.push_back(read_fx_variance_swap(trade));
    } else if (trade_type == "FxAverageForward") {
        data.asian_forwards.push_back(read_fx_average_forward(trade));
    } else if (trade_type == "FxAccumulator") {
        // The range bounds are the product's child, so they land beside the
        // fact row rather than inside it.
        data.accumulators.push_back(read_fx_accumulator(data, trade));
    } else if (trade_type == "FxTaRF") {
        data.asian_forwards.push_back(read_fx_tarf(trade));
    } else if (trade_type == "FxGenericBarrierOption") {
        data.barrier_options.push_back(read_fx_generic_barrier_option(trade));
    } else if (trade_type == "FxDoubleBarrierOption") {
        data.barrier_options.push_back(
            read_barrier_product(trade, "FxDoubleBarrierOptionData", trade_type));
    } else if (trade_type == "FxEuropeanBarrierOption") {
        data.barrier_options.push_back(
            read_barrier_product(trade, "FxEuropeanBarrierOptionData", trade_type));
    } else if (trade_type == "FxKIKOBarrierOption") {
        data.barrier_options.push_back(read_fx_kiko_barrier_option(trade));
    }
    return trading::domain::trade_instrument{std::move(data)};
}

auto write_fx_instrument(xml::record_writer& trade,
                         std::string_view trade_type,
                         const trading::domain::trade_instrument& instrument,
                         const portfolio_reference_curves&) -> bool {
    const auto* v = std::get_if<fx_instrument_data>(&instrument);
    if (v == nullptr || !is_fx_trade_type(trade_type))
        return false;

    // The row's own type states which product the trade is, as the exporter's
    // visitor did, and the trade type states which element carries it.
    bool matched = false;
    trading::domain::visit_fx_products(
        [&](const auto& row) {
            using Row = std::decay_t<decltype(row)>;
            if constexpr (std::is_same_v<Row, fx_forward_instrument>) {
                if (trade_type == "FxForward") {
                    write_fx_forward(trade, row);
                    matched = true;
                } else if (trade_type == "FxSwap") {
                    write_fx_swap(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, fx_vanilla_option_instrument>) {
                write_fx_option(trade, row);
                matched = true;
            } else if constexpr (std::is_same_v<Row, fx_barrier_option_instrument>) {
                if (trade_type == "FxBarrierOption") {
                    write_barrier_product(trade, "FxBarrierOptionData", row);
                    matched = true;
                } else if (trade_type == "FxGenericBarrierOption") {
                    write_fx_generic_barrier_option(trade, row);
                    matched = true;
                } else if (trade_type == "FxDoubleBarrierOption") {
                    write_barrier_product(trade, "FxDoubleBarrierOptionData", row);
                    matched = true;
                } else if (trade_type == "FxEuropeanBarrierOption") {
                    write_barrier_product(trade, "FxEuropeanBarrierOptionData", row);
                    matched = true;
                } else if (trade_type == "FxKIKOBarrierOption") {
                    write_fx_kiko_barrier_option(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, fx_digital_option_instrument>) {
                if (trade_type == "FxDigitalOption") {
                    write_fx_digital_option(trade, row);
                    matched = true;
                } else if (trade_type == "FxDigitalBarrierOption") {
                    write_fx_digital_barrier_option(trade, row);
                    matched = true;
                } else if (trade_type == "FxTouchOption" || trade_type == "FxDoubleTouchOption") {
                    write_fx_touch_option(trade, trade_type, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, fx_asian_forward_instrument>) {
                if (trade_type == "FxAverageForward") {
                    write_fx_average_forward(trade, row);
                    matched = true;
                } else if (trade_type == "FxTaRF") {
                    write_fx_tarf(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, fx_accumulator_instrument>) {
                write_fx_accumulator(trade, row, v->range_bounds);
                matched = true;
            } else if constexpr (std::is_same_v<Row, fx_variance_swap_instrument>) {
                write_fx_variance_swap(trade, row);
                matched = true;
            }
        },
        *v);
    return matched;
}

}
