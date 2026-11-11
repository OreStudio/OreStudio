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
#include "ores.ore.core/domain/index_reference.hpp"
#include "ores.ore.core/domain/leg_block_projection.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/domain/underlying_block_projection.hpp"
#include "ores.platform/time/datetime.hpp"
#include <algorithm>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

using ores::utility::decimal::decimal;

using trading::domain::commodity_basket_constituent;
using trading::domain::commodity_instrument;
using trading::domain::commodity_instrument_data;

using xml::record_reader;
using xml::record_writer;

/// The actor and reason every imported row states.
constexpr std::string_view commodity_instrument_projection_actor = "ores";
constexpr std::string_view commodity_instrument_projection_change_reason =
    "system.external_data_import";
constexpr std::string_view commodity_instrument_projection_change_commentary =
    "Imported from ORE XML";

auto make_row(std::string_view trade_type_code) -> commodity_instrument {
    commodity_instrument r{};
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = std::string(commodity_instrument_projection_actor);
    r.audit.performed_by = std::string(commodity_instrument_projection_actor);
    r.audit.change_reason_code = std::string(commodity_instrument_projection_change_reason);
    r.audit.change_commentary = std::string(commodity_instrument_projection_change_commentary);
    return r;
}

void stamp_columns(commodity_basket_constituent& row) {
    row.modified_by = std::string(commodity_instrument_projection_actor);
    row.performed_by = std::string(commodity_instrument_projection_actor);
    row.change_reason_code = std::string(commodity_instrument_projection_change_reason);
    row.change_commentary = std::string(commodity_instrument_projection_change_commentary);
}

// ---------------------------------------------------------------------------
// Reading: the values, dates and spellings a row states
// ---------------------------------------------------------------------------

auto commodity_instrument_projection_to_domain_date(const std::string& text)
    -> std::optional<std::chrono::year_month_day> {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto commodity_instrument_projection_to_ore_date(
    const std::optional<std::chrono::year_month_day>& d) -> std::string {
    if (!d || !d->ok())
        return {};
    return ores::platform::time::datetime::to_iso8601_date(*d);
}

auto commodity_instrument_projection_text_of(const record_reader& r,
                                             std::string_view name) -> std::string {
    if (const auto value = r.text(name))
        return *value;
    return {};
}

auto commodity_instrument_projection_spelling_of(const record_reader& r,
                                                 std::string_view name) -> std::string {
    if (const auto value = r.spelling(name))
        return *value;
    return {};
}

auto optional_date_of(const record_reader& r,
                      std::string_view name) -> std::optional<std::chrono::year_month_day> {
    return commodity_instrument_projection_to_domain_date(
        commodity_instrument_projection_text_of(r, name));
}

auto optional_decimal_of(const record_reader& r, std::string_view name) -> std::optional<decimal> {
    return r.decimal(name);
}

auto commodity_instrument_projection_decimal_of(const record_reader& r,
                                                std::string_view name) -> decimal {
    if (const auto value = r.decimal(name))
        return *value;
    return decimal{};
}

/// The exact decimal an element of a xs:simpleContent decimal type carries.
auto own_decimal_of(const record_reader& r) -> decimal {
    return r.own_decimal().value_or(decimal{});
}

auto own_decimal_as_double(const record_reader& r) -> double {
    return own_decimal_of(r).to_double();
}

auto option_type_of(const std::optional<record_reader>& option) -> std::string {
    if (!option)
        return {};
    return commodity_instrument_projection_text_of(*option, "OptionType");
}

auto exercise_style_of(const std::optional<record_reader>& option) -> std::string {
    if (!option)
        return {};
    return commodity_instrument_projection_text_of(*option, "Style");
}

auto first_exercise_date_of(const std::optional<record_reader>& option)
    -> std::optional<std::chrono::year_month_day> {
    if (!option)
        return std::nullopt;
    const auto dates = option->child("ExerciseDates");
    if (!dates)
        return std::nullopt;
    const auto values = dates->values("ExerciseDate");
    if (values.empty())
        return std::nullopt;
    return commodity_instrument_projection_to_domain_date(values.front());
}

/// The underlying a variance swap names, either itself or through a basket.
auto stated_underlying_name_of(const record_reader& data) -> std::string {
    if (const auto name = data.text("Name"))
        return *name;
    if (const auto underlying = data.child("Underlying"))
        return commodity_instrument_projection_text_of(*underlying, "Name");
    return {};
}

/// The strike a commodity option states, either as its own text or as a leaf.
auto optional_strike_of(const record_reader& data) -> std::optional<decimal> {
    if (const auto text = data.text("Strike")) {
        if (!text->empty()) {
            if (auto parsed = decimal::from_string(*text))
                return std::move(*parsed);
        }
        return std::nullopt;
    }
    if (const auto strike_data = data.child("StrikeData"))
        return optional_decimal_of(*strike_data, "Value");
    return std::nullopt;
}

/// The quantity a commodity leg states, or nothing when it states none.
auto quantity_of(const record_reader& data) -> double {
    const auto quantities = data.child("Quantities");
    if (!quantities)
        return 0.0;
    const auto rows = quantities->children("Quantity");
    if (rows.empty())
        return 0.0;
    return own_decimal_as_double(rows.front());
}

/// The terms the first commodity floating leg of a leg list states.
struct leg_terms final {
    std::string commodity_code;
    std::string currency;
    double quantity = 0.0;
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<std::chrono::year_month_day> maturity_date;
};

auto floating_leg_terms(const std::vector<record_reader>& legs) -> leg_terms {
    leg_terms terms;
    for (const auto& leg : legs) {
        const auto floating = leg.child("CommodityFloatingLegData");
        if (!floating)
            continue;
        terms.commodity_code =
            commodity_address_of(commodity_instrument_projection_text_of(*floating, "Name"));
        terms.quantity = quantity_of(*floating);
        if (const auto currency = leg.text("Currency"))
            terms.currency = *currency;

        const auto schedule = leg.child("ScheduleData");
        if (!schedule)
            break;
        const auto rules = schedule->children("Rules");
        if (!rules.empty()) {
            terms.start_date = optional_date_of(rules.front(), "StartDate");
            terms.maturity_date = optional_date_of(rules.front(), "EndDate");
            break;
        }
        const auto dates = schedule->children("Dates");
        if (dates.empty())
            break;
        const auto inner = dates.front().child("Dates");
        if (!inner)
            break;
        const auto values = inner->values("Date");
        if (values.empty())
            break;
        terms.start_date = commodity_instrument_projection_to_domain_date(values.front());
        terms.maturity_date = commodity_instrument_projection_to_domain_date(values.back());
        break;
    }
    return terms;
}

/// The first price the fixed leg of a swaption states.
auto first_fixed_leg_price(const std::vector<record_reader>& legs) -> std::optional<decimal> {
    for (const auto& leg : legs) {
        const auto fixed = leg.child("CommodityFixedLegData");
        if (!fixed)
            continue;
        const auto prices = fixed->child("Prices");
        if (!prices)
            continue;
        const auto rows = prices->children("Price");
        if (rows.empty())
            continue;
        return own_decimal_of(rows.front());
    }
    return std::nullopt;
}

// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

auto read_commodity_forward(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityForward");
    const auto data = trade.child("CommodityForwardData");
    if (!data)
        return r;
    r.commodity_code = commodity_address_of(commodity_instrument_projection_text_of(*data, "Name"));
    r.currency = commodity_instrument_projection_spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.fixed_price = commodity_instrument_projection_decimal_of(*data, "Strike");
    r.maturity_date = optional_date_of(*data, "Maturity");
    return r;
}

auto read_commodity_option(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityOption");
    const auto data = trade.child("CommodityOptionData");
    if (!data)
        return r;
    r.commodity_code = commodity_address_of(commodity_instrument_projection_text_of(*data, "Name"));
    r.currency = commodity_instrument_projection_spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.strike_price = optional_strike_of(*data);
    r.option_type = option_type_of(data->child("OptionData"));
    r.exercise_type = exercise_style_of(data->child("OptionData"));
    r.maturity_date = first_exercise_date_of(data->child("OptionData"));
    return r;
}

// ORE parses a commodity swap from SwapData, as every ORE example states it;
// the schema's CommoditySwapData is a spelling ORE does not read. The legs land
// in the shared leg rows, and the row keeps the floating leg's terms as the
// trade's summary.
auto read_commodity_swap(const record_reader& trade) -> commodity_instrument_data {
    commodity_instrument_data result{make_row("CommoditySwap"), {}};
    const auto data = trade.child("SwapData");
    if (!data)
        return result;
    auto& r = result.instrument;
    const auto terms = floating_leg_terms(data->children("LegData"));
    r.commodity_code = terms.commodity_code;
    r.currency = terms.currency;
    r.quantity = terms.quantity;
    r.start_date = terms.start_date;
    r.maturity_date = terms.maturity_date;
    leg_block::append_legs(result, *data);
    return result;
}

auto read_commodity_swaption(const record_reader& trade) -> commodity_instrument_data {
    commodity_instrument_data result{make_row("CommoditySwaption"), {}};
    const auto data = trade.child("CommoditySwaptionData");
    if (!data)
        return result;
    auto& r = result.instrument;
    r.swaption_expiry_date = first_exercise_date_of(data->child("OptionData"));
    const auto legs = data->children("LegData");
    const auto terms = floating_leg_terms(legs);
    r.commodity_code = terms.commodity_code;
    r.currency = terms.currency;
    r.quantity = terms.quantity;
    r.start_date = terms.start_date;
    r.maturity_date = terms.maturity_date;
    r.fixed_price = first_fixed_leg_price(legs);
    leg_block::append_legs(result, *data);
    return result;
}

auto read_commodity_variance_swap(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityVarianceSwap");
    const auto data = trade.child("CommodityVarianceSwapData");
    if (!data)
        return r;
    r.commodity_code = commodity_address_of(stated_underlying_name_of(*data));
    r.currency = commodity_instrument_projection_spelling_of(*data, "Currency");
    r.start_date = optional_date_of(*data, "StartDate");
    r.maturity_date = optional_date_of(*data, "EndDate");
    r.variance_strike = decimal_as_double(*data, "Strike").value_or(0.0);
    r.quantity = decimal_as_double(*data, "Notional").value_or(0.0);
    return r;
}

/*
 * A digital option states no quantity, so the row keeps the cash it pays in
 * its quantity, as the equity digital option keeps its payoff in its notional.
 */
auto read_commodity_digital_option(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityDigitalOption");
    const auto data = trade.child("CommodityDigitalOptionData");
    if (!data)
        return r;
    r.commodity_code = commodity_address_of(commodity_instrument_projection_text_of(*data, "Name"));
    r.currency = commodity_instrument_projection_spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Payoff").value_or(0.0);
    r.strike_price = optional_decimal_of(*data, "Strike");
    r.option_type = option_type_of(data->child("OptionData"));
    r.exercise_type = exercise_style_of(data->child("OptionData"));
    r.maturity_date = first_exercise_date_of(data->child("OptionData"));
    return r;
}

/// An average price option, whose amount element is its quantity or, for a digital, its payoff.
auto read_average_price_option(const record_reader& trade,
                               std::string_view trade_type,
                               std::string_view amount) -> commodity_instrument {
    auto r = make_row(trade_type);
    const auto data = trade.child(std::string(trade_type) + "Data");
    if (!data)
        return r;
    r.commodity_code = commodity_address_of(commodity_instrument_projection_text_of(*data, "Name"));
    r.currency = commodity_instrument_projection_spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, amount).value_or(0.0);
    r.strike_price = optional_decimal_of(*data, "Strike");
    r.option_type = option_type_of(data->child("OptionData"));
    r.exercise_type = exercise_style_of(data->child("OptionData"));
    r.maturity_date = first_exercise_date_of(data->child("OptionData"));
    r.averaging_start_date = optional_date_of(*data, "StartDate");
    r.averaging_end_date = optional_date_of(*data, "EndDate");
    return r;
}

auto read_commodity_average_price_option(const record_reader& trade) -> commodity_instrument {
    return read_average_price_option(trade, "CommodityAveragePriceOption", "Quantity");
}

auto read_commodity_digital_average_price_option(const record_reader& trade)
    -> commodity_instrument {
    return read_average_price_option(
        trade, "CommodityDigitalAveragePriceOption", "DigitalCashPayoff");
}

/*
 * A position states its commodities as underlyings, which land in the shared
 * underlying rows, and the row names the first of them. It states no currency,
 * because ORE takes the commodity's own.
 */
auto read_commodity_position(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityPosition");
    const auto data = trade.child("CommodityPositionData");
    if (!data)
        return r;
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    const auto underlyings = data->children("Underlying");
    if (!underlyings.empty())
        r.commodity_code = commodity_address_of(
            commodity_instrument_projection_text_of(underlyings.front(), "Name"));
    return r;
}

/*
 * A spread option is long its first leg's commodity and short its second's.
 * The legs land in the shared leg rows, and the row keeps the first leg's terms
 * and the second leg's commodity as the trade's summary.
 */
auto read_commodity_spread_option(const record_reader& trade) -> commodity_instrument_data {
    commodity_instrument_data result{make_row("CommoditySpreadOption"), {}};
    const auto data = trade.child("CommoditySpreadOptionData");
    if (!data)
        return result;
    auto& r = result.instrument;
    const auto legs = data->children("LegData");
    const auto terms = floating_leg_terms(legs);
    r.commodity_code = terms.commodity_code;
    r.currency = terms.currency;
    r.quantity = terms.quantity;
    r.start_date = terms.start_date;
    r.maturity_date = terms.maturity_date;
    if (legs.size() > 1)
        r.spread_commodity_code = floating_leg_terms({legs[1]}).commodity_code;
    r.strike_price = optional_decimal_of(*data, "SpreadStrike");
    r.option_type = option_type_of(data->child("OptionData"));
    r.exercise_type = exercise_style_of(data->child("OptionData"));
    leg_block::append_legs(result, *data);
    return result;
}

/// The delivery terms and the position land through the product's scalar terms.
auto read_intraday_power_forward(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("IntradayPowerForward");
    const auto data = trade.child("IntradayPowerForwardData");
    if (!data)
        return r;
    r.commodity_code = commodity_address_of(commodity_instrument_projection_text_of(*data, "Name"));
    r.currency = commodity_instrument_projection_spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.fixed_price = optional_decimal_of(*data, "Strike");
    r.maturity_date = optional_date_of(*data, "Maturity");
    return r;
}

/// The two sides of an option strip: the strike role each keeps and its element.
constexpr std::pair<std::string_view, std::string_view> option_strip_sides[] = {
    {"call", "Calls"},
    {"put", "Puts"},
};

auto read_commodity_option_strip(const record_reader& trade) -> commodity_instrument_data {
    commodity_instrument_data result{make_row("CommodityOptionStrip"), {}};
    auto& r = result.instrument;
    const auto data = trade.child("CommodityOptionStripData");
    if (!data)
        return result;
    const auto leg = data->child("LegData");
    if (!leg)
        return result;
    leg_block::append_leg(result, *leg, 1);
    if (const auto floating = leg->child("CommodityFloatingLegData")) {
        r.commodity_code =
            commodity_address_of(commodity_instrument_projection_text_of(*floating, "Name"));
        r.quantity = quantity_of(*floating);
    }
    if (const auto currency = leg->text("Currency"))
        r.currency = *currency;
    if (const auto schedule = leg->child("ScheduleData")) {
        const auto rules = schedule->children("Rules");
        if (!rules.empty())
            r.strip_frequency_code =
                commodity_instrument_projection_text_of(rules.front(), "Tenor");
    }
    for (const auto& [role, element] : option_strip_sides) {
        const auto side = data->child(element);
        if (!side)
            continue;
        const auto positions = side->child("LongShorts") ?
                                   side->child("LongShorts")->spellings("LongShort") :
                                   std::vector<std::string>{};
        const auto strikes = side->child("Strikes") ? side->child("Strikes")->decimals("Strike") :
                                                      std::vector<decimal>{};
        for (std::size_t i = 0; i < strikes.size(); ++i) {
            trading::domain::instrument_strike row;
            row.strike_role = std::string(role);
            row.sequence_number = static_cast<int>(i) + 1;
            row.bare_value = strikes[i];
            if (i < positions.size())
                row.long_short = positions[i];
            option_block::stamp_row(row);
            result.strikes.push_back(std::move(row));
        }
    }
    option_block::read_premiums(result, *data);
    return result;
}

auto read_commodity_basket_option(const record_reader& trade) -> commodity_instrument_data {
    auto inst = make_row("CommodityBasketOption");
    std::vector<commodity_basket_constituent> constituents;
    const auto data = trade.child("CommodityBasketOptionData");
    if (data) {
        inst.currency = commodity_instrument_projection_spelling_of(*data, "Currency");
        inst.quantity = decimal_as_double(*data, "Notional").value_or(0.0);
        inst.strike_price = optional_decimal_of(*data, "Strike");
        inst.option_type = option_type_of(data->child("OptionData"));
        inst.exercise_type = exercise_style_of(data->child("OptionData"));
        inst.maturity_date = first_exercise_date_of(data->child("OptionData"));

        std::vector<record_reader> rows;
        if (const auto underlyings = data->child("Underlyings"))
            rows = underlyings->children("Underlying");
        if (!rows.empty())
            inst.commodity_code =
                commodity_address_of(commodity_instrument_projection_text_of(rows.front(), "Name"));

        int sequence_number = 1;
        for (const auto& underlying : rows) {
            commodity_basket_constituent row;
            row.sequence_number = sequence_number++;
            row.underlying_code =
                commodity_address_of(commodity_instrument_projection_text_of(underlying, "Name"));
            row.weight = decimal_as_double(underlying, "Weight");
            stamp_columns(row);
            constituents.push_back(std::move(row));
        }
    }
    return commodity_instrument_data{std::move(inst), std::move(constituents)};
}

// ---------------------------------------------------------------------------
// Writing: the shared shapes
// ---------------------------------------------------------------------------

auto commodity_instrument_projection_flag_text(bool value) -> std::string_view {
    return value ? "true" : "false";
}

// The mapper names the direction rather than the row's own, because the option
// block states it and the table keeps no column for it.
void write_option_terms(record_writer option,
                        const commodity_instrument& v,
                        const std::optional<std::chrono::year_month_day>& exercise) {
    option.set_text("LongShort", "Long");
    if (!v.option_type.empty())
        option.set_text("OptionType", v.option_type);
    if (!v.exercise_type.empty())
        option.set_text("Style", v.exercise_type);
    if (exercise)
        option.child("ExerciseDates")
            .append_value("ExerciseDate", commodity_instrument_projection_to_ore_date(exercise));
}

void write_quantity(record_writer quantities, double value) {
    quantities.append("Quantity").set_own_text(decimal_from_double(value).to_string());
}

void write_commodity_leg_header(record_writer leg,
                                std::string_view leg_type,
                                bool payer,
                                const commodity_instrument& v) {
    leg.set_spelling("LegType", leg_type, "leg type");
    leg.set_text("Payer", commodity_instrument_projection_flag_text(payer));
    if (!v.currency.empty())
        leg.set_text("Currency", v.currency);
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

void write_commodity_forward(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityForwardData");
    data.set_spelling("Position", "Long", "position");
    data.set_text("Maturity", commodity_instrument_projection_to_ore_date(v.maturity_date));
    data.set_text("Name", underlying_name_of(v.commodity_code));
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Strike", v.fixed_price.value_or(decimal{}));
    set_decimal_from_double(data, "Quantity", v.quantity);
}

void write_commodity_option(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityOptionData");
    write_option_terms(data.child("OptionData"), v, v.maturity_date);
    data.set_text("Name", underlying_name_of(v.commodity_code));
    data.set_spelling("Currency", v.currency, "currency");
    data.set_text("Strike", v.strike_price.value_or(decimal{}).to_string());
    set_decimal_from_double(data, "Quantity", v.quantity);
}

// The schema requires exactly two legs, and the row's fixed price is the one
// the fixed leg states rather than a leg of its own.
void write_commodity_swaption(record_writer& trade, const commodity_instrument_data& carrier) {
    const auto& v = carrier.instrument;
    auto data = trade.child("CommoditySwaptionData");
    auto option = data.child("OptionData");
    option.set_text("LongShort", "Long");
    if (v.swaption_expiry_date)
        option.child("ExerciseDates")
            .append_value("ExerciseDate",
                          commodity_instrument_projection_to_ore_date(v.swaption_expiry_date));
    if (!carrier.legs.empty()) {
        leg_block::write_legs(data, v.start_date, v.maturity_date, carrier);
        return;
    }

    auto fixed_leg = data.append("LegData");
    write_commodity_leg_header(fixed_leg, "CommodityFixed", true, v);
    auto fixed_data = fixed_leg.child("CommodityFixedLegData");
    fixed_data.child("Prices").append("Price").set_own_text(
        decimal_from_double(v.fixed_price.value_or(decimal{}).to_double()).to_string());
    write_quantity(fixed_data.child("Quantities"), v.quantity);

    auto floating_leg = data.append("LegData");
    write_commodity_leg_header(floating_leg, "CommodityFloating", false, v);
    auto floating_data = floating_leg.child("CommodityFloatingLegData");
    floating_data.set_text("Name", underlying_name_of(v.commodity_code));
    floating_data.set_spelling("PriceType", "FutureSettlement", "price type");
    write_quantity(floating_data.child("Quantities"), v.quantity);
}

void write_commodity_swap(record_writer& trade, const commodity_instrument_data& carrier) {
    auto data = trade.child("SwapData");
    if (!carrier.legs.empty()) {
        leg_block::write_legs(
            data, carrier.instrument.start_date, carrier.instrument.maturity_date, carrier);
        return;
    }

    // A swap stored before its legs were rows states only its summary, so its
    // legs are rebuilt from it.
    const auto& v = carrier.instrument;

    auto fixed_leg = data.append("LegData");
    write_commodity_leg_header(fixed_leg, "CommodityFixed", false, v);
    auto fixed_data = fixed_leg.child("CommodityFixedLegData");
    fixed_data.child("Prices").append("Price").set_own_text(decimal{}.to_string());
    write_quantity(fixed_data.child("Quantities"), v.quantity);

    auto floating_leg = data.append("LegData");
    write_commodity_leg_header(floating_leg, "CommodityFloating", true, v);
    auto floating_data = floating_leg.child("CommodityFloatingLegData");
    floating_data.set_text("Name", underlying_name_of(v.commodity_code));
    floating_data.set_spelling("PriceType", "FutureSettlement", "price type");
    write_quantity(floating_data.child("Quantities"), v.quantity);
    if (v.start_date) {
        auto rule = write_schedule_rule(floating_leg.child("ScheduleData"),
                                        commodity_instrument_projection_to_ore_date(v.start_date),
                                        "1M");
        if (v.maturity_date)
            rule.set_text("EndDate", commodity_instrument_projection_to_ore_date(v.maturity_date));
    }
}

void write_commodity_variance_swap(record_writer& trade, const commodity_instrument_data& carrier) {
    const auto& v = carrier.instrument;
    auto data = trade.child("CommodityVarianceSwapData");
    data.set_text("StartDate", commodity_instrument_projection_to_ore_date(v.start_date));
    data.set_text("EndDate", commodity_instrument_projection_to_ore_date(v.maturity_date));
    data.set_spelling("Currency", v.currency, "currency");
    if (!underlying_block::write_underlyings(data, "Underlying", carrier, "underlying"))
        data.set_text("Name", underlying_name_of(v.commodity_code));
    data.set_text("LongShort", "Long");
    set_decimal_from_double(data, "Strike", v.variance_strike.value_or(0.0));
    set_decimal_from_double(data, "Notional", v.quantity);
    // The document states the calendar it accrues under and the row does not,
    // so the mapper's own default is what the export writes.
    data.set_text("Calendar", "USD");
}

void write_commodity_digital_option(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityDigitalOptionData");
    write_option_terms(data.child("OptionData"), v, v.maturity_date);
    data.set_text("Name", underlying_name_of(v.commodity_code));
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Strike", v.strike_price.value_or(decimal{}));
    set_decimal_from_double(data, "Payoff", v.quantity);
}

void write_average_price_option(record_writer& trade,
                                std::string_view trade_type,
                                std::string_view amount,
                                const commodity_instrument& v) {
    auto data = trade.child(std::string(trade_type) + "Data");
    write_option_terms(data.child("OptionData"), v, v.maturity_date);
    data.set_text("Name", underlying_name_of(v.commodity_code));
    data.set_spelling("Currency", v.currency, "currency");
    set_decimal_from_double(data, amount, v.quantity);
    data.set_decimal("Strike", v.strike_price.value_or(decimal{}));
    data.set_spelling("PriceType", "FutureSettlement", "price type");
    data.set_text("StartDate", commodity_instrument_projection_to_ore_date(v.averaging_start_date));
    data.set_text("EndDate", commodity_instrument_projection_to_ore_date(v.averaging_end_date));
    data.set_text("PaymentCalendar", "US-NYSE");
    data.set_text("PaymentLag", "5");
    data.set_spelling("PaymentConvention", "Following", "payment convention");
    data.set_text("PricingCalendar", "US-NYSE");
}

void write_commodity_average_price_option(record_writer& trade, const commodity_instrument& v) {
    write_average_price_option(trade, "CommodityAveragePriceOption", "Quantity", v);
}

void write_commodity_digital_average_price_option(record_writer& trade,
                                                  const commodity_instrument& v) {
    write_average_price_option(trade, "CommodityDigitalAveragePriceOption", "DigitalCashPayoff", v);
}

void write_commodity_position(record_writer& trade, const commodity_instrument_data& carrier) {
    const auto& v = carrier.instrument;
    auto data = trade.child("CommodityPositionData");
    set_decimal_from_double(data, "Quantity", v.quantity);
    if (underlying_block::write_underlyings(data, "Underlying", carrier, "underlying"))
        return;
    auto underlying = data.append("Underlying");
    underlying.set_text("Type", "Commodity");
    underlying.set_text("Name", underlying_name_of(v.commodity_code));
}

/*
 * ORE takes each option's expiry from its legs' last pricing date, so the
 * option block states no exercise date.
 */
void write_commodity_spread_option(record_writer& trade, const commodity_instrument_data& carrier) {
    const auto& v = carrier.instrument;
    auto data = trade.child("CommoditySpreadOptionData");
    leg_block::write_legs(data, v.start_date, v.maturity_date, carrier);
    write_option_terms(data.child("OptionData"), v, std::nullopt);
    data.set_decimal("SpreadStrike", v.strike_price.value_or(decimal{}));
}

/*
 * ORE requires the position before the maturity, so the writer states it in
 * schema order here, and the scalar terms that run after it write the row's
 * own position over it.
 */
void write_intraday_power_forward(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("IntradayPowerForwardData");
    data.set_spelling("Position", "Long", "position");
    data.set_text("Maturity", commodity_instrument_projection_to_ore_date(v.maturity_date));
    data.set_text("Name", underlying_name_of(v.commodity_code));
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Strike", v.fixed_price.value_or(decimal{}));
    set_decimal_from_double(data, "Quantity", v.quantity);
}

void write_commodity_option_strip(record_writer& trade, const commodity_instrument_data& carrier) {
    const auto& v = carrier.instrument;
    auto data = trade.child("CommodityOptionStripData");
    auto leg = data.child("LegData");
    for (const auto& [role, element] : option_strip_sides) {
        std::vector<const trading::domain::instrument_strike*> side;
        for (const auto& row : carrier.strikes)
            if (row.strike_role == role)
                side.push_back(&row);
        if (side.empty())
            continue;
        std::sort(side.begin(), side.end(), [](const auto* a, const auto* b) {
            return a->sequence_number < b->sequence_number;
        });
        auto holder = data.child(element);
        auto positions = holder.child("LongShorts");
        auto strikes = holder.child("Strikes");
        for (const auto* row : side) {
            if (row->long_short)
                positions.append_spelling("LongShort", *row->long_short, "long short");
            strikes.append_decimal("Strike", row->bare_value.value_or(decimal{}));
        }
    }
    option_block::write_premiums(data, carrier);
    if (!carrier.legs.empty()) {
        leg_block::write_leg(leg, v.start_date, v.maturity_date, carrier.legs.front(), carrier);
        return;
    }
    write_commodity_leg_header(leg, "CommodityFloating", false, v);
    auto floating_data = leg.child("CommodityFloatingLegData");
    floating_data.set_text("Name", underlying_name_of(v.commodity_code));
    floating_data.set_spelling("PriceType", "FutureSettlement", "price type");
    write_quantity(floating_data.child("Quantities"), v.quantity);
    if (!v.strip_frequency_code.empty())
        write_schedule_rule(leg.child("ScheduleData"),
                            commodity_instrument_projection_to_ore_date(v.start_date),
                            v.strip_frequency_code);
}

void write_commodity_basket_option(record_writer& trade, const commodity_instrument_data& carrier) {
    const auto& v = carrier.instrument;
    const auto& rows = carrier.constituents;
    auto data = trade.child("CommodityBasketOptionData");
    data.set_spelling("Currency", v.currency, "currency");
    set_decimal_from_double(data, "Notional", v.quantity);
    if (v.strike_price)
        data.set_decimal("Strike", *v.strike_price);
    write_option_terms(data.child("OptionData"), v, v.maturity_date);

    std::vector<const commodity_basket_constituent*> ordered;
    ordered.reserve(rows.size());
    for (const auto& row : rows)
        ordered.push_back(&row);
    std::sort(ordered.begin(), ordered.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    if (ordered.empty())
        return;
    auto underlyings = data.child("Underlyings");
    for (const auto* row : ordered) {
        auto underlying = underlyings.append("Underlying");
        underlying.set_text("Type", "Commodity");
        underlying.set_text("Name", underlying_name_of(row->underlying_code));
        if (row->weight)
            set_decimal_from_double(underlying, "Weight", *row->weight);
    }
}

// ---------------------------------------------------------------------------
// The products the family states
// ---------------------------------------------------------------------------

template <auto Read>
auto commodity_row_only(const record_reader& trade) -> commodity_instrument_data {
    return commodity_instrument_data{Read(trade), {}};
}

template <auto Write>
void commodity_of_row(record_writer& trade, const commodity_instrument_data& carrier) {
    Write(trade, carrier.instrument);
}

/**
 * @brief A commodity product: its trade type, how it is read and written, and
 * whether its element states an OptionData block.
 *
 * The option block is listed rather than probed, because asking a record type
 * for a field it does not declare is an error.
 */
struct commodity_product final {
    std::string_view trade_type;
    commodity_instrument_data (*read)(const record_reader&);
    void (*write)(record_writer&, const commodity_instrument_data&);
    bool states_option_data;
};

constexpr commodity_product commodity_products[] = {
    {"CommodityForward",
     commodity_row_only<read_commodity_forward>,
     commodity_of_row<write_commodity_forward>,
     false},
    {"CommodityOption",
     commodity_row_only<read_commodity_option>,
     commodity_of_row<write_commodity_option>,
     true},
    {"CommodityDigitalOption",
     commodity_row_only<read_commodity_digital_option>,
     commodity_of_row<write_commodity_digital_option>,
     true},
    {"CommoditySwap", read_commodity_swap, write_commodity_swap, false},
    {"CommoditySwaption", read_commodity_swaption, write_commodity_swaption, true},
    {"CommodityVarianceSwap",
     commodity_row_only<read_commodity_variance_swap>,
     write_commodity_variance_swap,
     false},
    {"CommodityAveragePriceOption",
     commodity_row_only<read_commodity_average_price_option>,
     commodity_of_row<write_commodity_average_price_option>,
     true},
    {"CommodityDigitalAveragePriceOption",
     commodity_row_only<read_commodity_digital_average_price_option>,
     commodity_of_row<write_commodity_digital_average_price_option>,
     true},
    {"CommodityOptionStrip", read_commodity_option_strip, write_commodity_option_strip, false},
    {"CommodityBasketOption", read_commodity_basket_option, write_commodity_basket_option, true},
    {"CommodityPosition",
     commodity_row_only<read_commodity_position>,
     write_commodity_position,
     false},
    {"CommoditySpreadOption", read_commodity_spread_option, write_commodity_spread_option, true},
    {"IntradayPowerForward",
     commodity_row_only<read_intraday_power_forward>,
     commodity_of_row<write_intraday_power_forward>,
     false},
};

const commodity_product* commodity_product_of(std::string_view trade_type) {
    for (const auto& product : commodity_products)
        if (product.trade_type == trade_type)
            return &product;
    return nullptr;
}

}

/// The single-valued terms a commodity product row keeps, by its element.
constexpr scalar_terms::term<commodity_instrument> commodity_terms[] = {
    {"Calendar", &commodity_instrument::calendar},
    {"MomentType", &commodity_instrument::moment_type},
    {"AddPastDividends", &commodity_instrument::add_past_dividends},
    {"CommodityPayRelativeTo", &commodity_instrument::pay_relative_to},
    {"CommodityQuantityFrequency", &commodity_instrument::quantity_frequency},
    {"DeliveryRollDays", &commodity_instrument::delivery_roll_days},
    {"FutureMonthOffset", &commodity_instrument::future_month_offset},
    {"IncludePeriodEnd", &commodity_instrument::include_period_end},
    {"Position", &commodity_instrument::position},
    {"DeliveryDate", &commodity_instrument::delivery_date},
    {"DeliveryStart", &commodity_instrument::delivery_start},
    {"DeliveryEnd", &commodity_instrument::delivery_end},
};

/// Where a commodity product states its underlyings.
constexpr underlying_block::underlying_holder commodity_underlying_holders[] = {
    {"CommodityVarianceSwap", "", "Underlying", "underlying"},
    {"CommodityPosition", "", "Underlying", "underlying"},
};

auto read_commodity_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    const auto* product = commodity_product_of(trade_type);
    if (product == nullptr)
        return std::nullopt;

    auto data = product->read(trade);
    const auto element = std::string(trade_type) + "Data";
    // The product's row keeps its own terms; the block's shared terms land in
    // the option rows every option family reads and writes.
    if (product->states_option_data) {
        if (const auto holder = trade.child(element)) {
            if (const auto option = holder->child("OptionData"))
                option_block::read_option_block(data, *option);
        }
    }
    underlying_block::read_product_underlyings(
        data, trade, trade_type, commodity_underlying_holders);
    if (trade.declares(element))
        if (const auto holder = trade.child(element))
            scalar_terms::read_terms(data.instrument, *holder, commodity_terms);
    return trading::domain::trade_instrument{std::move(data)};
}

auto write_commodity_instrument(xml::record_writer& trade,
                                std::string_view trade_type,
                                const trading::domain::trade_instrument& instrument,
                                const ores::marketdata::projection::ore_projection&) -> bool {
    const auto* v = std::get_if<commodity_instrument_data>(&instrument);
    const auto* product = commodity_product_of(trade_type);
    if (v == nullptr || product == nullptr)
        return false;

    product->write(trade, *v);
    const auto element = std::string(trade_type) + "Data";
    if (trade.declares(element))
        if (auto holder = trade.chosen_child(element))
            scalar_terms::write_terms(*holder, v->instrument, commodity_terms);
    if (!v->options.empty() && product->states_option_data) {
        if (auto holder = trade.chosen_child(element)) {
            auto option = holder->child("OptionData");
            option_block::write_option_block(option, *v);
        }
    }
    return true;
}

}
