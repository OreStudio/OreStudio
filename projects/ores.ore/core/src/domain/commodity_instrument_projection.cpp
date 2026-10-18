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
constexpr std::string_view actor = "ores";
constexpr std::string_view change_reason = "system.external_data_import";
constexpr std::string_view change_commentary = "Imported from ORE XML";

/// The trade types the family states, which are the ones the old dispatcher took.
constexpr bool is_commodity_trade_type(std::string_view type) {
    return type == "CommodityForward" || type == "CommodityOption" || type == "CommoditySwap" ||
           type == "CommoditySwaption" || type == "CommodityVarianceSwap" ||
           type == "CommodityAveragePriceOption" || type == "CommodityOptionStrip" ||
           type == "CommodityBasketOption";
}

auto make_row(std::string_view trade_type_code) -> commodity_instrument {
    commodity_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = std::string(actor);
    r.audit.performed_by = std::string(actor);
    r.audit.change_reason_code = std::string(change_reason);
    r.audit.change_commentary = std::string(change_commentary);
    return r;
}

void stamp_columns(commodity_basket_constituent& row) {
    row.modified_by = std::string(actor);
    row.performed_by = std::string(actor);
    row.change_reason_code = std::string(change_reason);
    row.change_commentary = std::string(change_commentary);
}

// ---------------------------------------------------------------------------
// Reading: the values, dates and spellings a row states
// ---------------------------------------------------------------------------

auto to_domain_date(const std::string& text) -> std::optional<std::chrono::year_month_day> {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto to_ore_date(const std::optional<std::chrono::year_month_day>& d) -> std::string {
    if (!d || !d->ok())
        return {};
    return ores::platform::time::datetime::to_iso8601_date(*d);
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

auto optional_date_of(const record_reader& r, std::string_view name)
    -> std::optional<std::chrono::year_month_day> {
    return to_domain_date(text_of(r, name));
}

auto optional_decimal_of(const record_reader& r, std::string_view name) -> std::optional<decimal> {
    return r.decimal(name);
}

auto decimal_of(const record_reader& r, std::string_view name) -> decimal {
    if (const auto value = r.decimal(name))
        return *value;
    return decimal{};
}

/// The exact decimal an element of a xs:simpleContent decimal type carries.
auto own_decimal_of(const record_reader& r) -> decimal {
    if (const auto text = r.own_text()) {
        if (auto parsed = decimal::from_string(*text))
            return std::move(*parsed);
    }
    return decimal{};
}

auto own_decimal_as_double(const record_reader& r) -> double {
    return own_decimal_of(r).to_double();
}

auto option_type_of(const std::optional<record_reader>& option) -> std::string {
    if (!option)
        return {};
    return text_of(*option, "OptionType");
}

auto exercise_style_of(const std::optional<record_reader>& option) -> std::string {
    if (!option)
        return {};
    return text_of(*option, "Style");
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
    return to_domain_date(values.front());
}

/// The underlying a variance swap names, either itself or through a basket.
auto underlying_name_of(const record_reader& data) -> std::string {
    if (const auto name = data.text("Name"))
        return *name;
    if (const auto underlying = data.child("Underlying"))
        return text_of(*underlying, "Name");
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
        terms.commodity_code = text_of(*floating, "Name");
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
        terms.start_date = to_domain_date(values.front());
        terms.maturity_date = to_domain_date(values.back());
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
    r.commodity_code = text_of(*data, "Name");
    r.currency = spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.fixed_price = decimal_of(*data, "Strike");
    r.maturity_date = optional_date_of(*data, "Maturity");
    return r;
}

auto read_commodity_option(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityOption");
    const auto data = trade.child("CommodityOptionData");
    if (!data)
        return r;
    r.commodity_code = text_of(*data, "Name");
    r.currency = spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.strike_price = optional_strike_of(*data);
    r.option_type = option_type_of(data->child("OptionData"));
    r.exercise_type = exercise_style_of(data->child("OptionData"));
    r.maturity_date = first_exercise_date_of(data->child("OptionData"));
    return r;
}

auto read_commodity_swap(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommoditySwap");
    const auto data = trade.child("CommoditySwapData");
    if (!data)
        return r;
    const auto terms = floating_leg_terms(data->children("LegData"));
    r.commodity_code = terms.commodity_code;
    r.currency = terms.currency;
    r.quantity = terms.quantity;
    r.start_date = terms.start_date;
    r.maturity_date = terms.maturity_date;
    return r;
}

auto read_commodity_swaption(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommoditySwaption");
    const auto data = trade.child("CommoditySwaptionData");
    if (!data)
        return r;
    r.swaption_expiry_date = first_exercise_date_of(data->child("OptionData"));
    const auto legs = data->children("LegData");
    const auto terms = floating_leg_terms(legs);
    r.commodity_code = terms.commodity_code;
    r.currency = terms.currency;
    r.quantity = terms.quantity;
    r.start_date = terms.start_date;
    r.maturity_date = terms.maturity_date;
    r.fixed_price = first_fixed_leg_price(legs);
    return r;
}

auto read_commodity_variance_swap(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityVarianceSwap");
    const auto data = trade.child("CommodityVarianceSwapData");
    if (!data)
        return r;
    r.commodity_code = underlying_name_of(*data);
    r.currency = spelling_of(*data, "Currency");
    r.start_date = optional_date_of(*data, "StartDate");
    r.maturity_date = optional_date_of(*data, "EndDate");
    r.variance_strike = decimal_as_double(*data, "Strike").value_or(0.0);
    r.quantity = decimal_as_double(*data, "Notional").value_or(0.0);
    return r;
}

auto read_commodity_average_price_option(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityAveragePriceOption");
    const auto data = trade.child("CommodityAveragePriceOptionData");
    if (!data)
        return r;
    r.commodity_code = text_of(*data, "Name");
    r.currency = spelling_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.strike_price = decimal_of(*data, "Strike");
    r.option_type = option_type_of(data->child("OptionData"));
    r.exercise_type = exercise_style_of(data->child("OptionData"));
    r.maturity_date = first_exercise_date_of(data->child("OptionData"));
    r.averaging_start_date = optional_date_of(*data, "StartDate");
    r.averaging_end_date = optional_date_of(*data, "EndDate");
    return r;
}

auto read_commodity_option_strip(const record_reader& trade) -> commodity_instrument {
    auto r = make_row("CommodityOptionStrip");
    const auto data = trade.child("CommodityOptionStripData");
    if (!data)
        return r;
    const auto leg = data->child("LegData");
    if (!leg)
        return r;
    if (const auto floating = leg->child("CommodityFloatingLegData")) {
        r.commodity_code = text_of(*floating, "Name");
        r.quantity = quantity_of(*floating);
    }
    if (const auto currency = leg->text("Currency"))
        r.currency = *currency;
    if (const auto schedule = leg->child("ScheduleData")) {
        const auto rules = schedule->children("Rules");
        if (!rules.empty())
            r.strip_frequency_code = text_of(rules.front(), "Tenor");
    }
    return r;
}

auto read_commodity_basket_option(const record_reader& trade) -> commodity_instrument_data {
    auto inst = make_row("CommodityBasketOption");
    std::vector<commodity_basket_constituent> constituents;
    const auto data = trade.child("CommodityBasketOptionData");
    if (data) {
        inst.currency = spelling_of(*data, "Currency");
        inst.quantity = decimal_as_double(*data, "Notional").value_or(0.0);
        inst.strike_price = optional_decimal_of(*data, "Strike");
        inst.option_type = option_type_of(data->child("OptionData"));
        inst.exercise_type = exercise_style_of(data->child("OptionData"));
        inst.maturity_date = first_exercise_date_of(data->child("OptionData"));

        std::vector<record_reader> rows;
        if (const auto underlyings = data->child("Underlyings"))
            rows = underlyings->children("Underlying");
        if (!rows.empty())
            inst.commodity_code = text_of(rows.front(), "Name");

        int sequence_number = 1;
        for (const auto& underlying : rows) {
            commodity_basket_constituent row;
            row.sequence_number = sequence_number++;
            row.underlying_code = text_of(underlying, "Name");
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

auto flag_text(bool value) -> std::string_view {
    return value ? "true" : "false";
}

// The mapper names the direction rather than the row's own, because the option
// block states it and the table keeps no column for it.
void write_option_terms(record_writer option, const commodity_instrument& v) {
    option.set_text("LongShort", "Long");
    if (!v.option_type.empty())
        option.set_text("OptionType", v.option_type);
    if (!v.exercise_type.empty())
        option.set_text("Style", v.exercise_type);
    if (v.maturity_date)
        option.child("ExerciseDates").append_value("ExerciseDate", to_ore_date(v.maturity_date));
}

void write_quantity(record_writer quantities, double value) {
    quantities.append("Quantity").set_own_text(decimal_from_double(value).to_string());
}

void write_commodity_leg_header(record_writer leg,
                                std::string_view leg_type,
                                bool payer,
                                const commodity_instrument& v) {
    leg.set_spelling("LegType", leg_type, "leg type");
    leg.set_text("Payer", flag_text(payer));
    if (!v.currency.empty())
        leg.set_text("Currency", v.currency);
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

void write_commodity_forward(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityForwardData");
    data.set_spelling("Position", "Long", "position");
    data.set_text("Maturity", to_ore_date(v.maturity_date));
    data.set_text("Name", v.commodity_code);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Strike", v.fixed_price.value_or(decimal{}));
    set_decimal_from_double(data, "Quantity", v.quantity);
}

void write_commodity_option(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityOptionData");
    write_option_terms(data.child("OptionData"), v);
    data.set_text("Name", v.commodity_code);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_text("Strike", v.strike_price.value_or(decimal{}).to_string());
    set_decimal_from_double(data, "Quantity", v.quantity);
}

// The schema requires exactly two legs, and the row's fixed price is the one
// the fixed leg states rather than a leg of its own.
void write_commodity_swaption(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommoditySwaptionData");
    auto option = data.child("OptionData");
    option.set_text("LongShort", "Long");
    if (v.swaption_expiry_date)
        option.child("ExerciseDates")
            .append_value("ExerciseDate", to_ore_date(v.swaption_expiry_date));

    auto fixed_leg = data.append("LegData");
    write_commodity_leg_header(fixed_leg, "CommodityFixed", true, v);
    auto fixed_data = fixed_leg.child("CommodityFixedLegData");
    fixed_data.child("Prices").append("Price").set_own_text(
        decimal_from_double(v.fixed_price.value_or(decimal{}).to_double()).to_string());
    write_quantity(fixed_data.child("Quantities"), v.quantity);

    auto floating_leg = data.append("LegData");
    write_commodity_leg_header(floating_leg, "CommodityFloating", false, v);
    auto floating_data = floating_leg.child("CommodityFloatingLegData");
    floating_data.set_text("Name", v.commodity_code);
    floating_data.set_spelling("PriceType", "FutureSettlement", "price type");
    write_quantity(floating_data.child("Quantities"), v.quantity);
}

void write_commodity_swap(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommoditySwapData");

    auto fixed_leg = data.append("LegData");
    write_commodity_leg_header(fixed_leg, "CommodityFixed", false, v);
    auto fixed_data = fixed_leg.child("CommodityFixedLegData");
    fixed_data.child("Prices").append("Price").set_own_text(decimal{}.to_string());
    write_quantity(fixed_data.child("Quantities"), v.quantity);

    auto floating_leg = data.append("LegData");
    write_commodity_leg_header(floating_leg, "CommodityFloating", true, v);
    auto floating_data = floating_leg.child("CommodityFloatingLegData");
    floating_data.set_text("Name", v.commodity_code);
    floating_data.set_spelling("PriceType", "FutureSettlement", "price type");
    write_quantity(floating_data.child("Quantities"), v.quantity);
    if (v.start_date) {
        auto rule = write_schedule_rule(
            floating_leg.child("ScheduleData"), to_ore_date(v.start_date), "1M");
        if (v.maturity_date)
            rule.set_text("EndDate", to_ore_date(v.maturity_date));
    }
}

void write_commodity_variance_swap(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityVarianceSwapData");
    data.set_text("StartDate", to_ore_date(v.start_date));
    data.set_text("EndDate", to_ore_date(v.maturity_date));
    data.set_spelling("Currency", v.currency, "currency");
    data.set_text("Name", v.commodity_code);
    data.set_text("LongShort", "Long");
    set_decimal_from_double(data, "Strike", v.variance_strike.value_or(0.0));
    set_decimal_from_double(data, "Notional", v.quantity);
    // The document states the calendar it accrues under and the row does not,
    // so the mapper's own default is what the export writes.
    data.set_text("Calendar", "USD");
}

void write_commodity_average_price_option(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityAveragePriceOptionData");
    write_option_terms(data.child("OptionData"), v);
    data.set_text("Name", v.commodity_code);
    data.set_spelling("Currency", v.currency, "currency");
    set_decimal_from_double(data, "Quantity", v.quantity);
    set_decimal_from_double(data, "Strike", v.strike_price.value_or(decimal{}).to_double());
    data.set_spelling("PriceType", "FutureSettlement", "price type");
    data.set_text("StartDate", to_ore_date(v.averaging_start_date));
    data.set_text("EndDate", to_ore_date(v.averaging_end_date));
    data.set_text("PaymentCalendar", "US-NYSE");
    data.set_text("PaymentLag", "5");
    data.set_spelling("PaymentConvention", "Following", "payment convention");
    data.set_text("PricingCalendar", "US-NYSE");
}

void write_commodity_option_strip(record_writer& trade, const commodity_instrument& v) {
    auto data = trade.child("CommodityOptionStripData");
    auto leg = data.child("LegData");
    write_commodity_leg_header(leg, "CommodityFloating", false, v);
    auto floating_data = leg.child("CommodityFloatingLegData");
    floating_data.set_text("Name", v.commodity_code);
    floating_data.set_spelling("PriceType", "FutureSettlement", "price type");
    write_quantity(floating_data.child("Quantities"), v.quantity);
    if (!v.strip_frequency_code.empty())
        write_schedule_rule(
            leg.child("ScheduleData"), to_ore_date(v.start_date), v.strip_frequency_code);
}

void write_commodity_basket_option(record_writer& trade,
                                   const commodity_instrument& v,
                                   const std::vector<commodity_basket_constituent>& rows) {
    auto data = trade.child("CommodityBasketOptionData");
    data.set_spelling("Currency", v.currency, "currency");
    set_decimal_from_double(data, "Notional", v.quantity);
    if (v.strike_price)
        set_decimal_from_double(data, "Strike", v.strike_price->to_double());
    write_option_terms(data.child("OptionData"), v);

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
        underlying.set_text("Name", row->underlying_code);
        if (row->weight)
            set_decimal_from_double(underlying, "Weight", *row->weight);
    }
}

}

auto read_commodity_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_commodity_trade_type(trade_type))
        return std::nullopt;

    if (trade_type == "CommodityBasketOption")
        return trading::domain::trade_instrument{read_commodity_basket_option(trade)};

    const auto wrap = [](commodity_instrument row) {
        return trading::domain::trade_instrument{commodity_instrument_data{std::move(row), {}}};
    };

    if (trade_type == "CommodityForward")
        return wrap(read_commodity_forward(trade));
    if (trade_type == "CommodityOption")
        return wrap(read_commodity_option(trade));
    if (trade_type == "CommoditySwap")
        return wrap(read_commodity_swap(trade));
    if (trade_type == "CommoditySwaption")
        return wrap(read_commodity_swaption(trade));
    if (trade_type == "CommodityVarianceSwap")
        return wrap(read_commodity_variance_swap(trade));
    if (trade_type == "CommodityAveragePriceOption")
        return wrap(read_commodity_average_price_option(trade));
    return wrap(read_commodity_option_strip(trade));
}

auto write_commodity_instrument(xml::record_writer& trade,
                                std::string_view trade_type,
                                const trading::domain::trade_instrument& instrument,
                                const portfolio_reference_curves&) -> bool {
    const auto* v = std::get_if<commodity_instrument_data>(&instrument);
    if (v == nullptr || !is_commodity_trade_type(trade_type))
        return false;

    if (trade_type == "CommodityForward") {
        write_commodity_forward(trade, v->instrument);
        return true;
    }
    if (trade_type == "CommodityOption") {
        write_commodity_option(trade, v->instrument);
        return true;
    }
    if (trade_type == "CommoditySwap") {
        write_commodity_swap(trade, v->instrument);
        return true;
    }
    if (trade_type == "CommoditySwaption") {
        write_commodity_swaption(trade, v->instrument);
        return true;
    }
    if (trade_type == "CommodityVarianceSwap") {
        write_commodity_variance_swap(trade, v->instrument);
        return true;
    }
    if (trade_type == "CommodityAveragePriceOption") {
        write_commodity_average_price_option(trade, v->instrument);
        return true;
    }
    if (trade_type == "CommodityOptionStrip") {
        write_commodity_option_strip(trade, v->instrument);
        return true;
    }
    write_commodity_basket_option(trade, v->instrument, v->constituents);
    return true;
}

}
