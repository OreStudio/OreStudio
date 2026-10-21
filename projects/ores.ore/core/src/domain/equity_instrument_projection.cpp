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
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
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
#include <variant>
#include <vector>

namespace ores::ore::domain {

namespace {

using ores::utility::decimal::decimal;

using trading::domain::equity_accumulator_instrument;
using trading::domain::equity_asian_option_instrument;
using trading::domain::equity_barrier_option_instrument;
using trading::domain::equity_digital_option_instrument;
using trading::domain::equity_forward_instrument;
using trading::domain::equity_instrument_data;
using trading::domain::equity_option_instrument;
using trading::domain::equity_position_instrument;
using trading::domain::equity_position_option_underlying;
using trading::domain::equity_swap_instrument;
using trading::domain::equity_variance_swap_instrument;

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
constexpr bool is_equity_trade_type(std::string_view type) {
    return type == "EquityPosition" || type == "EquityOptionPosition" || type == "EquityOption" ||
           type == "EquityForward" || type == "EquitySwap" || type == "EquityVarianceSwap" ||
           type == "EquityBarrierOption" || type == "EquityAsianOption" ||
           type == "EquityDigitalOption" || type == "EquityTouchOption" ||
           type == "EquityOutperformanceOption" || type == "EquityAccumulator" ||
           type == "EquityTaRF" || type == "EquityCliquetOption" ||
           type == "EquityWorstOfBasketSwap" || type == "EquityDoubleBarrierOption" ||
           type == "EquityEuropeanBarrierOption";
}

template <typename Row>
void stamp_audit(Row& row) {
    row.audit.modified_by = std::string(actor);
    row.audit.performed_by = std::string(actor);
    row.audit.change_reason_code = std::string(change_reason);
    row.audit.change_commentary = std::string(change_commentary);
}

void stamp_columns(equity_position_option_underlying& row) {
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

auto to_ore_date(const std::chrono::year_month_day& d) -> std::string {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
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

/// The exact decimal an element of a xs:simpleContent decimal type carries.
auto own_decimal_of(const record_reader& r) -> decimal {
    return r.own_decimal().value_or(decimal{});
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

auto exercise_style_of(const std::optional<record_reader>& option) -> std::string {
    if (!option)
        return {};
    return text_of(*option, "Style");
}

/**
 * @brief The underlying a product names.
 *
 * ORE states an underlying through a substitution group, so the document names
 * either the underlying itself or a basket; only the two forms that carry one
 * name are read, which is the pair the old mapper read.
 */
auto underlying_name_of(const record_reader& data) -> std::string {
    if (const auto name = data.text("Name"))
        return *name;
    if (const auto underlying = data.child("Underlying"))
        return text_of(*underlying, "Name");
    return {};
}

/// The strike the document states, either as its own digits or as a leaf.
auto strike_of(const record_reader& data) -> decimal {
    if (const auto text = data.text("Strike")) {
        if (!text->empty()) {
            if (auto parsed = decimal::from_string(*text))
                return std::move(*parsed);
        }
    }
    if (const auto strike_data = data.child("StrikeData"))
        return decimal_of(*strike_data, "Value");
    return decimal{};
}

auto barrier_levels_of(const record_reader& barrier) -> std::vector<decimal> {
    const auto levels = barrier.child("Levels");
    if (!levels)
        return {};
    return levels->decimals("Level");
}

auto json_escape(std::string_view text) -> std::string {
    std::string out;
    for (const char c : text) {
        if (c == '"')
            out += "\\\"";
        else if (c == '\\')
            out += "\\\\";
        else
            out += c;
    }
    return out;
}

auto numbers_json(const std::vector<decimal>& values) -> std::string {
    std::string out = "[";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i != 0)
            out.push_back(',');
        out += values[i].to_string();
    }
    out += "]";
    return out;
}

auto names_json(const std::vector<std::string>& values) -> std::string {
    std::string out = "[";
    for (std::size_t i = 0; i < values.size(); ++i) {
        if (i != 0)
            out.push_back(',');
        out += "\"" + json_escape(values[i]) + "\"";
    }
    out += "]";
    return out;
}

// ---------------------------------------------------------------------------
// Reading: the accumulator's range bounds, which the table keeps as JSON text
// ---------------------------------------------------------------------------

/**
 * @brief A range bound as an accumulator stores it.
 *
 * The table has no child table for a range bound, so the list lives in one JSON
 * column. Each entry states only the fields the document states.
 */
struct range_bound_text final {
    std::optional<std::string> start_date;
    std::optional<decimal> range_from;
    std::optional<decimal> range_to;
    std::optional<decimal> leverage;
    std::optional<decimal> strike;
    std::optional<decimal> strike_adjustment;
};

auto range_bounds_json(const record_reader& data) -> std::string {
    const auto bounds = data.child("RangeBounds");
    if (!bounds)
        return {};
    const auto rows = bounds->children("RangeBound");
    if (rows.empty())
        return {};

    std::string out = "[";
    for (std::size_t i = 0; i < rows.size(); ++i) {
        if (i != 0)
            out.push_back(',');
        std::string fields;
        const auto add = [&fields](std::string_view key, const std::string& value) {
            if (!fields.empty())
                fields.push_back(',');
            fields += "\"" + std::string(key) + "\":" + value;
        };
        if (const auto start = rows[i].text("startDate"))
            add("StartDate", "\"" + json_escape(*start) + "\"");
        if (const auto value = rows[i].decimal("RangeFrom"))
            add("RangeFrom", value->to_string());
        if (const auto value = rows[i].decimal("RangeTo"))
            add("RangeTo", value->to_string());
        if (const auto value = rows[i].decimal("Leverage"))
            add("Leverage", value->to_string());
        if (const auto value = rows[i].decimal("Strike"))
            add("Strike", value->to_string());
        if (const auto value = rows[i].decimal("StrikeAdjustment"))
            add("StrikeAdjustment", value->to_string());
        out += "{" + fields + "}";
    }
    out += "]";
    return out;
}

auto parse_range_bounds(std::string_view json) -> std::vector<range_bound_text> {
    std::vector<range_bound_text> rows;
    std::size_t i = 0;
    while (i < json.size()) {
        const auto open = json.find('{', i);
        if (open == std::string_view::npos)
            break;
        const auto close = json.find('}', open);
        if (close == std::string_view::npos)
            break;
        range_bound_text row;
        auto p = open + 1;
        while (p < close) {
            const auto key_open = json.find('"', p);
            if (key_open == std::string_view::npos || key_open >= close)
                break;
            const auto key_close = json.find('"', key_open + 1);
            if (key_close == std::string_view::npos)
                break;
            const auto key = std::string(json.substr(key_open + 1, key_close - key_open - 1));
            const auto colon = json.find(':', key_close);
            if (colon == std::string_view::npos || colon >= close)
                break;
            auto value_at = colon + 1;
            while (value_at < close && (json[value_at] == ' ' || json[value_at] == '\t'))
                ++value_at;
            if (value_at < close && json[value_at] == '"') {
                const auto value_open = value_at + 1;
                const auto value_close = json.find('"', value_open);
                if (value_close == std::string_view::npos)
                    break;
                if (key == "StartDate")
                    row.start_date = std::string(json.substr(value_open, value_close - value_open));
                p = value_close + 1;
                continue;
            }
            auto value_end = json.find(',', value_at);
            if (value_end == std::string_view::npos || value_end > close)
                value_end = close;
            auto parsed = decimal::from_string(json.substr(value_at, value_end - value_at));
            if (parsed) {
                if (key == "RangeFrom")
                    row.range_from = *parsed;
                else if (key == "RangeTo")
                    row.range_to = *parsed;
                else if (key == "Leverage")
                    row.leverage = *parsed;
                else if (key == "Strike")
                    row.strike = *parsed;
                else if (key == "StrikeAdjustment")
                    row.strike_adjustment = *parsed;
            }
            p = value_end;
        }
        rows.push_back(std::move(row));
        i = close + 1;
    }
    return rows;
}

// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

auto read_equity_option(const record_reader& trade,
                        std::string_view trade_type) -> equity_option_instrument {
    auto r = make_row<equity_option_instrument>(trade_type);
    const auto data = trade.child("EquityOptionData");
    if (!data)
        return r;
    r.underlying_name = underlying_name_of(*data);
    r.currency = text_of(*data, "Currency");
    r.notional = decimal_of(*data, "Quantity");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.exercise_type = exercise_style_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = strike_of(*data);
    return r;
}

auto read_equity_forward(const record_reader& trade) -> equity_forward_instrument {
    auto r = make_row<equity_forward_instrument>("EquityForward");
    const auto data = trade.child("EquityForwardData");
    if (!data)
        return r;
    r.underlying_name = underlying_name_of(*data);
    r.currency = text_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.forward_price = optional_decimal_of(*data, "Strike");
    r.expiry_date = to_domain_date(text_of(*data, "Maturity"));
    return r;
}

// The equity leg states the underlying and the return, the funding leg the
// currency, the notional and the schedule; a leg that states both overwrites
// the other's values in the order the document wrote them.
auto read_equity_swap(const record_reader& trade) -> equity_swap_instrument {
    auto r = make_row<equity_swap_instrument>("EquitySwap");
    const auto data = trade.child("EquitySwapData");
    if (!data)
        return r;
    for (const auto& leg : data->children("LegData")) {
        if (const auto equity = leg.child("EquityLegData")) {
            r.underlying_name = underlying_name_of(*equity);
            r.return_type = text_of(*equity, "ReturnType");
            if (const auto quantity = optional_decimal_of(*equity, "Quantity"))
                r.notional = *quantity;
        } else {
            if (const auto currency = leg.text("Currency"))
                r.currency = *currency;
            if (const auto notionals = leg.child("Notionals")) {
                const auto rows = notionals->children("Notional");
                if (!rows.empty())
                    r.notional = own_decimal_of(rows.front());
            }
            if (const auto schedule = leg.child("ScheduleData")) {
                const auto rules = schedule->children("Rules");
                if (!rules.empty()) {
                    r.start_date = to_domain_date(text_of(rules.front(), "StartDate"));
                    if (const auto end = rules.front().text("EndDate"))
                        r.maturity_date = to_domain_date(*end);
                    r.payment_frequency_code =
                        tenor_to_payment_frequency(text_of(rules.front(), "Tenor"));
                }
            }
        }
    }
    return r;
}

auto read_equity_variance_swap(const record_reader& trade) -> equity_variance_swap_instrument {
    auto r = make_row<equity_variance_swap_instrument>("EquityVarianceSwap");
    const auto data = trade.child("EquityVarianceSwapData");
    if (!data)
        return r;
    r.underlying_name = underlying_name_of(*data);
    r.currency = spelling_of(*data, "Currency");
    r.notional = decimal_of(*data, "Notional");
    r.variance_strike = decimal_as_double(*data, "Strike").value_or(0.0);
    r.start_date = to_domain_date(text_of(*data, "StartDate"));
    r.maturity_date = to_domain_date(text_of(*data, "EndDate"));
    return r;
}

// The three barrier products state one barrier term each under an element of
// their own, so the trade type states which element carries it.
auto read_equity_barrier_option(const record_reader& trade,
                                std::string_view element,
                                std::string_view trade_type) -> equity_barrier_option_instrument {
    auto r = make_row<equity_barrier_option_instrument>(trade_type);
    const auto data = trade.child(element);
    if (!data)
        return r;
    r.underlying_name = underlying_name_of(*data);
    r.currency = spelling_of(*data, "Currency");
    r.notional = decimal_of(*data, "Quantity");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.exercise_type = exercise_style_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = strike_of(*data);
    if (const auto barrier = data->child("BarrierData")) {
        r.lower_barrier_type = spelling_of(*barrier, "Type");
        const auto levels = barrier_levels_of(*barrier);
        if (!levels.empty())
            r.lower_barrier = levels.front();
        if (levels.size() > 1 && levels[1] > decimal{})
            r.upper_barrier = levels[1];
    }
    return r;
}

auto read_equity_asian_option(const record_reader& trade) -> equity_asian_option_instrument {
    auto r = make_row<equity_asian_option_instrument>("EquityAsianOption");
    const auto data = trade.child("EquityAsianOptionData");
    if (!data)
        return r;
    if (const auto underlying = data->child("Underlying"))
        r.underlying_name = text_of(*underlying, "Name");
    r.currency = spelling_of(*data, "Currency");
    r.notional = decimal_of(*data, "Quantity");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = strike_of(*data);
    if (const auto dates = data->child("ObservationDates")) {
        const auto rules = dates->children("Rules");
        if (!rules.empty())
            r.averaging_start_date = to_domain_date(text_of(rules.front(), "StartDate"));
    }
    return r;
}

auto read_equity_digital_option(const record_reader& trade) -> equity_digital_option_instrument {
    auto r = make_row<equity_digital_option_instrument>("EquityDigitalOption");
    const auto data = trade.child("EquityDigitalOptionData");
    if (!data)
        return r;
    r.underlying_name = underlying_name_of(*data);
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = optional_decimal_of(*data, "Strike");
    r.notional = decimal_of(*data, "PayoffAmount");
    r.currency = spelling_of(*data, "PayoffCurrency");
    return r;
}

// A touch option is the digital row with a barrier instead of a strike, which
// is the shape the old mapper gave it.
auto read_equity_touch_option(const record_reader& trade) -> equity_digital_option_instrument {
    auto r = make_row<equity_digital_option_instrument>("EquityTouchOption");
    const auto data = trade.child("EquityTouchOptionData");
    if (!data)
        return r;
    r.underlying_name = underlying_name_of(*data);
    r.currency = spelling_of(*data, "PayoffCurrency");
    r.notional = decimal_of(*data, "PayoffAmount");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    if (const auto barrier = data->child("BarrierData")) {
        r.barrier_type = spelling_of(*barrier, "Type");
        const auto levels = barrier_levels_of(*barrier);
        if (!levels.empty() && levels.front() > decimal{})
            r.barrier_level = levels.front();
    }
    return r;
}

auto read_equity_outperformance_option(const record_reader& trade) -> equity_option_instrument {
    auto r = make_row<equity_option_instrument>("EquityOutperformanceOption");
    const auto data = trade.child("EquityOutperformanceOptionData");
    if (!data)
        return r;
    r.currency = spelling_of(*data, "Currency");
    r.notional = decimal_of(*data, "Notional");
    const auto option = data->child("OptionData");
    r.option_type = option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = decimal_of(*data, "StrikeReturn");

    // The option row has no basket column, so the pair travels as one name.
    const auto first = data->child("Underlying1");
    const auto second = data->child("Underlying2");
    r.underlying_name = (first ? text_of(*first, "Name") : std::string{}) + "/" +
                        (second ? text_of(*second, "Name") : std::string{});
    return r;
}

auto read_equity_accumulator(const record_reader& trade) -> equity_accumulator_instrument {
    auto r = make_row<equity_accumulator_instrument>("EquityAccumulator");
    const auto data = trade.child("EquityAccumulatorData");
    if (!data)
        return r;
    if (const auto underlying = data->child("Underlying"))
        r.underlying_name = text_of(*underlying, "Name");
    r.currency = spelling_of(*data, "Currency");
    r.fixing_amount = decimal_of(*data, "FixingAmount");
    r.strike = decimal_of(*data, "Strike");
    if (const auto start = data->text("StartDate"))
        r.start_date = to_domain_date(*start);
    if (const auto barriers = data->child("Barriers")) {
        for (const auto& barrier : barriers->children("BarrierData")) {
            const auto type = spelling_of(barrier, "Type");
            if (type != "DownAndOut" && type != "UpAndOut")
                continue;
            const auto levels = barrier_levels_of(barrier);
            if (levels.empty())
                continue;
            r.knock_out_level = levels.front();
            break;
        }
    }
    r.range_bounds_json = range_bounds_json(*data);
    return r;
}

auto read_equity_tarf(const record_reader& trade) -> equity_accumulator_instrument {
    auto r = make_row<equity_accumulator_instrument>("EquityTaRF");
    const auto data = trade.child("EquityTaRFData");
    if (!data)
        return r;
    if (const auto underlying = data->child("Underlying"))
        r.underlying_name = text_of(*underlying, "Name");
    r.currency = spelling_of(*data, "Currency");
    r.fixing_amount = decimal_of(*data, "FixingAmount");
    r.strike = decimal_of(*data, "Strike");
    if (const auto barriers = data->child("Barriers")) {
        for (const auto& barrier : barriers->children("BarrierData")) {
            if (spelling_of(barrier, "Type") != "FixingCap")
                continue;
            const auto levels = barrier_levels_of(barrier);
            if (levels.empty())
                continue;
            r.knock_out_level = levels.front();
            break;
        }
    }
    return r;
}

auto read_equity_cliquet_option(const record_reader& trade) -> equity_option_instrument {
    auto r = make_row<equity_option_instrument>("EquityCliquetOption");
    const auto data = trade.child("EquityCliquetOptionData");
    if (!data)
        return r;
    r.underlying_name = underlying_name_of(*data);
    r.currency = spelling_of(*data, "Currency");
    r.notional = decimal_of(*data, "Notional");
    r.option_type = spelling_of(*data, "OptionType");

    const auto schedule = data->child("ScheduleData");
    if (!schedule)
        return r;
    const auto rules = schedule->children("Rules");
    if (!rules.empty()) {
        r.cliquet_frequency = text_of(rules.front(), "Tenor");
        return r;
    }
    const auto dates = schedule->children("Dates");
    if (dates.empty())
        return r;
    const auto inner = dates.front().child("Dates");
    if (!inner)
        return r;
    const auto values = inner->values("Date");
    if (values.size() >= 2)
        r.expiry_date = to_domain_date(values.back());
    return r;
}

auto read_equity_worst_of_basket_swap(const record_reader& trade) -> equity_swap_instrument {
    auto r = make_row<equity_swap_instrument>("EquityWorstOfBasketSwap");
    const auto data = trade.child("EquityWorstOfBasketSwapData");
    if (!data)
        return r;
    r.currency = spelling_of(*data, "Currency");
    r.notional = decimal_of(*data, "Quantity");

    std::vector<std::string> names;
    if (const auto underlyings = data->child("Underlyings"))
        for (const auto& underlying : underlyings->children("Underlying"))
            names.push_back(text_of(underlying, "Name"));
    r.basket_json = names_json(names);
    if (!names.empty())
        r.underlying_name = names.front();

    const auto levels_json = [&data](std::string_view element, std::string_view leaf) {
        const auto rows = data->child(element);
        if (!rows)
            return std::string{};
        return numbers_json(rows->decimals(leaf));
    };
    r.initial_prices_json = levels_json("InitialPrices", "InitialPrice");
    r.knock_out_levels_json = levels_json("KnockOutLevels", "KnockOutLevel");
    r.fixed_trigger_levels_json = levels_json("FixedTriggerLevels", "FixedTriggerLevel");
    return r;
}

auto read_equity_position(const record_reader& trade) -> equity_instrument_data {
    auto inst = make_row<equity_position_instrument>("EquityPosition");
    if (const auto data = trade.child("EquityPositionData")) {
        inst.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
        const auto rows = data->children("Underlying");
        if (!rows.empty())
            inst.underlying_name = text_of(rows.front(), "Name");
    }
    return equity_instrument_data{std::move(inst), {}};
}

auto read_equity_option_position(const record_reader& trade) -> equity_instrument_data {
    auto inst = make_row<equity_position_instrument>("EquityOptionPosition");
    std::vector<equity_position_option_underlying> underlyings;
    if (const auto data = trade.child("EquityOptionPositionData")) {
        inst.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
        int sequence_number = 0;
        for (const auto& entry : data->children("Underlying")) {
            equity_position_option_underlying row;
            row.sequence_number = ++sequence_number;
            if (const auto underlying = entry.child("Underlying")) {
                row.underlying_name = text_of(*underlying, "Name");
                row.weight = decimal_as_double(*underlying, "Weight");
            }
            row.strike = decimal_of(entry, "Strike");
            if (const auto option = entry.child("OptionData")) {
                row.long_short = text_of(*option, "LongShort");
                row.option_type = option_type_of(option);
                row.exercise_type = exercise_style_of(option);
                row.settlement_type = spelling_of(*option, "Settlement");
            }
            stamp_columns(row);
            underlyings.push_back(std::move(row));
        }
    }
    if (!underlyings.empty())
        inst.underlying_name = underlyings.front().underlying_name;
    return equity_instrument_data{std::move(inst), std::move(underlyings)};
}

// ---------------------------------------------------------------------------
// Writing: the shared shapes
// ---------------------------------------------------------------------------

auto flag_text(bool value) -> std::string_view {
    return value ? "true" : "false";
}

// The mapper names the direction rather than the row's own, because the option
// row states it and the table's per-type rows keep no column for it.
template <typename Row>
void write_option_entry(record_writer option, const Row& v) {
    option.set_text("LongShort", "Long");
    if (!v.option_type.empty())
        option.set_text("OptionType", v.option_type);
    if constexpr (requires { v.exercise_type; }) {
        if (!v.exercise_type.empty())
            option.set_text("Style", v.exercise_type);
    }
    if (v.expiry_date.ok())
        option.child("ExerciseDates").append_value("ExerciseDate", to_ore_date(v.expiry_date));
}

/// The six styles the barrier rows round-trip, refused by name like the mapper.
auto barrier_style_of(const std::string& type) -> std::string {
    if (type == "UpAndOut" || type == "UpAndIn" || type == "DownAndOut" || type == "DownAndIn" ||
        type == "KnockOut" || type == "KnockIn")
        return type;
    throw std::runtime_error("an equity barrier option cannot state the barrier type '" + type +
                             "': the schema has no such style");
}

/// The touch row's four styles; any other value falls back to DownAndOut.
auto touch_style_of(const std::string& type) -> std::string {
    if (type == "UpAndIn" || type == "UpAndOut" || type == "DownAndIn")
        return type;
    return "DownAndOut";
}

/// The settlement row's two spellings; any other value falls back to Cash.
auto settlement_spelling_of(const std::string& type) -> std::string {
    return type == "Physical" ? "Physical" : "Cash";
}

void write_range_bounds(record_writer& data, std::string_view json) {
    const auto rows = parse_range_bounds(json);
    if (rows.empty())
        return;
    auto bounds = data.child("RangeBounds");
    for (const auto& row : rows) {
        auto writer = bounds.append("RangeBound");
        if (row.start_date)
            writer.set_text("startDate", *row.start_date);
        if (row.range_from)
            writer.set_decimal("RangeFrom", *row.range_from);
        if (row.range_to)
            writer.set_decimal("RangeTo", *row.range_to);
        if (row.leverage)
            writer.set_decimal("Leverage", *row.leverage);
        if (row.strike)
            writer.set_decimal("Strike", *row.strike);
        if (row.strike_adjustment)
            writer.set_decimal("StrikeAdjustment", *row.strike_adjustment);
    }
}

void write_levels(record_writer& data,
                  std::string_view element,
                  std::string_view leaf,
                  const std::vector<decimal>& values) {
    if (values.empty())
        return;
    auto list = data.child(element);
    for (const auto& value : values)
        list.append_decimal(leaf, value);
}

auto numbers_of(std::string_view json) -> std::vector<decimal> {
    std::vector<decimal> values;
    std::size_t i = 0;
    while (i < json.size()) {
        const auto start = json.find_first_of("-0123456789.", i);
        if (start == std::string_view::npos)
            break;
        auto end = start;
        while (end < json.size() &&
               std::string_view("0123456789.eE+-").find(json[end]) != std::string_view::npos)
            ++end;
        if (auto parsed = decimal::from_string(json.substr(start, end - start)))
            values.push_back(std::move(*parsed));
        i = end;
    }
    return values;
}

auto names_of(std::string_view json) -> std::vector<std::string> {
    std::vector<std::string> values;
    std::size_t i = 0;
    while (i < json.size()) {
        const auto open = json.find('"', i);
        if (open == std::string_view::npos)
            break;
        std::string value;
        auto p = open + 1;
        while (p < json.size() && json[p] != '"') {
            if (json[p] == '\\' && p + 1 < json.size())
                ++p;
            value += json[p++];
        }
        values.push_back(std::move(value));
        i = p + 1;
    }
    return values;
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

void write_equity_option(record_writer& trade, const equity_option_instrument& v) {
    auto data = trade.child("EquityOptionData");
    write_option_entry(data.child("OptionData"), v);
    data.set_text("Name", v.underlying_name);
    data.set_text("Currency", v.currency);
    data.set_decimal("Quantity", v.notional);
    if (!v.strike.is_zero())
        data.set_text("Strike", v.strike.to_string());
}

void write_equity_forward(record_writer& trade, const equity_forward_instrument& v) {
    auto data = trade.child("EquityForwardData");
    data.set_spelling("LongShort", "Long", "long short");
    data.set_text("Maturity", to_ore_date(v.expiry_date));
    data.set_text("Name", v.underlying_name);
    data.set_text("Currency", v.currency);
    data.set_decimal("Strike", v.forward_price.value_or(decimal{}));
    set_decimal_from_double(data, "Quantity", v.quantity);
}

// The per-type row states one notional and the document states it on two legs,
// so the same amount is written to both.
void write_equity_swap(record_writer& trade, const equity_swap_instrument& v) {
    auto data = trade.child("EquitySwapData");

    auto equity_leg = data.append("LegData");
    equity_leg.set_spelling("LegType", "Equity", "leg type");
    equity_leg.set_text("Payer", flag_text(true));
    auto equity = equity_leg.child("EquityLegData");
    equity.set_text("Name", v.underlying_name);
    equity.set_text("ReturnType", v.return_type);
    if (!v.notional.is_zero())
        equity.set_decimal("Quantity", v.notional);

    auto funding_leg = data.append("LegData");
    funding_leg.set_spelling("LegType", "Floating", "leg type");
    funding_leg.set_text("Payer", flag_text(false));
    if (!v.currency.empty())
        funding_leg.set_text("Currency", v.currency);
    if (!v.notional.is_zero())
        funding_leg.child("Notionals").append("Notional").set_own_text(v.notional.to_string());
}

void write_equity_variance_swap(record_writer& trade, const equity_variance_swap_instrument& v) {
    auto data = trade.child("EquityVarianceSwapData");
    data.set_text("StartDate", to_ore_date(v.start_date));
    data.set_text("EndDate", to_ore_date(v.maturity_date));
    data.set_spelling("Currency", v.currency, "currency");
    data.set_text("Name", v.underlying_name);
    data.set_text("LongShort", "Long");
    set_decimal_from_double(data, "Strike", v.variance_strike);
    data.set_decimal("Notional", v.notional);
    // The document states the calendar it accrues under and the row does not,
    // so the mapper's own default is what the export writes.
    data.set_text("Calendar", "USD");
}

// A single barrier writes its first level and leaves the second column to the
// double and European variants, which are the two the mapper restored it for.
void write_equity_barrier_option(record_writer& trade,
                                 std::string_view element,
                                 const equity_barrier_option_instrument& v,
                                 bool writes_upper) {
    auto data = trade.child(element);
    write_option_entry(data.child("OptionData"), v);
    data.set_text("Name", v.underlying_name);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Quantity", v.notional);
    if (!v.strike.is_zero())
        data.set_text("Strike", v.strike.to_string());
    if (v.lower_barrier_type.empty())
        return;
    auto barrier = data.child("BarrierData");
    barrier.set_spelling("Type", barrier_style_of(v.lower_barrier_type), "barrier type");
    auto levels = barrier.child("Levels");
    if (!v.lower_barrier.is_zero())
        levels.append_decimal("Level", v.lower_barrier);
    if (writes_upper && v.upper_barrier)
        levels.append_decimal("Level", *v.upper_barrier);
}

void write_equity_asian_option(record_writer& trade, const equity_asian_option_instrument& v) {
    auto data = trade.child("EquityAsianOptionData");
    write_option_entry(data.child("OptionData"), v);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Quantity", v.notional);
    if (!v.strike.is_zero())
        data.child("StrikeData").set_decimal("Value", v.strike);
    if (!v.underlying_name.empty()) {
        auto underlying = data.child("Underlying");
        underlying.set_text("Name", v.underlying_name);
        underlying.set_text("Type", "Equity");
    }
}

void write_equity_digital_option(record_writer& trade, const equity_digital_option_instrument& v) {
    auto data = trade.child("EquityDigitalOptionData");
    write_option_entry(data.child("OptionData"), v);
    data.set_text("Name", v.underlying_name);
    data.set_decimal("Strike", v.strike.value_or(decimal{}));
    data.set_decimal("PayoffAmount", v.notional);
    // The schema requires a quantity and the row keeps the payoff amount alone,
    // which the export states as the payout of one unit.
    data.set_decimal("Quantity", decimal_from_double(1.0));
}

void write_equity_touch_option(record_writer& trade, const equity_digital_option_instrument& v) {
    auto data = trade.child("EquityTouchOptionData");
    write_option_entry(data.child("OptionData"), v);
    data.set_text("Name", v.underlying_name);
    data.set_spelling("PayoffCurrency", v.currency, "currency");
    data.set_decimal("PayoffAmount", v.notional);
    if (v.barrier_type.empty())
        return;
    auto barrier = data.child("BarrierData");
    barrier.set_spelling("Type", touch_style_of(v.barrier_type), "barrier type");
    if (v.barrier_level)
        barrier.child("Levels").append_decimal("Level", *v.barrier_level);
}

void write_equity_outperformance_option(record_writer& trade, const equity_option_instrument& v) {
    auto data = trade.child("EquityOutperformanceOptionData");
    write_option_entry(data.child("OptionData"), v);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Notional", v.notional);
    data.set_decimal("StrikeReturn", v.strike);

    // The option row carries the pair as one name, so the split is its reverse.
    const auto separator = v.underlying_name.find('/');
    const auto first =
        separator == std::string::npos ? v.underlying_name : v.underlying_name.substr(0, separator);
    const auto second =
        separator == std::string::npos ? first : v.underlying_name.substr(separator + 1);
    auto underlying1 = data.child("Underlying1");
    underlying1.set_text("Name", first);
    underlying1.set_text("Type", "Equity");
    auto underlying2 = data.child("Underlying2");
    underlying2.set_text("Name", second);
    underlying2.set_text("Type", "Equity");

    // The schema requires both initial prices and the row keeps neither, so the
    // export states the zero the rest of the writing states for a term the row
    // holds no column for.
    data.set_decimal("InitialPrice1", decimal{});
    data.set_decimal("InitialPrice2", decimal{});
}

void write_equity_accumulator(record_writer& trade, const equity_accumulator_instrument& v) {
    auto data = trade.child("EquityAccumulatorData");
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("FixingAmount", v.fixing_amount);
    if (!v.strike.is_zero())
        data.set_decimal("Strike", v.strike);
    auto underlying = data.child("Underlying");
    underlying.set_text("Name", v.underlying_name);
    underlying.set_text("Type", "Equity");
    data.child("OptionData").set_text("LongShort", "Long");
    if (v.start_date.ok())
        data.set_text("StartDate", to_ore_date(v.start_date));

    write_schedule_rule(data.child("ObservationDates"), to_ore_date(v.start_date), "1D");

    // The mapper restores no barrier for an accumulator, so the knock-out level
    // the row keeps has no element to go back into. Barriers is optional, so
    // the document stays readable.
    write_range_bounds(data, v.range_bounds_json);
}

void write_equity_tarf(record_writer& trade, const equity_accumulator_instrument& v) {
    auto data = trade.child("EquityTaRFData");
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("FixingAmount", v.fixing_amount);
    if (!v.strike.is_zero())
        data.set_decimal("Strike", v.strike);
    auto underlying = data.child("Underlying");
    underlying.set_text("Name", v.underlying_name);
    underlying.set_text("Type", "Equity");
    data.child("OptionData").set_text("LongShort", "Long");
    write_schedule_rule(data.child("ScheduleData"), "", "1Y");

    // The schema requires a barrier, and the cap the row keeps is the one the
    // document states.
    auto barrier = data.child("Barriers").append("BarrierData");
    barrier.set_spelling("Type", "FixingCap", "barrier type");
    barrier.child("Levels").append_decimal("Level", v.knock_out_level.value_or(decimal{}));
}

void write_equity_cliquet_option(record_writer& trade, const equity_option_instrument& v) {
    auto data = trade.child("EquityCliquetOptionData");
    data.set_text("Name", v.underlying_name);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Notional", v.notional);
    data.set_spelling("LongShort", "Long", "long short");
    if (v.option_type == "Put" || v.option_type == "Call")
        data.set_spelling("OptionType", v.option_type, "option type");
    else
        throw std::runtime_error("an EquityCliquetOption cannot state the option type '" +
                                 v.option_type + "': the schema has no such type");
    // The row holds no moneyness, and the schema requires one. One is the
    // value that leaves the payoff unchanged; it is a structural default, not
    // a quote from the document.
    set_decimal_from_double(data, "Moneyness", 1.0);
    // The schema requires a schedule, and a row that keeps no frequency states
    // the element without a rule rather than dropping it.
    auto schedule = data.child("ScheduleData");
    if (!v.cliquet_frequency.empty())
        write_schedule_rule(schedule, "", v.cliquet_frequency);
}

void write_equity_worst_of_basket_swap(record_writer& trade, const equity_swap_instrument& v) {
    auto data = trade.child("EquityWorstOfBasketSwapData");
    data.set_spelling("LongShort", "Long", "long short");
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Quantity", v.notional);
    // The schema requires the funding leg's three terms and the row holds
    // none of them. The index is the marker a reader resolves against the
    // trade, and the rate is the structural zero; neither is a quote.
    set_decimal_from_double(data, "FixedRate", 0.0);
    data.set_text("FloatingIndex", "PLACEHOLDER");
    data.set_spelling("FloatingDayCountFraction", "Actual/360", "day counter");

    auto names = names_of(v.basket_json);
    if (names.empty() && !v.underlying_name.empty())
        names.push_back(v.underlying_name);
    if (!names.empty()) {
        auto underlyings = data.child("Underlyings");
        for (const auto& name : names) {
            auto underlying = underlyings.append("Underlying");
            underlying.set_text("Name", name);
            underlying.set_text("Type", "Equity");
        }
    }

    // The schema requires a rule on each schedule and the row states none.
    write_schedule_rule(data.child("FloatingPeriodSchedule"), "", "6M");
    write_schedule_rule(data.child("FloatingPayDates"), "", "6M");

    // Each list spells its leaf its own way, and the schema names none of them
    // Level.
    write_levels(data, "InitialPrices", "InitialPrice", numbers_of(v.initial_prices_json));
    write_levels(data, "KnockOutLevels", "KnockOutLevel", numbers_of(v.knock_out_levels_json));
    write_levels(
        data, "FixedTriggerLevels", "FixedTriggerLevel", numbers_of(v.fixed_trigger_levels_json));
}

void write_equity_position(record_writer& trade, const equity_position_instrument& v) {
    auto data = trade.child("EquityPositionData");
    set_decimal_from_double(data, "Quantity", v.quantity);
    if (!v.underlying_name.empty())
        data.append("Underlying").set_text("Name", v.underlying_name);
}

void write_equity_option_position(record_writer& trade,
                                  const equity_position_instrument& v,
                                  const std::vector<equity_position_option_underlying>& rows) {
    auto data = trade.child("EquityOptionPositionData");
    set_decimal_from_double(data, "Quantity", v.quantity);

    std::vector<const equity_position_option_underlying*> ordered;
    ordered.reserve(rows.size());
    for (const auto& row : rows)
        ordered.push_back(&row);
    std::sort(ordered.begin(), ordered.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    for (const auto* row : ordered) {
        auto entry = data.append("Underlying");
        auto underlying = entry.child("Underlying");
        underlying.set_text("Name", row->underlying_name);
        if (row->weight)
            set_decimal_from_double(underlying, "Weight", *row->weight);
        auto option = entry.child("OptionData");
        option.set_text("LongShort", row->long_short);
        if (!row->option_type.empty())
            option.set_text("OptionType", row->option_type);
        if (!row->exercise_type.empty())
            option.set_text("Style", row->exercise_type);
        if (!row->settlement_type.empty())
            option.set_spelling(
                "Settlement", settlement_spelling_of(row->settlement_type), "settlement type");
        entry.set_decimal("Strike", row->strike);
    }
}

}

auto read_equity_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_equity_trade_type(trade_type))
        return std::nullopt;

    if (trade_type == "EquityPosition")
        return trading::domain::trade_instrument{read_equity_position(trade)};
    if (trade_type == "EquityOptionPosition")
        return trading::domain::trade_instrument{read_equity_option_position(trade)};

    const auto wrap = [](auto row) {
        return trading::domain::trade_instrument{equity_instrument_data{std::move(row), {}}};
    };

    if (trade_type == "EquityOption")
        return wrap(read_equity_option(trade, trade_type));
    if (trade_type == "EquityForward")
        return wrap(read_equity_forward(trade));
    if (trade_type == "EquitySwap")
        return wrap(read_equity_swap(trade));
    if (trade_type == "EquityVarianceSwap")
        return wrap(read_equity_variance_swap(trade));
    if (trade_type == "EquityBarrierOption")
        return wrap(read_equity_barrier_option(trade, "EquityBarrierOptionData", trade_type));
    if (trade_type == "EquityDoubleBarrierOption")
        return wrap(read_equity_barrier_option(trade, "EquityDoubleBarrierOptionData", trade_type));
    if (trade_type == "EquityEuropeanBarrierOption")
        return wrap(
            read_equity_barrier_option(trade, "EquityEuropeanBarrierOptionData", trade_type));
    if (trade_type == "EquityAsianOption")
        return wrap(read_equity_asian_option(trade));
    if (trade_type == "EquityDigitalOption")
        return wrap(read_equity_digital_option(trade));
    if (trade_type == "EquityTouchOption")
        return wrap(read_equity_touch_option(trade));
    if (trade_type == "EquityOutperformanceOption")
        return wrap(read_equity_outperformance_option(trade));
    if (trade_type == "EquityAccumulator")
        return wrap(read_equity_accumulator(trade));
    if (trade_type == "EquityTaRF")
        return wrap(read_equity_tarf(trade));
    if (trade_type == "EquityCliquetOption")
        return wrap(read_equity_cliquet_option(trade));
    return wrap(read_equity_worst_of_basket_swap(trade));
}

auto write_equity_instrument(xml::record_writer& trade,
                             std::string_view trade_type,
                             const trading::domain::trade_instrument& instrument,
                             const portfolio_reference_curves&) -> bool {
    const auto* v = std::get_if<equity_instrument_data>(&instrument);
    if (v == nullptr || !is_equity_trade_type(trade_type))
        return false;

    bool matched = false;
    std::visit(
        [&](const auto& row) {
            using Row = std::decay_t<decltype(row)>;
            if constexpr (std::is_same_v<Row, equity_position_instrument>) {
                // A position that holds option entries is the option position,
                // whatever the anchor says, which is the choice the exporter made.
                if (!v->underlyings.empty() ||
                    row.identity.trade_type_code == "EquityOptionPosition") {
                    write_equity_option_position(trade, row, v->underlyings);
                    matched = true;
                } else {
                    write_equity_position(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_option_instrument>) {
                if (trade_type == "EquityOption") {
                    write_equity_option(trade, row);
                    matched = true;
                } else if (trade_type == "EquityCliquetOption") {
                    write_equity_cliquet_option(trade, row);
                    matched = true;
                } else if (trade_type == "EquityOutperformanceOption") {
                    write_equity_outperformance_option(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_forward_instrument>) {
                write_equity_forward(trade, row);
                matched = true;
            } else if constexpr (std::is_same_v<Row, equity_swap_instrument>) {
                if (trade_type == "EquitySwap") {
                    write_equity_swap(trade, row);
                    matched = true;
                } else if (trade_type == "EquityWorstOfBasketSwap") {
                    write_equity_worst_of_basket_swap(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_variance_swap_instrument>) {
                write_equity_variance_swap(trade, row);
                matched = true;
            } else if constexpr (std::is_same_v<Row, equity_barrier_option_instrument>) {
                if (trade_type == "EquityBarrierOption") {
                    write_equity_barrier_option(trade, "EquityBarrierOptionData", row, false);
                    matched = true;
                } else if (trade_type == "EquityDoubleBarrierOption") {
                    write_equity_barrier_option(trade, "EquityDoubleBarrierOptionData", row, true);
                    matched = true;
                } else if (trade_type == "EquityEuropeanBarrierOption") {
                    write_equity_barrier_option(
                        trade, "EquityEuropeanBarrierOptionData", row, true);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_asian_option_instrument>) {
                write_equity_asian_option(trade, row);
                matched = true;
            } else if constexpr (std::is_same_v<Row, equity_digital_option_instrument>) {
                if (trade_type == "EquityDigitalOption") {
                    write_equity_digital_option(trade, row);
                    matched = true;
                } else if (trade_type == "EquityTouchOption") {
                    write_equity_touch_option(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_accumulator_instrument>) {
                if (trade_type == "EquityAccumulator") {
                    write_equity_accumulator(trade, row);
                    matched = true;
                } else if (trade_type == "EquityTaRF") {
                    write_equity_tarf(trade, row);
                    matched = true;
                }
            }
        },
        v->instrument);
    return matched;
}

}
