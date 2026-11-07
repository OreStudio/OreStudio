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
#include "ores.ore.core/domain/barrier_block_projection.hpp"
#include "ores.ore.core/domain/leg_block_projection.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/domain/underlying_block_projection.hpp"
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
using trading::domain::equity_basket_constituent;
using trading::domain::equity_asian_option_instrument;
using trading::domain::equity_barrier_option_instrument;
using trading::domain::equity_digital_option_instrument;
using trading::domain::equity_forward_instrument;
using trading::domain::equity_instrument_data;
using trading::domain::equity_instrument_variant;
using trading::domain::equity_option_instrument;
using trading::domain::equity_position_instrument;
using trading::domain::equity_position_option_underlying;
using trading::domain::equity_swap_instrument;
using trading::domain::equity_swap_period;
using trading::domain::equity_variance_swap_instrument;
using trading::domain::range_bound;

using xml::record_reader;
using xml::record_writer;

// ---------------------------------------------------------------------------
// The vocabulary both directions share
// ---------------------------------------------------------------------------

/// The actor and reason every imported row states.
constexpr std::string_view equity_instrument_projection_actor = "ores";
constexpr std::string_view equity_instrument_projection_change_reason =
    "system.external_data_import";
constexpr std::string_view equity_instrument_projection_change_commentary = "Imported from ORE XML";

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
    row.audit.modified_by = std::string(equity_instrument_projection_actor);
    row.audit.performed_by = std::string(equity_instrument_projection_actor);
    row.audit.change_reason_code = std::string(equity_instrument_projection_change_reason);
    row.audit.change_commentary = std::string(equity_instrument_projection_change_commentary);
}

void equity_instrument_projection_stamp_columns(equity_position_option_underlying& row) {
    row.modified_by = std::string(equity_instrument_projection_actor);
    row.performed_by = std::string(equity_instrument_projection_actor);
    row.change_reason_code = std::string(equity_instrument_projection_change_reason);
    row.change_commentary = std::string(equity_instrument_projection_change_commentary);
}

template <typename Row>
auto equity_instrument_projection_make_row(std::string_view trade_type_code) -> Row {
    Row r;
    r.identity.trade_type_code = std::string(trade_type_code);
    stamp_audit(r);
    return r;
}

// ---------------------------------------------------------------------------
// Reading: the values, dates and spellings a row states
// ---------------------------------------------------------------------------

auto equity_instrument_projection_to_domain_date(const std::string& text)
    -> std::chrono::year_month_day {
    if (text.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto equity_instrument_projection_to_ore_date(const std::chrono::year_month_day& d) -> std::string {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

auto equity_instrument_projection_text_of(const record_reader& r,
                                          std::string_view name) -> std::string {
    if (const auto value = r.text(name))
        return *value;
    return {};
}

auto equity_instrument_projection_spelling_of(const record_reader& r,
                                              std::string_view name) -> std::string {
    if (const auto value = r.spelling(name))
        return *value;
    return {};
}

/// A schema decimal element as the domain's exact decimal, zero when absent.
auto equity_instrument_projection_decimal_of(const record_reader& r,
                                             std::string_view name) -> decimal {
    if (const auto value = r.decimal(name))
        return *value;
    return decimal{};
}

auto equity_instrument_projection_optional_decimal_of(const record_reader& r, std::string_view name)
    -> std::optional<decimal> {
    return r.decimal(name);
}

/// The exact decimal an element of a xs:simpleContent decimal type carries.
auto equity_instrument_projection_own_decimal_of(const record_reader& r) -> decimal {
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
    return equity_instrument_projection_to_domain_date(values.front());
}

auto equity_instrument_projection_option_type_of(const std::optional<record_reader>& option)
    -> std::string {
    if (!option)
        return {};
    return equity_instrument_projection_text_of(*option, "OptionType");
}

auto equity_instrument_projection_exercise_style_of(const std::optional<record_reader>& option)
    -> std::string {
    if (!option)
        return {};
    return equity_instrument_projection_text_of(*option, "Style");
}

/**
 * @brief The underlying a product names.
 *
 * ORE states an underlying through a substitution group, so the document names
 * either the underlying itself or a basket; only the two forms that carry one
 * name are read, which is the pair the old mapper read.
 */
auto equity_instrument_projection_underlying_name_of(const record_reader& data) -> std::string {
    if (const auto name = data.text("Name"))
        return *name;
    if (const auto underlying = data.child("Underlying"))
        return equity_instrument_projection_text_of(*underlying, "Name");
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
        return equity_instrument_projection_decimal_of(*strike_data, "Value");
    return decimal{};
}

auto barrier_levels_of(const record_reader& barrier) -> std::vector<decimal> {
    const auto levels = barrier.child("Levels");
    if (!levels)
        return {};
    return levels->decimals("Level");
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


// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

auto read_equity_option(const record_reader& trade,
                        std::string_view trade_type) -> equity_option_instrument {
    auto r = equity_instrument_projection_make_row<equity_option_instrument>(trade_type);
    const auto data = trade.child("EquityOptionData");
    if (!data)
        return r;
    r.underlying_name = equity_instrument_projection_underlying_name_of(*data);
    r.currency = equity_instrument_projection_text_of(*data, "Currency");
    r.notional = equity_instrument_projection_decimal_of(*data, "Quantity");
    const auto option = data->child("OptionData");
    r.option_type = equity_instrument_projection_option_type_of(option);
    r.exercise_type = equity_instrument_projection_exercise_style_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = strike_of(*data);
    return r;
}

auto read_equity_forward(const record_reader& trade) -> equity_forward_instrument {
    auto r = equity_instrument_projection_make_row<equity_forward_instrument>("EquityForward");
    const auto data = trade.child("EquityForwardData");
    if (!data)
        return r;
    r.underlying_name = equity_instrument_projection_underlying_name_of(*data);
    r.currency = equity_instrument_projection_text_of(*data, "Currency");
    r.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
    r.forward_price = equity_instrument_projection_optional_decimal_of(*data, "Strike");
    r.expiry_date = equity_instrument_projection_to_domain_date(
        equity_instrument_projection_text_of(*data, "Maturity"));
    return r;
}

// The equity leg states the underlying and the return, the funding leg the
// currency, the notional and the schedule; a leg that states both overwrites
// the other's values in the order the document wrote them.
// The row keeps the swap's summary; the legs themselves land in the shared
// leg rows, as a rates swap's do.
auto read_equity_swap(const record_reader& trade) -> equity_instrument_data {
    equity_instrument_data result{
        equity_instrument_projection_make_row<equity_swap_instrument>("EquitySwap"), {}};
    auto& r = std::get<equity_swap_instrument>(result.instrument);
    const auto data = trade.child("EquitySwapData");
    if (!data)
        return result;
    leg_block::append_legs(result, *data);
    for (const auto& leg : data->children("LegData")) {
        if (const auto equity = leg.child("EquityLegData")) {
            r.underlying_name = equity_instrument_projection_underlying_name_of(*equity);
            r.return_type = equity_instrument_projection_text_of(*equity, "ReturnType");
            if (const auto quantity =
                    equity_instrument_projection_optional_decimal_of(*equity, "Quantity"))
                r.notional = *quantity;
        } else {
            if (const auto currency = leg.text("Currency"))
                r.currency = *currency;
            if (const auto notionals = leg.child("Notionals")) {
                const auto rows = notionals->children("Notional");
                if (!rows.empty())
                    r.notional = equity_instrument_projection_own_decimal_of(rows.front());
            }
            if (const auto schedule = leg.child("ScheduleData")) {
                const auto rules = schedule->children("Rules");
                if (!rules.empty()) {
                    r.start_date = equity_instrument_projection_to_domain_date(
                        equity_instrument_projection_text_of(rules.front(), "StartDate"));
                    if (const auto end = rules.front().text("EndDate"))
                        r.maturity_date = equity_instrument_projection_to_domain_date(*end);
                    r.payment_frequency_code = tenor_to_payment_frequency(
                        equity_instrument_projection_text_of(rules.front(), "Tenor"));
                }
            }
        }
    }
    return result;
}

auto read_equity_variance_swap(const record_reader& trade) -> equity_variance_swap_instrument {
    auto r = equity_instrument_projection_make_row<equity_variance_swap_instrument>(
        "EquityVarianceSwap");
    const auto data = trade.child("EquityVarianceSwapData");
    if (!data)
        return r;
    r.underlying_name = equity_instrument_projection_underlying_name_of(*data);
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.notional = equity_instrument_projection_decimal_of(*data, "Notional");
    r.variance_strike = decimal_as_double(*data, "Strike").value_or(0.0);
    r.start_date = equity_instrument_projection_to_domain_date(
        equity_instrument_projection_text_of(*data, "StartDate"));
    r.maturity_date = equity_instrument_projection_to_domain_date(
        equity_instrument_projection_text_of(*data, "EndDate"));
    return r;
}

// The three barrier products state one barrier term each under an element of
// their own, so the trade type states which element carries it.
auto read_equity_barrier_option(const record_reader& trade,
                                std::string_view element,
                                std::string_view trade_type) -> equity_barrier_option_instrument {
    auto r = equity_instrument_projection_make_row<equity_barrier_option_instrument>(trade_type);
    const auto data = trade.child(element);
    if (!data)
        return r;
    r.underlying_name = equity_instrument_projection_underlying_name_of(*data);
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.notional = equity_instrument_projection_decimal_of(*data, "Quantity");
    const auto option = data->child("OptionData");
    r.option_type = equity_instrument_projection_option_type_of(option);
    r.exercise_type = equity_instrument_projection_exercise_style_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = strike_of(*data);
    if (const auto barrier = data->child("BarrierData")) {
        r.lower_barrier_type = equity_instrument_projection_spelling_of(*barrier, "Type");
        const auto levels = barrier_levels_of(*barrier);
        if (!levels.empty())
            r.lower_barrier = levels.front();
        if (levels.size() > 1 && levels[1] > decimal{})
            r.upper_barrier = levels[1];
    }
    return r;
}

auto read_equity_asian_option(const record_reader& trade) -> equity_asian_option_instrument {
    auto r =
        equity_instrument_projection_make_row<equity_asian_option_instrument>("EquityAsianOption");
    const auto data = trade.child("EquityAsianOptionData");
    if (!data)
        return r;
    if (const auto underlying = data->child("Underlying"))
        r.underlying_name = equity_instrument_projection_text_of(*underlying, "Name");
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.notional = equity_instrument_projection_decimal_of(*data, "Quantity");
    const auto option = data->child("OptionData");
    r.option_type = equity_instrument_projection_option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = strike_of(*data);
    if (const auto dates = data->child("ObservationDates")) {
        const auto rules = dates->children("Rules");
        if (!rules.empty())
            r.averaging_start_date = equity_instrument_projection_to_domain_date(
                equity_instrument_projection_text_of(rules.front(), "StartDate"));
    }
    return r;
}

auto read_equity_digital_option(const record_reader& trade) -> equity_digital_option_instrument {
    auto r = equity_instrument_projection_make_row<equity_digital_option_instrument>(
        "EquityDigitalOption");
    const auto data = trade.child("EquityDigitalOptionData");
    if (!data)
        return r;
    r.underlying_name = equity_instrument_projection_underlying_name_of(*data);
    const auto option = data->child("OptionData");
    r.option_type = equity_instrument_projection_option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = equity_instrument_projection_optional_decimal_of(*data, "Strike");
    r.notional = equity_instrument_projection_decimal_of(*data, "PayoffAmount");
    r.currency = equity_instrument_projection_spelling_of(*data, "PayoffCurrency");
    return r;
}

// A touch option is the digital row with a barrier instead of a strike, which
// is the shape the old mapper gave it.
auto read_equity_touch_option(const record_reader& trade) -> equity_digital_option_instrument {
    auto r = equity_instrument_projection_make_row<equity_digital_option_instrument>(
        "EquityTouchOption");
    const auto data = trade.child("EquityTouchOptionData");
    if (!data)
        return r;
    r.underlying_name = equity_instrument_projection_underlying_name_of(*data);
    r.currency = equity_instrument_projection_spelling_of(*data, "PayoffCurrency");
    r.notional = equity_instrument_projection_decimal_of(*data, "PayoffAmount");
    const auto option = data->child("OptionData");
    r.option_type = equity_instrument_projection_option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    if (const auto barrier = data->child("BarrierData")) {
        r.barrier_type = equity_instrument_projection_spelling_of(*barrier, "Type");
        const auto levels = barrier_levels_of(*barrier);
        if (!levels.empty() && levels.front() > decimal{})
            r.barrier_level = levels.front();
    }
    return r;
}

auto read_equity_outperformance_option(const record_reader& trade) -> equity_option_instrument {
    auto r = equity_instrument_projection_make_row<equity_option_instrument>(
        "EquityOutperformanceOption");
    const auto data = trade.child("EquityOutperformanceOptionData");
    if (!data)
        return r;
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.notional = equity_instrument_projection_decimal_of(*data, "Notional");
    const auto option = data->child("OptionData");
    r.option_type = equity_instrument_projection_option_type_of(option);
    r.expiry_date = expiry_date_of(option);
    r.strike = equity_instrument_projection_decimal_of(*data, "StrikeReturn");

    // The option row has no basket column, so the pair travels as one name.
    const auto first = data->child("Underlying1");
    const auto second = data->child("Underlying2");
    r.underlying_name =
        (first ? equity_instrument_projection_text_of(*first, "Name") : std::string{}) + "/" +
        (second ? equity_instrument_projection_text_of(*second, "Name") : std::string{});
    return r;
}

auto read_equity_accumulator(const record_reader& trade) -> equity_instrument_data {
    auto r =
        equity_instrument_projection_make_row<equity_accumulator_instrument>("EquityAccumulator");
    const auto data = trade.child("EquityAccumulatorData");
    if (!data)
        return equity_instrument_data{equity_instrument_variant{r}, {}};
    if (const auto underlying = data->child("Underlying"))
        r.underlying_name = equity_instrument_projection_text_of(*underlying, "Name");
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.fixing_amount = equity_instrument_projection_decimal_of(*data, "FixingAmount");
    r.strike = equity_instrument_projection_decimal_of(*data, "Strike");
    if (const auto option = data->child("OptionData"))
        r.payoff_type = equity_instrument_projection_text_of(*option, "PayoffType");
    if (const auto start = data->text("StartDate"))
        r.start_date = equity_instrument_projection_to_domain_date(*start);
    if (const auto barriers = data->child("Barriers")) {
        for (const auto& barrier : barriers->children("BarrierData")) {
            const auto type = equity_instrument_projection_spelling_of(barrier, "Type");
            if (type != "DownAndOut" && type != "UpAndOut")
                continue;
            const auto levels = barrier_levels_of(barrier);
            if (levels.empty())
                continue;
            r.knock_out_level = levels.front();
            break;
        }
    }
    return equity_instrument_data{equity_instrument_variant{r}, {}};
}

auto read_equity_tarf(const record_reader& trade) -> equity_accumulator_instrument {
    auto r = equity_instrument_projection_make_row<equity_accumulator_instrument>("EquityTaRF");
    const auto data = trade.child("EquityTaRFData");
    if (!data)
        return r;
    if (const auto underlying = data->child("Underlying"))
        r.underlying_name = equity_instrument_projection_text_of(*underlying, "Name");
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.fixing_amount = equity_instrument_projection_decimal_of(*data, "FixingAmount");
    r.strike = equity_instrument_projection_decimal_of(*data, "Strike");
    if (const auto option = data->child("OptionData"))
        r.payoff_type = equity_instrument_projection_text_of(*option, "PayoffType");
    r.target_amount = data->decimal("TargetAmount");
    if (const auto barriers = data->child("Barriers")) {
        for (const auto& barrier : barriers->children("BarrierData")) {
            if (equity_instrument_projection_spelling_of(barrier, "Type") != "FixingCap")
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
    auto r = equity_instrument_projection_make_row<equity_option_instrument>("EquityCliquetOption");
    const auto data = trade.child("EquityCliquetOptionData");
    if (!data)
        return r;
    r.underlying_name = equity_instrument_projection_underlying_name_of(*data);
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.notional = equity_instrument_projection_decimal_of(*data, "Notional");
    r.option_type = equity_instrument_projection_spelling_of(*data, "OptionType");
    r.long_short = equity_instrument_projection_spelling_of(*data, "LongShort");

    const auto schedule = data->child("ScheduleData");
    if (!schedule)
        return r;
    const auto rules = schedule->children("Rules");
    if (!rules.empty()) {
        r.cliquet_frequency = equity_instrument_projection_text_of(rules.front(), "Tenor");
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
        r.expiry_date = equity_instrument_projection_to_domain_date(values.back());
    return r;
}

auto read_equity_worst_of_basket_swap(const record_reader& trade) -> equity_instrument_data {
    auto r =
        equity_instrument_projection_make_row<equity_swap_instrument>("EquityWorstOfBasketSwap");
    const auto data = trade.child("EquityWorstOfBasketSwapData");
    if (!data)
        return equity_instrument_data{equity_instrument_variant{r}, {}};
    r.currency = equity_instrument_projection_spelling_of(*data, "Currency");
    r.notional = equity_instrument_projection_decimal_of(*data, "Quantity");

    /*
     * The basket is a list, so it becomes rows: one constituent per Underlying,
     * carrying the ordinal the document stated it in.
     */
    std::vector<equity_basket_constituent> constituents;
    if (const auto underlyings = data->child("Underlyings")) {
        std::uint32_t ordinal = 1;
        for (const auto& underlying : underlyings->children("Underlying")) {
            equity_basket_constituent row;
            row.sequence_number = static_cast<int>(ordinal++);
            row.underlying_name = equity_instrument_projection_text_of(underlying, "Name");
            row.underlying_kind = equity_instrument_projection_text_of(underlying, "Type");
            if (underlying.has("IdentifierType"))
                row.identifier_type = underlying.text("IdentifierType");
            constituents.push_back(std::move(row));
        }
    }
    if (!constituents.empty())
        r.underlying_name = constituents.front().underlying_name;

    /*
     * The three lists run beside the basket and the schedule: InitialPrices is
     * one per underlying, and the two level lists are one per determination
     * date, so a basket of two names over four dates states two prices and
     * four levels.
     */
    const auto levels = [&data](std::string_view element, std::string_view leaf) {
        if (const auto rows = data->child(element))
            return rows->decimals(leaf);
        return std::vector<decimal>{};
    };

    const auto prices = levels("InitialPrices", "InitialPrice");
    for (std::size_t i = 0; i < prices.size() && i < constituents.size(); ++i)
        constituents[i].initial_price = prices[i];

    const auto knock_outs = levels("KnockOutLevels", "KnockOutLevel");
    const auto triggers = levels("FixedTriggerLevels", "FixedTriggerLevel");
    std::vector<equity_swap_period> periods;
    const auto period_count = std::max(knock_outs.size(), triggers.size());
    for (std::size_t i = 0; i < period_count; ++i) {
        equity_swap_period row;
        row.sequence_number = static_cast<int>(i + 1);
        if (i < knock_outs.size())
            row.knock_out_level = knock_outs[i].to_double();
        if (i < triggers.size())
            row.fixed_trigger_level = triggers[i].to_double();
        periods.push_back(std::move(row));
    }

    return equity_instrument_data{
        equity_instrument_variant{r}, {}, std::move(constituents), std::move(periods)};
}

auto read_equity_position(const record_reader& trade) -> equity_instrument_data {
    auto inst = equity_instrument_projection_make_row<equity_position_instrument>("EquityPosition");
    if (const auto data = trade.child("EquityPositionData")) {
        inst.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
        const auto rows = data->children("Underlying");
        if (!rows.empty())
            inst.underlying_name = equity_instrument_projection_text_of(rows.front(), "Name");
    }
    return equity_instrument_data{std::move(inst), {}};
}

auto read_equity_option_position(const record_reader& trade) -> equity_instrument_data {
    auto inst =
        equity_instrument_projection_make_row<equity_position_instrument>("EquityOptionPosition");
    std::vector<equity_position_option_underlying> underlyings;
    if (const auto data = trade.child("EquityOptionPositionData")) {
        inst.quantity = decimal_as_double(*data, "Quantity").value_or(0.0);
        int sequence_number = 0;
        for (const auto& entry : data->children("Underlying")) {
            equity_position_option_underlying row;
            row.sequence_number = ++sequence_number;
            if (const auto underlying = entry.child("Underlying")) {
                row.underlying_name = equity_instrument_projection_text_of(*underlying, "Name");
                row.weight = decimal_as_double(*underlying, "Weight");
                if (underlying->has("IdentifierType"))
                    row.identifier_type = underlying->text("IdentifierType");
            }
            row.strike = equity_instrument_projection_decimal_of(entry, "Strike");
            if (const auto option = entry.child("OptionData")) {
                row.long_short = equity_instrument_projection_text_of(*option, "LongShort");
                row.option_type = equity_instrument_projection_option_type_of(option);
                row.exercise_type = equity_instrument_projection_exercise_style_of(option);
                row.settlement_type =
                    equity_instrument_projection_spelling_of(*option, "Settlement");
                if (const auto dates = option->child("ExerciseDates")) {
                    const auto values = dates->values("ExerciseDate");
                    if (!values.empty())
                        row.exercise_date =
                            equity_instrument_projection_to_domain_date(values.front());
                }
            }
            equity_instrument_projection_stamp_columns(row);
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

auto equity_instrument_projection_flag_text(bool value) -> std::string_view {
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
        option.child("ExerciseDates")
            .append_value("ExerciseDate", equity_instrument_projection_to_ore_date(v.expiry_date));
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


// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

/*
 * A product that states its underlying as a Name or an Underlying block
 * writes the block from the kept rows, and the name its row keeps when the
 * trade was stored before the rows were kept.
 */
void write_name_or_underlying(record_writer& data,
                              const equity_instrument_data& carrier,
                              const std::string& name) {
    if (!underlying_block::write_underlyings(data, "Underlying", carrier, "underlying"))
        data.set_text("Name", name);
}

void write_underlying_or_equity(record_writer& data,
                                const equity_instrument_data& carrier,
                                const std::string& name) {
    if (underlying_block::write_underlyings(data, "Underlying", carrier, "underlying"))
        return;
    auto underlying = data.child("Underlying");
    underlying.set_text("Name", name);
    underlying.set_text("Type", "Equity");
}

void write_equity_option(record_writer& trade,
                         const equity_option_instrument& v,
                         const equity_instrument_data& carrier) {
    auto data = trade.child("EquityOptionData");
    write_option_entry(data.child("OptionData"), v);
    write_name_or_underlying(data, carrier, v.underlying_name);
    data.set_text("Currency", v.currency);
    data.set_decimal("Quantity", v.notional);
    if (!v.strike.is_zero())
        data.set_text("Strike", v.strike.to_string());
}

void write_equity_forward(record_writer& trade,
                          const equity_forward_instrument& v,
                          const equity_instrument_data& carrier) {
    auto data = trade.child("EquityForwardData");
    data.set_spelling("LongShort", "Long", "long short");
    data.set_text("Maturity", equity_instrument_projection_to_ore_date(v.expiry_date));
    write_name_or_underlying(data, carrier, v.underlying_name);
    data.set_text("Currency", v.currency);
    data.set_decimal("Strike", v.forward_price.value_or(decimal{}));
    set_decimal_from_double(data, "Quantity", v.quantity);
}

// The per-type row states one notional and the document states it on two legs,
// so the same amount is written to both.
void write_equity_swap(record_writer& trade,
                       const equity_swap_instrument& v,
                       const equity_instrument_data& carrier) {
    auto data = trade.child("EquitySwapData");
    if (!carrier.legs.empty()) {
        const auto stated = [](const std::chrono::year_month_day& d) {
            return d.ok() ? std::optional(d) : std::nullopt;
        };
        leg_block::write_legs(data, stated(v.start_date), stated(v.maturity_date), carrier);
        return;
    }

    // A swap stored before its legs were rows states only its summary, so its
    // legs are rebuilt from it.

    auto equity_leg = data.append("LegData");
    equity_leg.set_spelling("LegType", "Equity", "leg type");
    equity_leg.set_text("Payer", equity_instrument_projection_flag_text(true));
    auto equity = equity_leg.child("EquityLegData");
    equity.set_text("Name", v.underlying_name);
    equity.set_text("ReturnType", v.return_type);
    if (!v.notional.is_zero())
        equity.set_decimal("Quantity", v.notional);

    auto funding_leg = data.append("LegData");
    funding_leg.set_spelling("LegType", "Floating", "leg type");
    funding_leg.set_text("Payer", equity_instrument_projection_flag_text(false));
    if (!v.currency.empty())
        funding_leg.set_text("Currency", v.currency);
    if (!v.notional.is_zero())
        funding_leg.child("Notionals").append("Notional").set_own_text(v.notional.to_string());
}

void write_equity_variance_swap(record_writer& trade,
                                const equity_variance_swap_instrument& v,
                                const equity_instrument_data& carrier) {
    auto data = trade.child("EquityVarianceSwapData");
    data.set_text("StartDate", equity_instrument_projection_to_ore_date(v.start_date));
    data.set_text("EndDate", equity_instrument_projection_to_ore_date(v.maturity_date));
    data.set_spelling("Currency", v.currency, "currency");
    write_name_or_underlying(data, carrier, v.underlying_name);
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
                                 bool writes_upper,
                                 const equity_instrument_data& carrier) {
    auto data = trade.child(element);
    write_option_entry(data.child("OptionData"), v);
    write_name_or_underlying(data, carrier, v.underlying_name);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Quantity", v.notional);
    if (!v.strike.is_zero())
        data.set_text("Strike", v.strike.to_string());
    if (barrier_block::write_kept_barriers(data, "", carrier, "barrier"))
        return;
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

void write_equity_asian_option(record_writer& trade,
                               const equity_asian_option_instrument& v,
                               const equity_instrument_data& carrier) {
    auto data = trade.child("EquityAsianOptionData");
    write_option_entry(data.child("OptionData"), v);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Quantity", v.notional);
    if (!v.strike.is_zero())
        data.child("StrikeData").set_decimal("Value", v.strike);
    if (!underlying_block::write_underlyings(data, "Underlying", carrier, "underlying") &&
        !v.underlying_name.empty()) {
        auto underlying = data.child("Underlying");
        underlying.set_text("Name", v.underlying_name);
        underlying.set_text("Type", "Equity");
    }
    option_block::write_product_schedule(data, carrier, "ObservationDates", "observation_schedule");
}

void write_equity_digital_option(record_writer& trade,
                                 const equity_digital_option_instrument& v,
                                 const equity_instrument_data& carrier) {
    auto data = trade.child("EquityDigitalOptionData");
    write_option_entry(data.child("OptionData"), v);
    write_name_or_underlying(data, carrier, v.underlying_name);
    data.set_decimal("Strike", v.strike.value_or(decimal{}));
    if (!v.currency.empty())
        data.set_spelling("PayoffCurrency", v.currency, "currency");
    data.set_decimal("PayoffAmount", v.notional);
    // The schema requires a quantity and the row keeps the payoff amount alone,
    // which the export states as the payout of one unit.
    data.set_decimal("Quantity", decimal_from_double(1.0));
}

void write_equity_touch_option(record_writer& trade,
                               const equity_digital_option_instrument& v,
                               const equity_instrument_data& carrier) {
    auto data = trade.child("EquityTouchOptionData");
    write_option_entry(data.child("OptionData"), v);
    write_name_or_underlying(data, carrier, v.underlying_name);
    data.set_spelling("PayoffCurrency", v.currency, "currency");
    data.set_decimal("PayoffAmount", v.notional);
    if (barrier_block::write_kept_barriers(data, "", carrier, "barrier"))
        return;
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

void write_equity_accumulator(record_writer& trade,
                              const equity_accumulator_instrument& v,
                              const std::vector<range_bound>& rows,
                              const equity_instrument_data& carrier) {
    auto data = trade.child("EquityAccumulatorData");
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("FixingAmount", v.fixing_amount);
    if (!v.strike.is_zero())
        data.set_decimal("Strike", v.strike);
    write_underlying_or_equity(data, carrier, v.underlying_name);
    data.child("OptionData").set_text("LongShort", "Long");
    if (v.start_date.ok())
        data.set_text("StartDate", equity_instrument_projection_to_ore_date(v.start_date));

    if (!option_block::write_product_schedule(
            data, carrier, "ObservationDates", "observation_schedule"))
        write_schedule_rule(data.child("ObservationDates"),
                            equity_instrument_projection_to_ore_date(v.start_date),
                            "1D");
    option_block::write_product_schedule(data, carrier, "PricingDates", "pricing_schedule");

    /*
     * The document requires at least one range bound, so a row set that holds
     * none is written as none and the schema refuses it: the bounds are the
     * economics of the product, not a decoration.
     */
    option_block::write_range_bounds(data, rows);
    barrier_block::write_kept_barriers(data, "Barriers", carrier, "barrier");
}

void write_equity_tarf(record_writer& trade,
                       const equity_accumulator_instrument& v,
                       const equity_instrument_data& carrier) {
    auto data = trade.child("EquityTaRFData");
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("FixingAmount", v.fixing_amount);
    if (!v.strike.is_zero())
        data.set_decimal("Strike", v.strike);
    write_underlying_or_equity(data, carrier, v.underlying_name);
    data.child("OptionData").set_text("LongShort", "Long");
    if (v.target_amount)
        data.set_decimal("TargetAmount", *v.target_amount);
    option_block::write_range_bounds(data, carrier.range_bounds);
    if (!option_block::write_product_schedule(data, carrier, "ScheduleData", "product_schedule"))
        write_schedule_rule(data.child("ScheduleData"), "", "1Y");

    // The schema requires a barrier. A row stored before the barriers were kept
    // states only its cap, and that is the barrier it writes.
    if (!barrier_block::write_kept_barriers(data, "Barriers", carrier, "barrier")) {
        auto barrier = data.child("Barriers").append("BarrierData");
        barrier.set_spelling("Type", "FixingCap", "barrier type");
        barrier.child("Levels").append_decimal("Level", v.knock_out_level.value_or(decimal{}));
    }
}

void write_equity_cliquet_option(record_writer& trade,
                                 const equity_option_instrument& v,
                                 const equity_instrument_data& carrier) {
    auto data = trade.child("EquityCliquetOptionData");
    write_name_or_underlying(data, carrier, v.underlying_name);
    data.set_spelling("Currency", v.currency, "currency");
    data.set_decimal("Notional", v.notional);
    data.set_spelling("LongShort", v.long_short.empty() ? "Long" : v.long_short, "long short");
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
    if (!option_block::write_product_schedule(data, carrier, "ScheduleData", "product_schedule")) {
        auto schedule = data.child("ScheduleData");
        if (!v.cliquet_frequency.empty())
            write_schedule_rule(schedule, "", v.cliquet_frequency);
    }
}

void write_equity_worst_of_basket_swap(record_writer& trade,
                                       const equity_swap_instrument& v,
                                       const std::vector<equity_basket_constituent>& rows,
                                       const std::vector<equity_swap_period>& periods,
                                       const equity_instrument_data& carrier) {
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

    /*
     * The lists come back from the rows in ordinal order: a price per
     * constituent, and a level per period. A row written before the rows
     * existed states no constituent, so the fact carries the single name.
     */
    auto ordered_rows = rows;
    std::sort(ordered_rows.begin(), ordered_rows.end(), [](const auto& lhs, const auto& rhs) {
        return lhs.sequence_number < rhs.sequence_number;
    });
    struct named_constituent {
        std::string name;
        std::string kind;
        std::optional<std::string> identifier_type;
    };
    std::vector<named_constituent> members;
    std::vector<decimal> prices;
    for (const auto& row : ordered_rows) {
        if (row.underlying_name.empty())
            continue;
        members.push_back({row.underlying_name, row.underlying_kind, row.identifier_type});
        if (row.initial_price.has_value())
            prices.push_back(*row.initial_price);
    }
    if (members.empty() && !v.underlying_name.empty())
        members.push_back({v.underlying_name, "Equity", std::nullopt});
    if (!members.empty()) {
        auto underlyings = data.child("Underlyings");
        for (const auto& member : members) {
            auto underlying = underlyings.append("Underlying");
            underlying.set_text("Name", member.name);
            underlying.set_text("Type", member.kind);
            if (member.identifier_type)
                underlying.set_text("IdentifierType", *member.identifier_type);
        }
    }

    // The schema requires a rule on each schedule and the row states none.
    if (!option_block::write_product_schedule(
            data, carrier, "FloatingPeriodSchedule", "floating_period_schedule"))
        write_schedule_rule(data.child("FloatingPeriodSchedule"), "", "6M");
    if (!option_block::write_product_schedule(
            data, carrier, "FloatingPayDates", "floating_pay_schedule"))
        write_schedule_rule(data.child("FloatingPayDates"), "", "6M");
    option_block::write_product_schedule(
        data, carrier, "FloatingFixingSchedule", "floating_fixing_schedule");

    write_levels(data, "InitialPrices", "InitialPrice", prices);

    auto ordered_periods = periods;
    std::sort(ordered_periods.begin(), ordered_periods.end(), [](const auto& lhs, const auto& rhs) {
        return lhs.sequence_number < rhs.sequence_number;
    });
    std::vector<decimal> knock_outs;
    std::vector<decimal> triggers;
    for (const auto& period : ordered_periods) {
        if (period.knock_out_level.has_value())
            knock_outs.push_back(decimal::from_double(*period.knock_out_level).value());
        if (period.fixed_trigger_level.has_value())
            triggers.push_back(decimal::from_double(*period.fixed_trigger_level).value());
    }
    write_levels(data, "KnockOutLevels", "KnockOutLevel", knock_outs);
    write_levels(data, "FixedTriggerLevels", "FixedTriggerLevel", triggers);
}

void write_equity_position(record_writer& trade,
                           const equity_position_instrument& v,
                           const equity_instrument_data& carrier) {
    auto data = trade.child("EquityPositionData");
    set_decimal_from_double(data, "Quantity", v.quantity);
    if (!underlying_block::write_underlyings(data, "Underlying", carrier, "underlying") &&
        !v.underlying_name.empty()) {
        auto underlying = data.append("Underlying");
        underlying.set_text("Type", "Equity");
        underlying.set_text("Name", v.underlying_name);
    }
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
        underlying.set_text("Type", "Equity");
        underlying.set_text("Name", row->underlying_name);
        if (row->identifier_type)
            underlying.set_text("IdentifierType", *row->identifier_type);
        if (row->weight)
            set_decimal_from_double(underlying, "Weight", *row->weight);
        auto option = entry.child("OptionData");
        if (row->exercise_date)
            option.child("ExerciseDates")
                .append_value("ExerciseDate",
                              equity_instrument_projection_to_ore_date(*row->exercise_date));
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

/**
 * @brief The element of an equity product that states an OptionData block, or
 * nothing for a product that states none.
 *
 * Listed rather than probed, because asking a record type for a field it does
 * not declare is an error.
 */
/// The single-valued terms each equity product row keeps, by its element.
constexpr scalar_terms::term<equity_variance_swap_instrument> equity_variance_swap_terms[] = {
    {"Calendar", &equity_variance_swap_instrument::calendar},
    {"MomentType", &equity_variance_swap_instrument::moment_type},
    {"AddPastDividends", &equity_variance_swap_instrument::add_past_dividends},
};

constexpr scalar_terms::term<equity_accumulator_instrument> equity_accumulator_terms[] = {
    {"SettlementLag", &equity_accumulator_instrument::settlement_lag},
    {"SettlementCalendar", &equity_accumulator_instrument::settlement_calendar},
    {"SettlementConvention", &equity_accumulator_instrument::settlement_convention},
};

constexpr scalar_terms::term<equity_option_instrument> equity_option_terms[] = {
    {"StrikeCurrency", &equity_option_instrument::strike_currency},
    {"LocalCap", &equity_option_instrument::local_cap},
    {"LocalFloor", &equity_option_instrument::local_floor},
    {"GlobalCap", &equity_option_instrument::global_cap},
    {"GlobalFloor", &equity_option_instrument::global_floor},
    {"SettlementDays", &equity_option_instrument::settlement_days},
    {"Premium", &equity_option_instrument::premium},
    {"PremiumPaymentDate", &equity_option_instrument::premium_payment_date},
    {"InitialPrice1", &equity_option_instrument::initial_price_1},
    {"InitialPrice2", &equity_option_instrument::initial_price_2},
    {"KnockInPrice", &equity_option_instrument::knock_in_price},
    {"KnockOutPrice", &equity_option_instrument::knock_out_price},
};

constexpr scalar_terms::term<equity_swap_instrument> equity_swap_terms[] = {
    {"FixedRate", &equity_swap_instrument::fixed_rate},
    {"FloatingIndex", &equity_swap_instrument::floating_index},
    {"FloatingSpread", &equity_swap_instrument::floating_spread},
    {"FloatingDayCountFraction", &equity_swap_instrument::floating_day_count_fraction},
    {"InitialFixedRate", &equity_swap_instrument::initial_fixed_rate},
    {"KnockInLevel", &equity_swap_instrument::knock_in_level},
    {"BermudanKnockIn", &equity_swap_instrument::bermudan_knock_in},
    {"AccruingFixedCoupons", &equity_swap_instrument::accruing_fixed_coupons},
    {"AccumulatingFixedCoupons", &equity_swap_instrument::accumulating_fixed_coupons},
};

constexpr scalar_terms::term<equity_asian_option_instrument> equity_asian_option_terms[] = {
    {"Settlement", &equity_asian_option_instrument::settlement},
};

constexpr scalar_terms::term<equity_digital_option_instrument> equity_digital_option_terms[] = {
    {"Quantity", &equity_digital_option_instrument::quantity},
};

/// Calls the function with the row's term table, for a row that keeps terms.
template <typename Row, typename Fn>
void with_equity_terms(Fn&& fn) {
    if constexpr (std::is_same_v<Row, equity_variance_swap_instrument>)
        fn(equity_variance_swap_terms);
    else if constexpr (std::is_same_v<Row, equity_accumulator_instrument>)
        fn(equity_accumulator_terms);
    else if constexpr (std::is_same_v<Row, equity_option_instrument>)
        fn(equity_option_terms);
    else if constexpr (std::is_same_v<Row, equity_asian_option_instrument>)
        fn(equity_asian_option_terms);
    else if constexpr (std::is_same_v<Row, equity_swap_instrument>)
        fn(equity_swap_terms);
    else if constexpr (std::is_same_v<Row, equity_digital_option_instrument>)
        fn(equity_digital_option_terms);
}

/// Where an equity product states its underlyings.
constexpr underlying_block::underlying_holder equity_underlying_holders[] = {
    {"EquityOption", "", "Underlying", "underlying"},
    {"EquityForward", "", "Underlying", "underlying"},
    {"EquityVarianceSwap", "", "Underlying", "underlying"},
    {"EquityBarrierOption", "", "Underlying", "underlying"},
    {"EquityDoubleBarrierOption", "", "Underlying", "underlying"},
    {"EquityEuropeanBarrierOption", "", "Underlying", "underlying"},
    {"EquityAsianOption", "", "Underlying", "underlying"},
    {"EquityDigitalOption", "", "Underlying", "underlying"},
    {"EquityTouchOption", "", "Underlying", "underlying"},
    {"EquityCliquetOption", "", "Underlying", "underlying"},
    {"EquityAccumulator", "", "Underlying", "underlying"},
    {"EquityTaRF", "", "Underlying", "underlying"},
    {"EquityPosition", "", "Underlying", "underlying"},
};

/// Where an equity product states its barriers.
constexpr barrier_block::barrier_holder equity_barrier_holders[] = {
    {"EquityBarrierOption", "", "barrier"},
    {"EquityDoubleBarrierOption", "", "barrier"},
    {"EquityEuropeanBarrierOption", "", "barrier"},
    {"EquityTouchOption", "", "barrier"},
    {"EquityAccumulator", "Barriers", "barrier"},
    {"EquityTaRF", "Barriers", "barrier"},
};

/// The schedules an equity product states on its own element.
constexpr option_block::product_schedule equity_product_schedules[] = {
    {"EquityAccumulator", "ObservationDates", "observation_schedule"},
    {"EquityAccumulator", "PricingDates", "pricing_schedule"},
    {"EquityAsianOption", "ObservationDates", "observation_schedule"},
    {"EquityTaRF", "ScheduleData", "product_schedule"},
    {"EquityCliquetOption", "ScheduleData", "product_schedule"},
    {"EquityWorstOfBasketSwap", "FloatingPeriodSchedule", "floating_period_schedule"},
    {"EquityWorstOfBasketSwap", "FloatingPayDates", "floating_pay_schedule"},
    {"EquityWorstOfBasketSwap", "FloatingFixingSchedule", "floating_fixing_schedule"},
};

std::optional<std::string> equity_option_data_element(std::string_view trade_type) {
    static constexpr std::string_view with_option_data[] = {
        "EquityOption",
        "EquityBarrierOption",
        "EquityDoubleBarrierOption",
        "EquityEuropeanBarrierOption",
        "EquityAsianOption",
        "EquityDigitalOption",
        "EquityTouchOption",
        "EquityOutperformanceOption",
        "EquityAccumulator",
        "EquityTaRF",
    };
    for (const auto type : with_option_data)
        if (type == trade_type)
            return std::string(trade_type) + "Data";
    return std::nullopt;
}

auto read_equity_product(const xml::record_reader& trade,
                         std::string_view trade_type) -> trading::domain::trade_instrument {
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
        return trading::domain::trade_instrument{read_equity_swap(trade)};
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
        return read_equity_accumulator(trade);
    if (trade_type == "EquityTaRF")
        return wrap(read_equity_tarf(trade));
    if (trade_type == "EquityCliquetOption")
        return wrap(read_equity_cliquet_option(trade));
    return read_equity_worst_of_basket_swap(trade);
}

}

auto read_equity_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_equity_trade_type(trade_type))
        return std::nullopt;

    auto instrument = read_equity_product(trade, trade_type);
    // The product's row keeps its own terms; the block's shared terms land in
    // the option rows every option family reads and writes.
    if (auto* data = std::get_if<equity_instrument_data>(&instrument)) {
        if (const auto element = equity_option_data_element(trade_type)) {
            if (const auto product = trade.child(*element)) {
                if (const auto option = product->child("OptionData"))
                    option_block::read_option_block(*data, *option);
            }
        }
        option_block::read_product_schedules(*data, trade, trade_type, equity_product_schedules);
        barrier_block::read_product_barriers(*data, trade, trade_type, equity_barrier_holders);
        underlying_block::read_product_underlyings(
            *data, trade, trade_type, equity_underlying_holders);
        const auto element = std::string(trade_type) + "Data";
        if (const auto product = trade.declares(element) ? trade.child(element) : std::nullopt) {
            if (trade_type == "EquityAccumulator" || trade_type == "EquityTaRF")
                option_block::read_range_bounds(*data, *product);
            std::visit(
                [&](auto& row) {
                    using Row = std::decay_t<decltype(row)>;
                    if constexpr (barrier_block::monitored<Row>)
                        barrier_block::read_monitoring(row, *product, "EQIndex");
                    with_equity_terms<Row>(
                        [&](const auto& terms) { scalar_terms::read_terms(row, *product, terms); });
                    if constexpr (std::is_same_v<Row, equity_asian_option_instrument>)
                        if (const auto strike = product->child("StrikeData"))
                            if (strike->has("Currency"))
                                row.strike_currency = scalar_terms::read_text(*strike, "Currency");
                },
                data->instrument);
        }
    }
    return instrument;
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
                    write_equity_position(trade, row, *v);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_option_instrument>) {
                if (trade_type == "EquityOption") {
                    write_equity_option(trade, row, *v);
                    matched = true;
                } else if (trade_type == "EquityCliquetOption") {
                    write_equity_cliquet_option(trade, row, *v);
                    matched = true;
                } else if (trade_type == "EquityOutperformanceOption") {
                    write_equity_outperformance_option(trade, row);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_forward_instrument>) {
                write_equity_forward(trade, row, *v);
                matched = true;
            } else if constexpr (std::is_same_v<Row, equity_swap_instrument>) {
                if (trade_type == "EquitySwap") {
                    write_equity_swap(trade, row, *v);
                    matched = true;
                } else if (trade_type == "EquityWorstOfBasketSwap") {
                    write_equity_worst_of_basket_swap(
                        trade, row, v->basket_constituents, v->periods, *v);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_variance_swap_instrument>) {
                write_equity_variance_swap(trade, row, *v);
                matched = true;
            } else if constexpr (std::is_same_v<Row, equity_barrier_option_instrument>) {
                if (trade_type == "EquityBarrierOption") {
                    write_equity_barrier_option(trade, "EquityBarrierOptionData", row, false, *v);
                    matched = true;
                } else if (trade_type == "EquityDoubleBarrierOption") {
                    write_equity_barrier_option(
                        trade, "EquityDoubleBarrierOptionData", row, true, *v);
                    matched = true;
                } else if (trade_type == "EquityEuropeanBarrierOption") {
                    write_equity_barrier_option(
                        trade, "EquityEuropeanBarrierOptionData", row, true, *v);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_asian_option_instrument>) {
                write_equity_asian_option(trade, row, *v);
                matched = true;
            } else if constexpr (std::is_same_v<Row, equity_digital_option_instrument>) {
                if (trade_type == "EquityDigitalOption") {
                    write_equity_digital_option(trade, row, *v);
                    matched = true;
                } else if (trade_type == "EquityTouchOption") {
                    write_equity_touch_option(trade, row, *v);
                    matched = true;
                }
            } else if constexpr (std::is_same_v<Row, equity_accumulator_instrument>) {
                if (trade_type == "EquityAccumulator") {
                    write_equity_accumulator(trade, row, v->range_bounds, *v);
                    matched = true;
                } else if (trade_type == "EquityTaRF") {
                    write_equity_tarf(trade, row, *v);
                    matched = true;
                }
            }
        },
        v->instrument);

    if (matched && !v->options.empty()) {
        if (const auto element = equity_option_data_element(trade_type)) {
            if (auto product = trade.chosen_child(*element)) {
                auto option = product->child("OptionData");
                option_block::write_option_block(option, *v);
            }
        }
    }
    if (const auto element = std::string(trade_type) + "Data"; matched && trade.declares(element)) {
        if (auto product = trade.chosen_child(element)) {
            std::visit(
                [&](const auto& row) {
                    using Row = std::decay_t<decltype(row)>;
                    if constexpr (barrier_block::monitored<Row>)
                        barrier_block::write_monitoring(*product, row, "EQIndex");
                    with_equity_terms<Row>([&](const auto& terms) {
                        scalar_terms::write_terms(*product, row, terms);
                    });
                    if constexpr (std::is_same_v<Row, equity_asian_option_instrument>)
                        if (row.strike_currency) {
                            auto strike = product->child("StrikeData");
                            scalar_terms::write_text(strike, "Currency", *row.strike_currency);
                        }
                },
                v->instrument);
        }
    }
    return matched;
}

}
