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
#ifndef ORES_ORE_CORE_DOMAIN_LEG_BLOCK_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_LEG_BLOCK_PROJECTION_HPP

#include "ores.ore.core/domain/decimal_projection.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.trading.api/domain/instrument_schedule_date.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amortization.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_indexing.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file leg_block_projection.hpp
 * @brief ORE's LegData block, read into and written from the shared swap leg
 * rows, for any family whose composite holds them.
 *
 * A composite reads and writes legs when it holds the vectors the rows land
 * in: legs, leg_amounts, leg_rates, leg_amortizations, schedules and
 * schedule_dates. ORE states a leg the same way in a swap, a commodity swap,
 * an equity swap and a total return swap's funding, so one reader and one
 * writer serve them all.
 */

namespace ores::ore::domain::leg_block {

using ores::utility::decimal::decimal;
using trading::domain::swap_leg;
using trading::domain::swap_leg_amortization;
using trading::domain::swap_leg_amount;
using trading::domain::swap_leg_indexing;
using trading::domain::swap_leg_rate;
using xml::record_reader;
using xml::record_writer;

using option_block::decimal_of;
using option_block::decimal_text;
using option_block::flag_text;
using option_block::is_flag_on;
using option_block::set_spelling_or;
using option_block::spelling_of;
using option_block::stamp_row;
using option_block::text_of;
using option_block::to_domain_date;
using option_block::to_optional_domain_date;
using option_block::to_ore_date;

/**
 * @brief Whether a leg's record type declares a field.
 *
 * ORE states a leg with more than one record type: a cap/floor's LegData
 * declares fewer sections than a swap's, and states its currency as an
 * enumeration rather than text. Asking a type for a field it does not declare
 * is an error, so the block asks first.
 */
template <typename Record>
bool declares(const Record& r, std::string_view name) {
    return xml::find_field(r.type(), name, false) != nullptr;
}

inline std::optional<record_reader> child_if(const record_reader& r, std::string_view name) {
    return declares(r, name) ? r.child(name) : std::nullopt;
}

/*
 * Some payloads, the digital CMS ones among them, are generated with every
 * element repeatable although the document states each once. These read and
 * write that one element whichever way the record declares it.
 */
inline std::optional<record_reader> single_child(const record_reader& r, std::string_view name) {
    const auto* f = xml::find_field(r.type(), name, false);
    if (f == nullptr)
        return std::nullopt;
    if (f->kind == xml::field_kind::repeated_nested) {
        auto all = r.children(name);
        if (all.empty())
            return std::nullopt;
        return all.front();
    }
    return r.child(name);
}

inline std::optional<std::string> single_value(const record_reader& r, std::string_view name) {
    const auto* f = xml::find_field(r.type(), name, false);
    if (f == nullptr)
        return std::nullopt;
    if (f->kind == xml::field_kind::repeated_leaf) {
        auto all = r.values(name);
        if (all.empty())
            return std::nullopt;
        return all.front();
    }
    if (!r.has(name))
        return std::nullopt;
    return r.text(name);
}

inline record_writer single_child_writer(record_writer& w, std::string_view name) {
    const auto* f = xml::find_field(w.type(), name, false);
    if (f != nullptr && f->kind == xml::field_kind::repeated_nested)
        return w.append(name);
    return w.child(name);
}

inline void set_single_value(record_writer& w, std::string_view name, std::string_view value) {
    const auto* f = xml::find_field(w.type(), name, false);
    if (f != nullptr && f->kind == xml::field_kind::repeated_leaf)
        w.append_value(name, value);
    else
        w.set_text(name, value);
}

inline bool has_if(const record_reader& r, std::string_view name) {
    return declares(r, name) && r.has(name);
}

/// A scalar the record states as text or as an enumeration, read as written.
inline std::string scalar_of(const record_reader& r, std::string_view name) {
    const auto* f = xml::find_field(r.type(), name, false);
    if (f == nullptr)
        return {};
    return f->kind == xml::field_kind::enumeration ? spelling_of(r, name) : text_of(r, name);
}

inline void set_scalar(record_writer& w, std::string_view name, const std::string& value) {
    const auto* f = xml::find_field(w.type(), name, false);
    if (f != nullptr && f->kind == xml::field_kind::enumeration)
        w.set_spelling(name, value, name);
    else
        w.set_text(name, value);
}

inline void stamp_leg(swap_leg& row) {
    row.audit.modified_by = std::string(option_block::block_actor);
    row.audit.performed_by = std::string(option_block::block_actor);
    row.audit.change_reason_code = std::string(option_block::block_change_reason);
    row.audit.change_commentary = std::string(option_block::block_change_commentary);
}

/// A flag the element states, or nothing when it states none.
inline std::optional<bool> optional_flag(const record_reader& r, std::string_view name) {
    if (!r.has(name))
        return std::nullopt;
    return is_flag_on(text_of(r, name));
}

/// The first schedule rule's tenor, which is the leg's payment frequency.
inline auto first_tenor(const std::optional<record_reader>& schedule) -> std::string {
    if (!schedule)
        return {};
    const auto rules = schedule->children("Rules");
    if (rules.empty())
        return {};
    return text_of(rules.front(), "Tenor");
}

/**
 * @brief A CMS payload: a leg type, the element that carries it and the index
 * fields it names.
 *
 * The three CMS payloads have the floating leg's shape (arrears, fixing days,
 * naked option and the four rate lists) and differ only in their index fields
 * and, for the duration adjusted one, a duration.
 */
struct cms_payload {
    std::string_view leg_type;
    std::string_view element;
    std::string_view index;
    std::string_view index_2;
    bool has_duration;
};

inline constexpr cms_payload cms_payloads[] = {
    {"CMS", "CMSLegData", "Index", "", false},
    {"DurationAdjustedCMS", "DurationAdjustedCMSLegData", "Index", "", true},
    {"CMSSpread", "CMSSpreadLegData", "Index1", "Index2", false},
};

/**
 * @brief A digital CMS payload: the CMS payload it wraps, by its place in
 * cms_payloads, and the call and put digitals around it.
 */
struct digital_payload {
    std::string_view leg_type;
    std::string_view element;
    std::size_t cms;
};

inline constexpr digital_payload digital_payloads[] = {
    {"DigitalCMS", "DigitalCMSLegData", 0},
    {"DigitalCMSSpread", "DigitalCMSSpreadLegData", 2},
};

inline void read_cms_terms(swap_leg& row, const record_reader& cms, const cms_payload& payload) {
    row.floating_index_code = text_of(cms, payload.index);
    if (!payload.index_2.empty())
        row.floating_index_code_2 = text_of(cms, payload.index_2);
    if (payload.has_duration)
        if (const auto duration = cms.integer("Duration"))
            row.cms_duration = static_cast<int>(*duration);
    if (const auto days = cms.integer("FixingDays"))
        row.fixing_days = static_cast<int>(*days);
    row.is_in_arrears = optional_flag(cms, "IsInArrears");
    row.naked_option = optional_flag(cms, "NakedOption");
}

inline void read_equity_terms(swap_leg& row, const record_reader& equity) {
    if (equity.has("ReturnType"))
        row.equity_return_type = text_of(equity, "ReturnType");
    if (has_if(equity, "Name")) {
        row.equity_name = text_of(equity, "Name");
    } else if (const auto underlying = child_if(equity, "Underlying")) {
        row.equity_underlying_type = text_of(*underlying, "Type");
        row.equity_name = text_of(*underlying, "Name");
        if (underlying->has("IdentifierType"))
            row.equity_underlying_identifier_type = text_of(*underlying, "IdentifierType");
    }
    row.equity_quantity = decimal_as_double(equity, "Quantity");
    row.equity_initial_price = equity.decimal("InitialPrice");
    if (equity.has("InitialPriceCurrency"))
        row.equity_initial_price_currency = text_of(equity, "InitialPriceCurrency");
    row.equity_dividend_factor = equity.decimal("DividendFactor");
    row.equity_notional_reset = optional_flag(equity, "NotionalReset");
    if (const auto days = equity.integer("FixingDays"))
        row.fixing_days = static_cast<int>(*days);
    if (const auto terms = equity.child("FXTerms")) {
        if (terms->has("EquityCurrency"))
            row.equity_currency = text_of(*terms, "EquityCurrency");
        if (terms->has("FXIndex"))
            row.fx_index = text_of(*terms, "FXIndex");
    }
}

/// A commodity leg's single-valued terms, by the payload that states them.
inline constexpr scalar_terms::term<swap_leg> commodity_fixed_leg_terms[] = {
    {"CommodityPayRelativeTo", &swap_leg::commodity_pay_relative_to},
};

inline constexpr scalar_terms::term<swap_leg> commodity_floating_leg_terms[] = {
    {"CommodityPayRelativeTo", &swap_leg::commodity_pay_relative_to},
    {"CommodityQuantityFrequency", &swap_leg::commodity_quantity_frequency},
    {"PricingDateRule", &swap_leg::pricing_date_rule},
};

inline swap_leg read_leg(const record_reader& leg, int leg_number) {
    swap_leg row;
    row.identity.leg_number = leg_number;
    row.payer = is_flag_on(text_of(leg, "Payer"));
    row.leg_type_code = spelling_of(leg, "LegType");
    row.currency = scalar_of(leg, "Currency");
    row.day_count_fraction_code = spelling_of(leg, "DayCounter");
    row.business_day_convention_code = spelling_of(leg, "PaymentConvention");
    row.payment_frequency_code =
        tenor_to_payment_frequency(first_tenor(child_if(leg, "ScheduleData")));
    if (has_if(leg, "PaymentLag"))
        row.payment_lag = text_of(leg, "PaymentLag");
    if (has_if(leg, "PaymentCalendar"))
        row.payment_calendar = text_of(leg, "PaymentCalendar");
    if (const auto notionals = child_if(leg, "Notionals")) {
        if (const auto all = notionals->children("Exchanges"); !all.empty()) {
            const auto& exchanges = all.front();
            const auto spelled = [&](std::string_view name) -> std::optional<bool> {
                const auto value = exchanges.spelling(name);
                if (!value)
                    return std::nullopt;
                return is_flag_on(*value);
            };
            row.notional_initial_exchange = spelled("NotionalInitialExchange");
            row.notional_final_exchange = spelled("NotionalFinalExchange");
        }
        if (const auto all = notionals->children("FXReset"); !all.empty()) {
            const auto& reset = all.front();
            row.fx_reset_foreign_currency = reset.spelling("ForeignCurrency");
            row.fx_reset_start_date = reset.text("StartDate");
            row.fx_reset_foreign_amount = reset.decimal("ForeignAmount");
            row.fx_reset_fx_index = reset.text("FXIndex");
            if (const auto days = reset.integer("FixingDays"))
                row.fx_reset_fixing_days = static_cast<int>(*days);
            row.fx_reset_fixing_calendar = reset.text("FixingCalendar");
        }
    }
    if (const auto floating = child_if(leg, "FloatingLegData")) {
        row.floating_index_code = text_of(*floating, "Index");
        if (const auto days = floating->integer("FixingDays"))
            row.fixing_days = static_cast<int>(*days);
        row.is_in_arrears = optional_flag(*floating, "IsInArrears");
        row.include_spread = optional_flag(*floating, "IncludeSpread");
        row.is_averaged = optional_flag(*floating, "IsAveraged");
        row.has_sub_periods = optional_flag(*floating, "HasSubPeriods");
        row.naked_option = optional_flag(*floating, "NakedOption");
        row.local_cap_floor = optional_flag(*floating, "LocalCapFloor");
        if (floating->has("Lookback"))
            row.lookback = text_of(*floating, "Lookback");
        if (const auto cutoff = floating->integer("RateCutoff"))
            row.rate_cutoff = static_cast<int>(*cutoff);
    }
    for (const auto& payload : cms_payloads)
        if (const auto cms = child_if(leg, payload.element))
            read_cms_terms(row, *cms, payload);
    for (const auto& digital : digital_payloads) {
        const auto payload = child_if(leg, digital.element);
        if (!payload)
            continue;
        const auto& cms = cms_payloads[digital.cms];
        if (const auto inner = single_child(*payload, cms.element))
            read_cms_terms(row, *inner, cms);
        const auto flag = [&](std::string_view name) -> std::optional<bool> {
            if (const auto value = single_value(*payload, name))
                return is_flag_on(*value);
            return std::nullopt;
        };
        row.digital_call_position = single_value(*payload, "CallPosition");
        row.digital_is_call_atm_included = flag("IsCallATMIncluded");
        row.digital_put_position = single_value(*payload, "PutPosition");
        row.digital_is_put_atm_included = flag("IsPutATMIncluded");
    }
    if (const auto formula = child_if(leg, "FormulaBasedLegData")) {
        row.floating_index_code = text_of(*formula, "Index");
        row.is_in_arrears = optional_flag(*formula, "IsInArrears");
        if (const auto days = formula->integer("FixingDays"))
            row.fixing_days = static_cast<int>(*days);
        if (formula->has("FixingCalendar"))
            row.fixing_calendar = text_of(*formula, "FixingCalendar");
    }
    if (const auto fixed = child_if(leg, "CommodityFixedLegData"))
        scalar_terms::read_terms(row, *fixed, commodity_fixed_leg_terms);
    if (const auto commodity = child_if(leg, "CommodityFloatingLegData")) {
        scalar_terms::read_terms(row, *commodity, commodity_floating_leg_terms);
        row.floating_index_code = text_of(*commodity, "Name");
        if (commodity->has("PriceType"))
            row.commodity_price_type = spelling_of(*commodity, "PriceType");
        if (commodity->has("PricingCalendar"))
            row.pricing_calendar = text_of(*commodity, "PricingCalendar");
        if (const auto offset = commodity->integer("FutureMonthOffset"))
            row.future_month_offset = static_cast<int>(*offset);
        if (const auto offset = commodity->integer("DailyExpiryOffset"))
            row.daily_expiry_offset = static_cast<int>(*offset);
        row.is_in_arrears = optional_flag(*commodity, "IsInArrears");
        row.is_averaged = optional_flag(*commodity, "IsAveraged");
    }
    if (const auto equity = child_if(leg, "EquityLegData"))
        read_equity_terms(row, *equity);
    if (const auto margin = child_if(leg, "EquityMarginLegData")) {
        row.equity_margin_initial_margin_factor = margin->decimal("InitialMarginFactor");
        row.equity_margin_multiplier = margin->decimal("Multiplier");
        if (const auto equity = margin->child("EquityLegData"))
            read_equity_terms(row, *equity);
    }
    if (const auto cpi = child_if(leg, "CPILegData")) {
        const auto first = [&](std::string_view name) -> std::optional<std::string> {
            const auto values = cpi->values(name);
            if (values.empty())
                return std::nullopt;
            return values.front();
        };
        row.floating_index_code = first("Index").value_or("");
        if (const auto base = cpi->decimals("BaseCPI"); !base.empty())
            row.cpi_base = base.front().to_double();
        if (const auto start = first("StartDate"))
            row.cpi_start_date = to_domain_date(*start);
        row.observation_lag = first("ObservationLag");
        row.cpi_interpolation = first("Interpolation");
        if (const auto flags = cpi->spellings("Interpolated"); !flags.empty())
            row.cpi_interpolated = is_flag_on(flags.front());
        if (const auto flags = cpi->spellings("SubtractInflationNotional"); !flags.empty())
            row.subtract_inflation_notional = is_flag_on(flags.front());
    }
    if (const auto yy = child_if(leg, "YYLegData")) {
        row.floating_index_code = text_of(*yy, "Index");
        if (const auto days = yy->integer("FixingDays"))
            row.fixing_days = static_cast<int>(*days);
        if (yy->has("ObservationLag"))
            row.observation_lag = text_of(*yy, "ObservationLag");
        row.naked_option = optional_flag(*yy, "NakedOption");
    }
    if (const auto zero = child_if(leg, "ZeroCouponFixedLegData")) {
        if (const auto values = zero->values("Compounding"); !values.empty())
            row.zero_coupon_compounding = values.front();
        if (const auto values = zero->values("SubtractNotional"); !values.empty())
            row.zero_coupon_subtract_notional = is_flag_on(values.front());
    }
    if (const auto indexings = child_if(leg, "Indexings"))
        row.indexings_from_asset_leg = optional_flag(*indexings, "FromAssetLeg");
    stamp_leg(row);
    return row;
}

/// The schedule role an indexing's valuation schedule is kept under.
inline std::string indexing_valuation_role(int sequence_number) {
    return "indexing_" + std::to_string(sequence_number) + "_valuation";
}

template <typename Data>
void read_leg_indexings(Data& result, const record_reader& leg, int leg_number) {
    const auto indexings = child_if(leg, "Indexings");
    if (!indexings)
        return;
    int sequence_number = 0;
    for (const auto& indexing : indexings->children("Indexing")) {
        swap_leg_indexing row;
        row.leg_number = leg_number;
        row.sequence_number = ++sequence_number;
        row.index_name = text_of(indexing, "Index");
        row.quantity = decimal_as_double(indexing, "Quantity");
        row.initial_fixing = indexing.decimal("InitialFixing");
        if (const auto days = indexing.integer("FixingDays"))
            row.fixing_days = static_cast<int>(*days);
        if (indexing.has("FixingCalendar"))
            row.fixing_calendar = text_of(indexing, "FixingCalendar");
        if (indexing.has("FixingConvention"))
            row.fixing_convention = spelling_of(indexing, "FixingConvention");
        row.is_in_arrears = optional_flag(indexing, "IsInArrears");
        stamp_row(row);
        result.leg_indexings.push_back(std::move(row));
        if (const auto schedule = indexing.child("ValuationSchedule"))
            option_block::read_schedule(
                result, *schedule, "swap", leg_number, indexing_valuation_role(sequence_number));
    }
}

template <typename Data>
void write_leg_indexings(record_writer& leg, const swap_leg& row, const Data& v) {
    const auto leg_number = row.identity.leg_number;
    const bool any = std::any_of(v.leg_indexings.begin(),
                                 v.leg_indexings.end(),
                                 [&](const auto& i) { return i.leg_number == leg_number; });
    if (!any && !row.indexings_from_asset_leg)
        return;
    auto group = leg.child("Indexings");
    if (row.indexings_from_asset_leg)
        group.set_text("FromAssetLeg", flag_text(*row.indexings_from_asset_leg));
    for (const auto& indexing : v.leg_indexings) {
        if (indexing.leg_number != leg_number)
            continue;
        auto writer = group.append("Indexing");
        if (indexing.quantity)
            set_decimal_from_double(writer, "Quantity", *indexing.quantity);
        writer.set_text("Index", indexing.index_name);
        if (indexing.initial_fixing)
            writer.set_decimal("InitialFixing", *indexing.initial_fixing);
        const auto role = indexing_valuation_role(indexing.sequence_number);
        if (!option_block::schedules_of(v, "swap", leg_number, role).empty())
            option_block::write_stored_schedule(
                writer.child("ValuationSchedule"), v, "swap", leg_number, role);
        if (indexing.fixing_days)
            writer.set_integer("FixingDays", *indexing.fixing_days);
        if (indexing.fixing_calendar)
            writer.set_text("FixingCalendar", *indexing.fixing_calendar);
        if (indexing.fixing_convention)
            set_spelling_or(writer,
                            "FixingConvention",
                            *indexing.fixing_convention,
                            "U",
                            "business day convention");
        if (indexing.is_in_arrears)
            writer.set_text("IsInArrears", flag_text(*indexing.is_in_arrears));
    }
}

/// A commodity leg states its notional as quantities on its own payload.
inline std::optional<record_reader> commodity_quantities(const record_reader& leg) {
    if (const auto fixed = child_if(leg, "CommodityFixedLegData"))
        return fixed->child("Quantities");
    if (const auto floating = child_if(leg, "CommodityFloatingLegData"))
        return floating->child("Quantities");
    return std::nullopt;
}

inline std::vector<swap_leg_amount> read_leg_amounts(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_amount> amounts;
    const auto append = [&](const decimal& value, const std::string& start_date) {
        swap_leg_amount row;
        row.leg_number = leg_number;
        row.sequence_number = static_cast<int>(amounts.size()) + 1;
        row.amount = value;
        if (!start_date.empty())
            row.start_date = to_domain_date(start_date);
        stamp_row(row);
        amounts.push_back(std::move(row));
    };

    // The document states its notional as a list, and a cashflow leg states its
    // amounts as dated payments. Both arms land in the same column. An
    // amortization schedule is a rule over the notional, not an amount, so it
    // keeps rows of its own.
    if (const auto notionals = child_if(leg, "Notionals")) {
        for (const auto& notional : notionals->children("Notional"))
            append(decimal_text(notional), text_of(notional, "startDate"));
    } else if (const auto cashflow_data = child_if(leg, "CashflowData")) {
        if (const auto cashflow = cashflow_data->child("Cashflow"))
            for (const auto& amount : cashflow->children("Amount"))
                append(decimal_text(amount), text_of(amount, "date"));
    } else if (const auto quantities = commodity_quantities(leg)) {
        for (const auto& quantity : quantities->children("Quantity"))
            append(decimal_text(quantity), text_of(quantity, "startDate"));
    }
    return amounts;
}

inline std::vector<swap_leg_amortization> read_leg_amortizations(const record_reader& leg,
                                                                 int leg_number) {
    std::vector<swap_leg_amortization> steps;
    const auto amortizations = child_if(leg, "Amortizations");
    if (!amortizations)
        return steps;
    for (const auto& step : amortizations->children("AmortizationData")) {
        swap_leg_amortization row;
        row.leg_number = leg_number;
        row.sequence_number = static_cast<int>(steps.size()) + 1;
        row.amortization_type = spelling_of(step, "Type");
        if (step.has("Value"))
            row.value = decimal_of(step, "Value");
        row.start_date = to_optional_domain_date(text_of(step, "StartDate"));
        row.end_date = to_optional_domain_date(text_of(step, "EndDate"));
        if (step.has("Frequency"))
            row.frequency = text_of(step, "Frequency");
        row.underflow = optional_flag(step, "Underflow");
        stamp_row(row);
        steps.push_back(std::move(row));
    }
    return steps;
}

/// The schedules a floating leg states, each as its list, its item and the
/// role a rate row keeps it under.
struct floating_rate_list {
    std::string_view element;
    std::string_view item;
    std::string_view role;
};

inline constexpr floating_rate_list floating_rate_lists[] = {
    {"Spreads", "Spread", "spread"},
    {"Gearings", "Gearing", "gearing"},
    {"Caps", "Cap", "cap"},
    {"Floors", "Floor", "floor"},
};

/// A digital CMS leg states its call and put strikes and payoffs as plain
/// lists, with no start date on any of them.
inline constexpr floating_rate_list digital_rate_lists[] = {
    {"CallStrikes", "Strike", "call_strike"},
    {"CallPayoffs", "Payoff", "call_payoff"},
    {"PutStrikes", "Strike", "put_strike"},
    {"PutPayoffs", "Payoff", "put_payoff"},
};

/// A commodity fixed leg states its prices.
inline constexpr floating_rate_list commodity_fixed_rate_lists[] = {
    {"Prices", "Price", "price"},
};

/// A commodity floating leg states spreads and gearings, and no caps or floors.
inline constexpr floating_rate_list commodity_floating_rate_lists[] = {
    {"Spreads", "Spread", "spread"},
    {"Gearings", "Gearing", "gearing"},
};

/// Writes each schedule a floating leg's rows state, in the order the schema
/// gives them.
template <std::size_t N>
void write_rate_lists(record_writer& floating,
                      const std::vector<swap_leg_rate>& rates,
                      const floating_rate_list (&lists)[N]) {
    for (const auto& [element, item, role] : lists) {
        const auto states = std::any_of(
            rates.begin(), rates.end(), [&](const auto& rate) { return rate.rate_role == role; });
        if (!states)
            continue;
        auto list = floating.child(element);
        for (const auto& rate : rates) {
            if (rate.rate_role != role)
                continue;
            auto writer = list.append(item);
            writer.set_own_text(rate.value.to_string());
            if (rate.start_date)
                writer.set_text("startDate", to_ore_date(*rate.start_date));
        }
    }
}

inline void write_floating_rates(record_writer& floating, const std::vector<swap_leg_rate>& rates) {
    write_rate_lists(floating, rates, floating_rate_lists);
}

/// A floating leg's fixings already known on the trade, each dated by its
/// fixing date rather than by the start date the rate lists state.
inline void write_historical_fixings(record_writer& floating,
                                     const std::vector<swap_leg_rate>& rates) {
    std::optional<record_writer> list;
    for (const auto& rate : rates) {
        if (rate.rate_role != "historical_fixing")
            continue;
        if (!list)
            list.emplace(floating.child("HistoricalFixings"));
        auto writer = list->append("Fixing");
        writer.set_own_text(rate.value.to_string());
        if (rate.start_date)
            writer.set_text("fixingDate", to_ore_date(*rate.start_date));
    }
}

inline void write_equity_terms(record_writer payload, const swap_leg& row) {
    if (row.equity_quantity)
        set_decimal_from_double(payload, "Quantity", *row.equity_quantity);
    if (row.equity_return_type)
        payload.set_text("ReturnType", *row.equity_return_type);
    if (row.equity_underlying_type) {
        auto underlying = payload.child("Underlying");
        underlying.set_text("Type", *row.equity_underlying_type);
        underlying.set_text("Name", row.equity_name.value_or(""));
        if (row.equity_underlying_identifier_type)
            underlying.set_text("IdentifierType", *row.equity_underlying_identifier_type);
    } else if (row.equity_name) {
        payload.set_text("Name", *row.equity_name);
    }
    if (row.equity_initial_price)
        payload.set_decimal("InitialPrice", *row.equity_initial_price);
    if (row.equity_initial_price_currency)
        payload.set_text("InitialPriceCurrency", *row.equity_initial_price_currency);
    if (row.equity_dividend_factor)
        payload.set_decimal("DividendFactor", *row.equity_dividend_factor);
    if (row.equity_notional_reset)
        payload.set_text("NotionalReset", flag_text(*row.equity_notional_reset));
    if (row.fixing_days)
        payload.set_integer("FixingDays", *row.fixing_days);
    if (row.equity_currency || row.fx_index) {
        auto terms = payload.child("FXTerms");
        if (row.equity_currency)
            terms.set_text("EquityCurrency", *row.equity_currency);
        if (row.fx_index)
            terms.set_text("FXIndex", *row.fx_index);
    }
}

inline void write_cms_terms(record_writer cms,
                            const swap_leg& row,
                            const cms_payload& payload,
                            const std::vector<swap_leg_rate>& rates) {
    cms.set_text(payload.index, row.floating_index_code);
    if (!payload.index_2.empty())
        cms.set_text(payload.index_2, row.floating_index_code_2.value_or(""));
    if (payload.has_duration && row.cms_duration)
        cms.set_integer("Duration", *row.cms_duration);
    if (row.is_in_arrears)
        cms.set_text("IsInArrears", flag_text(*row.is_in_arrears));
    if (row.fixing_days)
        cms.set_integer("FixingDays", *row.fixing_days);
    write_floating_rates(cms, rates);
    if (row.naked_option)
        cms.set_text("NakedOption", flag_text(*row.naked_option));
}

inline std::vector<swap_leg_rate> read_leg_rates(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_rate> rates;
    const auto append_value = [&](std::string_view role, const decimal& value) {
        swap_leg_rate row;
        row.leg_number = leg_number;
        row.rate_role = std::string(role);
        row.sequence_number = static_cast<int>(rates.size()) + 1;
        row.value = value;
        stamp_row(row);
        rates.push_back(std::move(row));
    };
    const auto append = [&](std::string_view role,
                            const record_reader& value,
                            std::string_view date_attribute = "startDate") {
        swap_leg_rate row;
        row.leg_number = leg_number;
        row.rate_role = std::string(role);
        row.sequence_number = static_cast<int>(rates.size()) + 1;
        row.value = decimal_text(value);
        if (const auto start = text_of(value, date_attribute); !start.empty())
            row.start_date = to_domain_date(start);
        stamp_row(row);
        rates.push_back(std::move(row));
    };

    if (const auto fixed = child_if(leg, "FixedLegData")) {
        if (const auto list = fixed->child("Rates"))
            for (const auto& rate : list->children("Rate"))
                append("fixed", rate);
    }
    if (const auto floating = child_if(leg, "FloatingLegData")) {
        for (const auto& [element, item, role] : floating_rate_lists)
            if (const auto list = floating->child(element))
                for (const auto& value : list->children(item))
                    append(role, value);
        if (const auto fixings = floating->child("HistoricalFixings"))
            for (const auto& fixing : fixings->children("Fixing"))
                append("historical_fixing", fixing, "fixingDate");
    }
    for (const auto* element : {"CPILegData", "ZeroCouponFixedLegData"})
        if (const auto payload = child_if(leg, element))
            for (const auto& list : payload->children("Rates"))
                for (const auto& rate : list.children("Rate"))
                    append("fixed", rate);
    if (const auto yy = child_if(leg, "YYLegData"))
        for (const auto& [element, item, role] : floating_rate_lists)
            if (const auto list = yy->child(element))
                for (const auto& value : list->children(item))
                    append(role, value);
    for (const auto& payload : cms_payloads)
        if (const auto cms = child_if(leg, payload.element))
            for (const auto& [element, item, role] : floating_rate_lists)
                if (const auto list = cms->child(element))
                    for (const auto& value : list->children(item))
                        append(role, value);
    for (const auto& digital : digital_payloads) {
        const auto payload = child_if(leg, digital.element);
        if (!payload)
            continue;
        if (const auto cms = single_child(*payload, cms_payloads[digital.cms].element))
            for (const auto& [element, item, role] : floating_rate_lists)
                if (const auto list = cms->child(element))
                    for (const auto& value : list->children(item))
                        append(role, value);
        for (const auto& [element, item, role] : digital_rate_lists)
            if (const auto list = single_child(*payload, element))
                for (const auto& value : list->decimals(item))
                    append_value(role, value);
    }
    if (const auto margin = child_if(leg, "EquityMarginLegData"))
        if (const auto list = margin->child("Rates"))
            for (const auto& rate : list->children("Rate"))
                append("fixed", rate);
    if (const auto fixed = child_if(leg, "CommodityFixedLegData")) {
        if (const auto list = fixed->child("Prices"))
            for (const auto& price : list->children("Price"))
                append("price", price);
    }
    if (const auto floating = child_if(leg, "CommodityFloatingLegData")) {
        for (const auto& [element, item, role] : commodity_floating_rate_lists)
            if (const auto list = floating->child(element))
                for (const auto& value : list->children(item))
                    append(role, value);
    }
    return rates;
}

/// A leg's explicit payment dates, kept as a dates schedule under the leg.
template <typename Data>
void read_payment_dates(Data& result, const record_reader& leg, int leg_number) {
    if (const auto group = child_if(leg, "PaymentDates"))
        option_block::read_date_list(
            result, *group, "PaymentDate", "swap", leg_number, "payment_dates");
}

template <typename Data>
void append_leg(Data& result, const record_reader& leg, int leg_number) {
    result.legs.push_back(read_leg(leg, leg_number));
    for (auto& amount : read_leg_amounts(leg, leg_number))
        result.leg_amounts.push_back(std::move(amount));
    for (auto& rate : read_leg_rates(leg, leg_number))
        result.leg_rates.push_back(std::move(rate));
    for (auto& step : read_leg_amortizations(leg, leg_number))
        result.leg_amortizations.push_back(std::move(step));
    if (const auto schedule = child_if(leg, "ScheduleData"))
        option_block::read_schedule(result, *schedule, "swap", leg_number, "schedule");
    read_payment_dates(result, leg, leg_number);
    read_leg_indexings(result, leg, leg_number);
}

template <typename Data>
void append_legs(Data& result, const record_reader& data) {
    int leg_number = 1;
    for (const auto& leg : data.children("LegData"))
        append_leg(result, leg, leg_number++);
}

inline void write_schedule_rules(record_writer& schedule,
                                 const std::optional<std::chrono::year_month_day>& start_date,
                                 const std::optional<std::chrono::year_month_day>& end_date,
                                 const std::string& tenor) {
    auto rule = schedule.append("Rules");
    rule.set_text("StartDate", to_ore_date(start_date));
    if (end_date)
        rule.set_text("EndDate", to_ore_date(*end_date));
    rule.set_text("Tenor", tenor);
    rule.set_spelling("Convention", "MF", "business day convention");
}

inline bool has_role_in(const std::vector<swap_leg_rate>& rates, std::string_view role) {
    return std::any_of(rates.begin(), rates.end(), [&](const swap_leg_rate& rate) {
        return rate.rate_role == role;
    });
}

template <typename Data>
std::vector<swap_leg_amount> amounts_for_leg(const Data& v, int leg_number) {
    std::vector<swap_leg_amount> out;
    for (const auto& row : v.leg_amounts)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

template <typename Data>
std::vector<swap_leg_rate> rates_for_leg(const Data& v, int leg_number) {
    std::vector<swap_leg_rate> out;
    for (const auto& row : v.leg_rates)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

/**
 * @brief Writes one leg from its rows.
 *
 * The start and maturity are the instrument's own dates: a leg stored before
 * its schedule rows existed falls back to them.
 */
template <typename Data>
void write_leg(record_writer& leg,
               const std::optional<std::chrono::year_month_day>& start_date,
               const std::optional<std::chrono::year_month_day>& maturity_date,
               const swap_leg& row,
               const Data& v) {
    const auto leg_number = row.identity.leg_number;
    const auto amounts = amounts_for_leg(v, leg_number);
    const auto rates = rates_for_leg(v, leg_number);

    leg.set_spelling("LegType", row.leg_type_code, "leg type");
    leg.set_text("Payer", flag_text(row.payer.value_or(false)));
    set_scalar(leg, "Currency", row.currency);
    if (!row.day_count_fraction_code.empty())
        set_spelling_or(leg, "DayCounter", row.day_count_fraction_code, "A360", "day counter");
    if (!row.business_day_convention_code.empty())
        set_spelling_or(leg,
                        "PaymentConvention",
                        row.business_day_convention_code,
                        "MF",
                        "business day convention");
    if (row.payment_lag)
        leg.set_text("PaymentLag", *row.payment_lag);
    if (row.payment_calendar)
        leg.set_text("PaymentCalendar", *row.payment_calendar);

    // A cashflow leg has dated payments and no notional or schedule.
    if (row.leg_type_code == "Cashflow") {
        auto cashflow = leg.child("CashflowData").child("Cashflow");
        for (const auto& amount : amounts) {
            auto writer = cashflow.append("Amount");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("date", to_ore_date(*amount.start_date));
        }
        return;
    }

    const bool commodity =
        row.leg_type_code == "CommodityFixed" || row.leg_type_code == "CommodityFloating";
    if (!amounts.empty() && !commodity) {
        auto notionals = leg.child("Notionals");
        for (const auto& amount : amounts) {
            auto writer = notionals.append("Notional");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("startDate", to_ore_date(*amount.start_date));
        }
    }
    if (row.notional_initial_exchange || row.notional_final_exchange) {
        auto exchanges = leg.child("Notionals").append("Exchanges");
        if (row.notional_initial_exchange)
            exchanges.set_spelling(
                "NotionalInitialExchange", flag_text(*row.notional_initial_exchange), "boolean");
        if (row.notional_final_exchange)
            exchanges.set_spelling(
                "NotionalFinalExchange", flag_text(*row.notional_final_exchange), "boolean");
    }
    if (row.fx_reset_foreign_currency || row.fx_reset_fx_index) {
        auto reset = leg.child("Notionals").append("FXReset");
        if (row.fx_reset_foreign_currency)
            reset.set_spelling("ForeignCurrency", *row.fx_reset_foreign_currency, "currency");
        if (row.fx_reset_start_date)
            reset.set_text("StartDate", *row.fx_reset_start_date);
        if (row.fx_reset_foreign_amount)
            reset.set_decimal("ForeignAmount", *row.fx_reset_foreign_amount);
        if (row.fx_reset_fx_index)
            reset.set_text("FXIndex", *row.fx_reset_fx_index);
        if (row.fx_reset_fixing_days)
            reset.set_integer("FixingDays", *row.fx_reset_fixing_days);
        if (row.fx_reset_fixing_calendar)
            reset.set_text("FixingCalendar", *row.fx_reset_fixing_calendar);
    }

    std::optional<record_writer> amortizations;
    for (const auto& step : v.leg_amortizations) {
        if (step.leg_number != leg_number)
            continue;
        if (!amortizations)
            amortizations = leg.child("Amortizations");
        auto writer = amortizations->append("AmortizationData");
        set_spelling_or(writer, "Type", step.amortization_type, "FixedAmount", "amortization type");
        if (step.value)
            writer.set_decimal("Value", *step.value);
        if (step.start_date)
            writer.set_text("StartDate", to_ore_date(step.start_date));
        if (step.end_date)
            writer.set_text("EndDate", to_ore_date(step.end_date));
        if (step.frequency)
            writer.set_text("Frequency", *step.frequency);
        if (step.underflow)
            writer.set_text("Underflow", flag_text(*step.underflow));
    }

    write_leg_indexings(leg, row, v);

    auto schedule = leg.child("ScheduleData");
    if (!option_block::write_stored_schedule(schedule, v, "swap", leg_number, "schedule"))
        write_schedule_rules(schedule,
                             start_date,
                             maturity_date,
                             payment_frequency_to_tenor(row.payment_frequency_code));

    option_block::write_date_list(
        leg, "PaymentDates", "PaymentDate", v, "swap", leg_number, "payment_dates");

    const auto write_amounts = [&](record_writer payload) {
        if (amounts.empty())
            return;
        auto list = payload.child("Quantities");
        for (const auto& amount : amounts) {
            auto writer = list.append("Quantity");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("startDate", to_ore_date(*amount.start_date));
        }
    };
    if (row.leg_type_code == "CommodityFixed") {
        auto payload = leg.child("CommodityFixedLegData");
        write_amounts(payload);
        write_rate_lists(payload, rates, commodity_fixed_rate_lists);
        scalar_terms::write_terms(payload, row, commodity_fixed_leg_terms);
        return;
    }
    if (row.leg_type_code == "CommodityFloating") {
        auto payload = leg.child("CommodityFloatingLegData");
        payload.set_text("Name", row.floating_index_code);
        scalar_terms::write_terms(payload, row, commodity_floating_leg_terms);
        if (row.commodity_price_type)
            set_spelling_or(
                payload, "PriceType", *row.commodity_price_type, "FutureSettlement", "price type");
        write_amounts(payload);
        write_rate_lists(payload, rates, commodity_floating_rate_lists);
        if (row.pricing_calendar)
            payload.set_text("PricingCalendar", *row.pricing_calendar);
        if (row.is_averaged)
            payload.set_text("IsAveraged", flag_text(*row.is_averaged));
        if (row.is_in_arrears)
            payload.set_text("IsInArrears", flag_text(*row.is_in_arrears));
        if (row.future_month_offset)
            payload.set_integer("FutureMonthOffset", *row.future_month_offset);
        if (row.daily_expiry_offset)
            payload.set_integer("DailyExpiryOffset", *row.daily_expiry_offset);
        return;
    }

    if (row.leg_type_code == "Equity") {
        write_equity_terms(leg.child("EquityLegData"), row);
        return;
    }
    if (row.leg_type_code == "EquityMargin") {
        auto margin = leg.child("EquityMarginLegData");
        if (has_role_in(rates, "fixed")) {
            auto list = margin.child("Rates");
            for (const auto& rate : rates) {
                if (rate.rate_role != "fixed")
                    continue;
                auto writer = list.append("Rate");
                writer.set_own_text(rate.value.to_string());
                if (rate.start_date)
                    writer.set_text("startDate", to_ore_date(*rate.start_date));
            }
        }
        if (row.equity_margin_initial_margin_factor)
            margin.set_decimal("InitialMarginFactor", *row.equity_margin_initial_margin_factor);
        if (row.equity_margin_multiplier)
            margin.set_decimal("Multiplier", *row.equity_margin_multiplier);
        write_equity_terms(margin.child("EquityLegData"), row);
        return;
    }
    if (row.leg_type_code == "FormulaBased") {
        auto formula = leg.child("FormulaBasedLegData");
        formula.set_text("Index", row.floating_index_code);
        if (row.is_in_arrears)
            formula.set_text("IsInArrears", flag_text(*row.is_in_arrears));
        if (row.fixing_days)
            formula.set_integer("FixingDays", *row.fixing_days);
        if (row.fixing_calendar)
            formula.set_text("FixingCalendar", *row.fixing_calendar);
        return;
    }

    const auto write_fixed_rates = [&](record_writer& payload) {
        if (!has_role_in(rates, "fixed"))
            return;
        auto list = payload.append("Rates");
        for (const auto& rate : rates)
            if (rate.rate_role == "fixed")
                list.append("Rate").set_own_text(rate.value.to_string());
    };
    if (row.leg_type_code == "CPI") {
        auto cpi = leg.child("CPILegData");
        cpi.append_value("Index", row.floating_index_code);
        write_fixed_rates(cpi);
        if (row.cpi_base)
            cpi.append_decimal("BaseCPI", decimal_from_double(*row.cpi_base));
        if (row.cpi_start_date)
            cpi.append_value("StartDate", to_ore_date(*row.cpi_start_date));
        if (row.observation_lag)
            cpi.append_value("ObservationLag", *row.observation_lag);
        if (row.cpi_interpolation)
            cpi.append_value("Interpolation", *row.cpi_interpolation);
        if (row.cpi_interpolated)
            cpi.append_spelling("Interpolated", flag_text(*row.cpi_interpolated), "boolean");
        if (row.subtract_inflation_notional)
            cpi.append_spelling("SubtractInflationNotional",
                                flag_text(*row.subtract_inflation_notional),
                                "boolean");
        return;
    }
    if (row.leg_type_code == "YY") {
        auto yy = leg.child("YYLegData");
        yy.set_text("Index", row.floating_index_code);
        if (row.fixing_days)
            yy.set_integer("FixingDays", *row.fixing_days);
        if (row.observation_lag)
            yy.set_text("ObservationLag", *row.observation_lag);
        write_floating_rates(yy, rates);
        if (row.naked_option)
            yy.set_text("NakedOption", flag_text(*row.naked_option));
        return;
    }
    if (row.leg_type_code == "ZeroCouponFixed") {
        auto zero = leg.child("ZeroCouponFixedLegData");
        write_fixed_rates(zero);
        if (row.zero_coupon_compounding)
            zero.append_value("Compounding", *row.zero_coupon_compounding);
        if (row.zero_coupon_subtract_notional)
            zero.append_value("SubtractNotional", flag_text(*row.zero_coupon_subtract_notional));
        return;
    }

    for (const auto& payload : cms_payloads) {
        if (row.leg_type_code != payload.leg_type)
            continue;
        write_cms_terms(leg.child(payload.element), row, payload, rates);
        return;
    }
    for (const auto& digital : digital_payloads) {
        if (row.leg_type_code != digital.leg_type)
            continue;
        auto payload = leg.child(digital.element);
        const auto& cms = cms_payloads[digital.cms];
        write_cms_terms(single_child_writer(payload, cms.element), row, cms, rates);
        if (row.digital_call_position)
            set_single_value(payload, "CallPosition", *row.digital_call_position);
        if (row.digital_is_call_atm_included)
            set_single_value(
                payload, "IsCallATMIncluded", flag_text(*row.digital_is_call_atm_included));
        if (row.digital_put_position)
            set_single_value(payload, "PutPosition", *row.digital_put_position);
        if (row.digital_is_put_atm_included)
            set_single_value(
                payload, "IsPutATMIncluded", flag_text(*row.digital_is_put_atm_included));
        for (const auto& [element, item, role] : digital_rate_lists) {
            if (!has_role_in(rates, role))
                continue;
            auto list = single_child_writer(payload, element);
            for (const auto& rate : rates)
                if (rate.rate_role == role)
                    list.append_decimal(item, rate.value);
        }
        return;
    }

    const auto has_role = [&](std::string_view role) {
        return std::any_of(rates.begin(), rates.end(), [&](const swap_leg_rate& rate) {
            return rate.rate_role == role;
        });
    };
    if (row.leg_type_code == "Fixed" && has_role("fixed")) {
        auto list = leg.child("FixedLegData").child("Rates");
        for (const auto& rate : rates)
            if (rate.rate_role == "fixed")
                list.append("Rate").set_own_text(rate.value.to_string());
    } else if (row.leg_type_code == "Floating") {
        auto floating = leg.child("FloatingLegData");
        floating.set_text("Index", row.floating_index_code);
        if (row.fixing_days)
            floating.set_integer("FixingDays", *row.fixing_days);
        const auto write_flag = [&floating](std::string_view name,
                                            const std::optional<bool>& flag) {
            if (flag)
                floating.set_text(name, flag_text(*flag));
        };
        write_flag("IsInArrears", row.is_in_arrears);
        write_flag("IncludeSpread", row.include_spread);
        write_flag("IsAveraged", row.is_averaged);
        write_flag("HasSubPeriods", row.has_sub_periods);
        if (row.lookback)
            floating.set_text("Lookback", *row.lookback);
        if (row.rate_cutoff)
            floating.set_integer("RateCutoff", *row.rate_cutoff);
        write_floating_rates(floating, rates);
        write_historical_fixings(floating, rates);
        write_flag("NakedOption", row.naked_option);
        write_flag("LocalCapFloor", row.local_cap_floor);
    }
}

template <typename Data>
void write_legs(record_writer& data,
                const std::optional<std::chrono::year_month_day>& start_date,
                const std::optional<std::chrono::year_month_day>& maturity_date,
                const Data& v) {
    for (const auto& leg : v.legs) {
        auto writer = data.append("LegData");
        write_leg(writer, start_date, maturity_date, leg, v);
    }
}

}

#endif
