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
#include "ores.trading.core/service/bond_instrument_reader.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/service/ascot_service.hpp"
#include "ores.trading.core/service/bond_future_service.hpp"
#include "ores.trading.core/service/bond_instrument_service.hpp"
#include "ores.trading.core/service/bond_issue_service.hpp"
#include "ores.trading.api/domain/bond_issue_leg.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amortization.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_date.hpp"
#include "ores.trading.core/service/bond_option_service.hpp"
#include "ores.trading.core/service/bond_repo_service.hpp"
#include "ores.trading.core/service/bond_trs_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::trading::service {

using namespace ores::logging;

namespace {

// The bond schedule aggregates the reader builds are hand-written string
// carriers; the repository entities now hold calendar dates. Convert at this
// boundary, one spelling each way.
std::string iso_or_empty(const std::chrono::year_month_day& d) {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

std::string iso_or_empty(const std::optional<std::chrono::year_month_day>& d) {
    return d && d->ok() ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

std::optional<std::string> iso_optional(const std::optional<std::chrono::year_month_day>& d) {
    if (!d || !d->ok())
        return std::nullopt;
    return ores::platform::time::datetime::to_iso8601_date(*d);
}

/**
 * @brief The instrument's own rows, as the tables hold them.
 *
 * The rows stay in the order their query returned: by instrument, then by
 * the owner's role and number, then by the child's own ordinal. A reader
 * therefore walks a container's children in document order without
 * sorting again.
 */
struct instrument_rows final {
    std::vector<domain::bond_leg> legs;
    std::vector<domain::bond_leg_amount> amounts;
    std::vector<domain::bond_leg_rate> rates;
    std::vector<domain::bond_leg_amortization> amortizations;
    std::vector<domain::instrument_schedule> schedules;
    std::vector<domain::instrument_schedule_date> schedule_dates;
    std::optional<domain::instrument_option> option;
    std::vector<domain::instrument_option_premium> option_premiums;
    std::vector<domain::instrument_option_exercise_fee> option_exercise_fees;
    std::vector<domain::instrument_option_payment_date> option_payment_dates;
    std::optional<domain::instrument_strike> strike;
    std::optional<domain::bond_forward> forward;
    std::vector<domain::bond_future_delivery_basket> delivery_basket;
};

/**
 * @brief The rows a bond security owns, as the issue-keyed tables hold them.
 *
 * A bond's own legs belong to the security rather than to the trade, so
 * they and everything they state key on the issue. The option block, the
 * strike, the forward and the delivery basket are the trade's product
 * and stay in instrument_rows.
 */
struct issue_leg_rows final {
    std::vector<domain::bond_issue_leg> legs;
    std::vector<domain::bond_issue_leg_amount> amounts;
    std::vector<domain::bond_issue_leg_rate> rates;
    std::vector<domain::bond_issue_leg_amortization> amortizations;
    std::vector<domain::bond_issue_leg_schedule> schedules;
    std::vector<domain::bond_issue_leg_schedule_date> schedule_dates;
};

/**
 * @brief The leg role a leg or one of its children belongs to.
 *
 * The trade-keyed family states the role on every row, because one table
 * holds three lists. The issue-keyed family's tables already say whose
 * rows these are, so a row there carries no role and the answer is the
 * bond.
 */
std::string_view leg_role_of(const domain::bond_leg& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const domain::bond_issue_leg&) {
    return "bond";
}

std::string_view leg_role_of(const domain::bond_leg_amount& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const domain::bond_issue_leg_amount&) {
    return "bond";
}

std::string_view leg_role_of(const domain::bond_leg_rate& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const domain::bond_issue_leg_rate&) {
    return "bond";
}

std::string_view leg_role_of(const domain::bond_leg_amortization& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const domain::bond_issue_leg_amortization&) {
    return "bond";
}

/**
 * @brief Whether a schedule row belongs to the owner a role and number name.
 *
 * The trade-keyed schedule states its owner. The issue-keyed one is owned
 * by the leg it names, so the number alone decides.
 */
bool owner_matches(const domain::instrument_schedule& row,
                   std::string_view owner_role,
                   int owner_number) {
    return row.owner_role == owner_role && row.owner_number == owner_number;
}

bool owner_matches(const domain::instrument_schedule_date& row,
                   std::string_view owner_role,
                   int owner_number) {
    return row.owner_role == owner_role && row.owner_number == owner_number;
}

bool owner_matches(const domain::bond_issue_leg_schedule& row, std::string_view, int leg_number) {
    return row.leg_number == leg_number;
}

bool owner_matches(const domain::bond_issue_leg_schedule_date& row,
                   std::string_view,
                   int leg_number) {
    return row.leg_number == leg_number;
}

/**
 * @brief The six amounts a leg can state, split by the role each plays.
 */
struct leg_amounts final {
    std::vector<domain::bond_float_data> notional;
    std::vector<domain::bond_float_data> rate;
    std::vector<domain::bond_float_data> spread;
    std::vector<domain::bond_float_data> cap;
    std::vector<domain::bond_float_data> floor;
    std::vector<domain::bond_float_data> gearing;
};

std::unordered_map<std::string, instrument_rows>
read_family_rows(ores::database::context ctx, const std::vector<std::string>& trade_ids) {
    std::unordered_map<std::string, instrument_rows> rows;

    for (auto& row : repository::read_legs_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].legs.push_back(std::move(row));

    for (auto& row : repository::read_leg_amounts_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].amounts.push_back(std::move(row));

    for (auto& row : repository::read_leg_rates_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].rates.push_back(std::move(row));

    for (auto& row : repository::read_leg_amortizations_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].amortizations.push_back(std::move(row));

    for (auto& row : repository::read_schedules_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].schedules.push_back(std::move(row));

    for (auto& row : repository::read_schedule_dates_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].schedule_dates.push_back(std::move(row));

    for (auto& row : repository::read_options_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option = std::move(row);

    for (auto& row : repository::read_option_premiums_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option_premiums.push_back(std::move(row));

    for (auto& row : repository::read_option_exercise_fees_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option_exercise_fees.push_back(std::move(row));

    for (auto& row : repository::read_option_payment_dates_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].option_payment_dates.push_back(std::move(row));

    for (auto& row : repository::read_strikes_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].strike = std::move(row);

    for (auto& row : repository::read_forwards_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].forward = std::move(row);

    for (auto& row : repository::read_delivery_baskets_by_trade_ids(ctx, trade_ids))
        rows[boost::uuids::to_string(row.trade_id)].delivery_basket.push_back(std::move(row));

    return rows;
}

/**
 * @brief The security-owned rows of a set of issues, keyed by issue.
 *
 * Every instrument of one ISIN shares these rows, so the caller copies
 * them into each instrument's container rather than moving them.
 */
std::unordered_map<std::string, issue_leg_rows>
read_issue_family_rows(ores::database::context ctx, const std::vector<std::string>& issue_ids) {
    std::unordered_map<std::string, issue_leg_rows> rows;
    if (issue_ids.empty())
        return rows;

    for (auto& row : repository::read_issue_legs_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].legs.push_back(std::move(row));

    for (auto& row : repository::read_issue_leg_amounts_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].amounts.push_back(std::move(row));

    for (auto& row : repository::read_issue_leg_rates_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].rates.push_back(std::move(row));

    for (auto& row : repository::read_issue_leg_amortizations_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].amortizations.push_back(std::move(row));

    for (auto& row : repository::read_issue_schedules_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].schedules.push_back(std::move(row));

    for (auto& row : repository::read_issue_schedule_dates_by_issue_ids(ctx, issue_ids))
        rows[boost::uuids::to_string(row.issue_id)].schedule_dates.push_back(std::move(row));

    return rows;
}

template <typename ScheduleRow>
domain::bond_schedule_rules to_rules(const ScheduleRow& row) {
    domain::bond_schedule_rules rules;
    rules.start_date = iso_or_empty(row.start_date);
    rules.end_date = iso_optional(row.end_date);
    rules.adjust_end_date_to_previous_month_end = row.adjust_end_date_to_previous_month_end;
    rules.tenor = row.tenor.value_or("");
    rules.calendar = row.calendar;
    rules.convention = row.convention.value_or("");
    rules.term_convention = row.term_convention;
    rules.rule = row.rule;
    rules.end_of_month = row.end_of_month;
    rules.end_of_month_convention = row.end_of_month_convention;
    rules.first_date = iso_optional(row.first_date);
    rules.last_date = iso_optional(row.last_date);
    rules.remove_first_date = row.remove_first_date;
    rules.remove_last_date = row.remove_last_date;
    return rules;
}

template <typename ScheduleRow, typename DateRow>
domain::bond_schedule_data
to_schedule_data(const std::vector<const ScheduleRow*>& rows,
                 const std::vector<const DateRow*>& date_rows) {
    std::unordered_map<int, std::vector<const DateRow*>> by_sequence;
    for (const auto* date_row : date_rows)
        by_sequence[date_row->schedule_sequence_number].push_back(date_row);

    domain::bond_schedule_data schedule;
    for (const auto* row : rows) {
        if (row->schedule_kind == "rules") {
            schedule.rules.push_back(to_rules(*row));
            continue;
        }
        domain::bond_schedule_dates dates;
        dates.calendar = row->calendar;
        dates.convention = row->convention;
        dates.tenor = row->tenor;
        dates.end_of_month = row->end_of_month;
        dates.include_duplicate_dates = row->include_duplicate_dates;
        if (auto it = by_sequence.find(row->sequence_number); it != by_sequence.end())
            for (const auto* date_row : it->second)
                dates.dates.push_back(iso_or_empty(date_row->schedule_date));
        schedule.dates.push_back(std::move(dates));
    }
    return schedule;
}

/**
 * @brief Reads one of an owner's schedules, which its role and number name.
 */
template <typename ScheduleRow, typename DateRow>
domain::bond_schedule_data schedule_for(const std::vector<ScheduleRow>& schedules,
                                        const std::vector<DateRow>& schedule_dates,
                                        std::string_view owner_role,
                                        int owner_number,
                                        std::string_view role) {
    std::vector<const ScheduleRow*> rows;
    for (const auto& row : schedules)
        if (row.schedule_role == role && owner_matches(row, owner_role, owner_number))
            rows.push_back(&row);

    std::vector<const DateRow*> dates;
    for (const auto& row : schedule_dates)
        if (row.schedule_role == role && owner_matches(row, owner_role, owner_number))
            dates.push_back(&row);

    return to_schedule_data(rows, dates);
}

/**
 * @brief Reads one of a trade-keyed owner's schedules.
 */
domain::bond_schedule_data schedule_for(const instrument_rows& rows,
                                        std::string_view owner_role,
                                        int owner_number,
                                        std::string_view role) {
    return schedule_for(rows.schedules, rows.schedule_dates, owner_role, owner_number, role);
}

template <typename AmountRow>
leg_amounts collect_amounts(const std::vector<AmountRow>& rows,
                            std::string_view leg_role,
                            int leg_number) {
    leg_amounts amounts;
    for (const auto& row : rows) {
        if (leg_role_of(row) != leg_role || row.leg_number != leg_number)
            continue;
        // The stored amount is a decimal and the ORE-shaped value the reader
        // rebuilds is the float the ORE document holds, so the conversion
        // happens once, here, at that boundary.
        domain::bond_float_data amount{row.value.to_double(), row.start_date};
        if (row.amount_role == "notional")
            amounts.notional.push_back(std::move(amount));
        else if (row.amount_role == "rate")
            amounts.rate.push_back(std::move(amount));
        else if (row.amount_role == "spread")
            amounts.spread.push_back(std::move(amount));
        else if (row.amount_role == "cap")
            amounts.cap.push_back(std::move(amount));
        else if (row.amount_role == "floor")
            amounts.floor.push_back(std::move(amount));
        else if (row.amount_role == "gearing")
            amounts.gearing.push_back(std::move(amount));
    }
    return amounts;
}

std::optional<domain::bond_stub_interpolation>
to_interpolation(const std::optional<std::string>& short_index,
                 const std::optional<std::string>& long_index,
                 const std::optional<std::string>& rounding_type,
                 const std::optional<std::int64_t>& rounding_precision) {
    if (!short_index && !long_index && !rounding_type && !rounding_precision)
        return std::nullopt;
    domain::bond_stub_interpolation interpolation;
    interpolation.short_index = short_index.value_or("");
    interpolation.long_index = long_index.value_or("");
    interpolation.rounding_type = rounding_type;
    interpolation.rounding_precision = rounding_precision;
    return interpolation;
}

template <typename RateRow>
domain::bond_leg_rate_data to_rate_data(RateRow row,
                                        const leg_amounts& amounts,
                                        domain::bond_schedule_data fixing_schedule,
                                        domain::bond_schedule_data reset_schedule) {
    domain::bond_leg_rate_data rate;
    if (row.rate_kind == "floating") {
        domain::bond_floating_leg_data floating;
        floating.index = row.index.value_or("");
        floating.is_in_arrears = row.is_in_arrears;
        floating.last_recent_period = row.last_recent_period;
        floating.last_recent_period_calendar = row.last_recent_period_calendar;
        // The column is signed and the container is not, so a negative
        // value would wrap rather than fail.
        if (row.fixing_days && *row.fixing_days >= 0)
            floating.fixing_days = static_cast<std::uint64_t>(*row.fixing_days);
        floating.lookback = row.lookback;
        floating.rate_cutoff = row.rate_cutoff;
        floating.is_averaged = row.is_averaged;
        floating.has_sub_periods = row.has_sub_periods;
        floating.include_spread = row.include_spread;
        floating.is_not_resetting_xccy = row.is_not_resetting_xccy;
        floating.spreads = amounts.spread;
        floating.caps = amounts.cap;
        floating.floors = amounts.floor;
        floating.gearings = amounts.gearing;
        floating.naked_option = row.naked_option;
        floating.local_cap_floor = row.local_cap_floor;
        floating.fixing_schedule = std::move(fixing_schedule);
        floating.reset_schedule = std::move(reset_schedule);
        floating.front_stub_interpolation = to_interpolation(row.front_stub_short_index,
                                                             row.front_stub_long_index,
                                                             row.front_stub_rounding_type,
                                                             row.front_stub_rounding_precision);
        floating.back_stub_interpolation = to_interpolation(row.back_stub_short_index,
                                                            row.back_stub_long_index,
                                                            row.back_stub_rounding_type,
                                                            row.back_stub_rounding_precision);
        floating.stub_use_original_curve = row.stub_use_original_curve;
        floating.observation_shift = row.observation_shift;
        rate.floating = std::move(floating);
        return rate;
    }
    if (row.rate_kind == "formula_based") {
        domain::bond_formula_based_leg_data formula;
        formula.index = row.index.value_or("");
        formula.is_in_arrears = row.is_in_arrears;
        formula.fixing_days = row.fixing_days.value_or(0);
        formula.fixing_calendar = row.fixing_calendar;
        rate.formula_based = std::move(formula);
        return rate;
    }
    rate.fixed = domain::bond_fixed_leg_data{amounts.rate};
    return rate;
}

/**
 * @brief Builds one leg from the rows that hold it and everything it states.
 *
 * The leg row may come from either leg family. Its children are read
 * from the same family, which is why the row set is a template parameter:
 * the trade-keyed family and the issue-keyed one have the same shape and
 * the shared body serves both.
 */
template <typename Rows, typename LegRow>
domain::bond_leg_data build_leg(const Rows& rows, const LegRow& row) {
    const auto leg_role = leg_role_of(row);
    const auto leg_number = row.leg_number;

    domain::bond_leg_data leg;
    leg.payer = row.payer;
    leg.leg_type = row.leg_type;
    leg.currency = row.currency;
    leg.payment_convention = row.payment_convention;
    leg.payment_lag = row.payment_lag;
    leg.payment_calendar = row.payment_calendar;
    leg.day_counter = row.day_counter;
    leg.last_period_day_counter = row.last_period_day_counter;
    leg.notional_payment_lag = row.notional_payment_lag;
    leg.strict_notional_dates = row.strict_notional_dates;
    leg.indexings_from_asset_leg = row.indexings_from_asset_leg;
    if (row.settlement_fx_index)
        leg.settlement =
            domain::bond_settlement_data{*row.settlement_fx_index, row.settlement_fixing_date};

    const auto amounts = collect_amounts(rows.amounts, leg_role, leg_number);
    leg.notionals = amounts.notional;

    for (const auto& amortization : rows.amortizations) {
        if (leg_role_of(amortization) != leg_role || amortization.leg_number != leg_number)
            continue;
        // The stored amount is a decimal and the ORE-shaped block the reader
        // rebuilds holds the float the ORE document carries, so the value
        // crosses at that boundary.
        leg.amortizations.push_back(
            {amortization.amortization_type,
             amortization.value ? std::optional(amortization.value->to_double()) : std::nullopt,
             amortization.start_date,
             amortization.end_date,
             amortization.frequency,
             amortization.underflow});
    }

    leg.schedule = schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "schedule");
    leg.payment_schedule =
        schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "payment_schedule");
    const auto payment_dates =
        schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "payment_dates");
    for (const auto& block : payment_dates.dates)
        for (const auto& date : block.dates)
            leg.payment_dates.push_back(date);

    for (const auto& rate_row : rows.rates) {
        if (leg_role_of(rate_row) != leg_role || rate_row.leg_number != leg_number)
            continue;
        leg.rate = to_rate_data(
            rate_row,
            amounts,
            schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "fixing_schedule"),
            schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "reset_schedule"));
        break;
    }
    return leg;
}

void apply_leg_family(domain::bond_instrument_data& data,
                      const instrument_rows& rows,
                      const issue_leg_rows& issue_rows) {
    // The security's legs come first, in the order the issue's leg list
    // holds them; they are shared by every instrument of the ISIN.
    for (const auto& row : issue_rows.legs)
        data.bond_legs.push_back(build_leg(issue_rows, row));

    for (const auto& row : rows.legs) {
        if (row.leg_role == "trs_funding")
            data.trs_funding_leg = build_leg(rows, row);
        else if (row.leg_role == "repo")
            data.repo_leg = build_leg(rows, row);
        else if (row.leg_role == "ascot_swap")
            data.ascot_swap_leg = build_leg(rows, row);
    }
}

std::optional<domain::bond_schedule_data> optional_schedule(domain::bond_schedule_data schedule) {
    if (schedule.rules.empty() && schedule.dates.empty())
        return std::nullopt;
    return schedule;
}

/**
 * @brief Builds an option settlement block, which a flag on the row engages.
 */
std::optional<domain::bond_option_settlement>
to_option_settlement(bool engaged,
                     const std::optional<std::string>& pay_currency,
                     const std::optional<std::string>& fx_index,
                     const std::optional<std::string>& fixing_date) {
    if (!engaged)
        return std::nullopt;
    return domain::bond_option_settlement{
        pay_currency.value_or(""), fx_index.value_or(""), fixing_date};
}

/**
 * @brief Builds the payment rule block, which the row states as three columns.
 *
 * The three are required members of the rule, so a document that stated
 * the rule stated all three. A row with none of them set stated the date
 * list instead, and the two arms exclude each other.
 */
std::optional<domain::bond_option_payment_rules>
to_payment_rules(const domain::instrument_option& row) {
    if (!row.payment_lag && !row.payment_calendar && !row.payment_convention)
        return std::nullopt;
    domain::bond_option_payment_rules rules;
    rules.lag = static_cast<std::uint64_t>(std::max<std::int64_t>(row.payment_lag.value_or(0), 0));
    rules.calendar = row.payment_calendar.value_or("");
    rules.convention = row.payment_convention.value_or("");
    rules.relative_to = row.payment_relative_to;
    return rules;
}

void apply_option_block(domain::bond_instrument_data& data, const instrument_rows& rows) {
    if (rows.option) {
        const auto& row = *rows.option;

        domain::bond_option_data block;
        block.long_short = row.long_short;
        block.option_type = row.option_type;
        block.payoff_type = row.payoff_type;
        block.payoff_type_2 = row.payoff_type_2;
        block.style = row.style;
        block.notice_period = row.notice_period;
        block.notice_calendar = row.notice_calendar;
        block.notice_convention = row.notice_convention;
        block.mid_coupon_exercise = row.mid_coupon_exercise;
        block.settlement = row.settlement;
        block.settlement_method = row.settlement_method;
        block.pay_off_at_expiry = row.pay_off_at_expiry;
        block.premium_amount = row.premium_amount;
        block.premium_currency = row.premium_currency;
        block.premium_pay_date = row.premium_pay_date;

        for (const auto& premium : rows.option_premiums)
            block.premiums.push_back({premium.amount.to_double(),
                                      premium.currency,
                                      iso_or_empty(premium.pay_date),
                                      to_option_settlement(premium.has_settlement,
                                                           premium.settlement_pay_currency,
                                                           premium.settlement_fx_index,
                                                           premium.settlement_fixing_date)});

        block.exercise_prices = row.exercise_prices;
        for (const auto& fee : rows.option_exercise_fees)
            block.exercise_fees.push_back(
                {fee.amount.to_double(), fee.type, fee.start_date, fee.currency});

        block.exercise_fee_settlement_period = row.exercise_fee_settlement_period;
        block.exercise_fee_settlement_calendar = row.exercise_fee_settlement_calendar;
        block.exercise_fee_settlement_convention = row.exercise_fee_settlement_convention;
        block.automatic_exercise = row.automatic_exercise;

        if (row.has_exercise_data)
            block.exercise_data = domain::bond_option_exercise{
                iso_or_empty(row.exercise_date),
                row.exercise_price ? std::optional(row.exercise_price->to_double()) : std::nullopt};

        if (row.has_payment_data) {
            domain::bond_option_payment_data payment;
            for (const auto& date : rows.option_payment_dates)
                payment.dates.push_back(iso_or_empty(date.payment_date));
            payment.rules = to_payment_rules(row);
            block.payment_data = std::move(payment);
        }

        block.settlement_data = to_option_settlement(row.has_settlement_data,
                                                     row.settlement_pay_currency,
                                                     row.settlement_fx_index,
                                                     row.settlement_fixing_date);

        data.option_data = std::move(block);
    }

    const auto exercise_dates = schedule_for(rows, "option", 1, "exercise_dates");
    for (const auto& block : exercise_dates.dates)
        for (const auto& date : block.dates)
            data.option_exercise_dates.push_back(date);

    data.option_exercise_schedule =
        optional_schedule(schedule_for(rows, "option", 1, "exercise_schedule"));
}

void apply_strike(domain::bond_instrument_data& data, const instrument_rows& rows) {
    if (!rows.strike)
        return;
    const auto& row = *rows.strike;
    data.strike_data = domain::bond_strike_data{
        row.price_value ? std::optional(row.price_value->to_double()) : std::nullopt,
        row.price_currency,
        row.yield_value,
        row.yield_compounding,
        row.bare_value ? std::optional(row.bare_value->to_double()) : std::nullopt,
        row.bare_currency};
}

void apply_forward(domain::bond_instrument_data& data, const instrument_rows& rows) {
    if (!rows.forward)
        return;
    const auto& row = *rows.forward;
    data.forward_long_in_forward = row.long_in_forward;

    domain::bond_forward_settlement settlement;
    settlement.forward_maturity_date = row.forward_maturity_date.value_or("");
    settlement.forward_settlement_date = row.forward_settlement_date;
    settlement.settlement = row.settlement;
    settlement.amount = row.amount ? std::optional(row.amount->to_double()) : std::nullopt;
    settlement.lock_rate = row.lock_rate;
    settlement.dv01 = row.dv01 ? std::optional(row.dv01->to_double()) : std::nullopt;
    settlement.lock_rate_day_counter = row.lock_rate_day_counter;
    settlement.settlement_dirty = row.settlement_dirty;
    data.forward_settlement = std::move(settlement);

    if (row.premium_amount || row.premium_date)
        data.forward_premium = domain::bond_forward_premium{row.premium_amount.value_or(""),
                                                            row.premium_date.value_or("")};
}

void apply_delivery_basket(domain::bond_instrument_data& data, const instrument_rows& rows) {
    for (const auto& row : rows.delivery_basket)
        data.future_delivery_basket.push_back(row.delivery_basket_id);
}

/**
 * @brief Copies the return-side members the fact row holds back into the container.
 *
 * The mapper leaves the payer, the price type and the initial price out
 * of the fact row it builds, so a stored row carries them only because
 * the writer put them there. The schedule is not a column and comes from
 * the shared schedule tables under the owner the swap names.
 */
void apply_trs_residue(domain::bond_instrument_data& data, const instrument_rows& rows) {
    if (data.trs) {
        data.trs_payer = data.trs->payer;
        data.trs_initial_price = data.trs->initial_price ?
                                     std::optional(data.trs->initial_price->to_double()) :
                                     std::nullopt;
        if (data.trs->price_type)
            data.trs_price_type = *data.trs->price_type;
    }
    data.trs_schedule = schedule_for(rows, "trs", 1, "schedule");
}

/**
 * @brief Copies the three option members the fact row holds back into the container.
 *
 * The mapper writes the option type and the strike to the fact row and
 * leaves the other three members nowhere, so a stored row carries them
 * only because the writer put them there.
 */
void apply_option_residue(domain::bond_instrument_data& data) {
    if (!data.option)
        return;
    data.option_redemption = data.option->redemption;
    data.option_price_type = data.option->price_type;
    data.option_knocks_out = data.option->knocks_out;
}

}

bond_instrument_reader::bond_instrument_reader(context ctx)
    : ctx_(std::move(ctx)) {}

std::unordered_map<std::string, domain::bond_instrument_data>
bond_instrument_reader::read_instruments(const std::vector<std::string>& trade_ids) const {
    std::unordered_map<std::string, domain::bond_instrument_data> result;
    if (trade_ids.empty())
        return result;

    bond_instrument_service instrument_svc(ctx_);
    bond_issue_service issue_svc(ctx_);
    bond_option_service option_svc(ctx_);
    bond_trs_service trs_svc(ctx_);
    bond_repo_service repo_svc(ctx_);
    bond_future_service future_svc(ctx_);
    ascot_service ascot_svc(ctx_);

    auto rows = instrument_svc.get_bond_instruments(trade_ids);

    std::unordered_map<std::string, domain::bond_issue> issue_cache;
    std::vector<std::string> issue_ids;
    for (const auto& row : rows) {
        const auto issue_id = boost::uuids::to_string(row.issue_id);
        if (issue_cache.contains(issue_id))
            continue;
        issue_ids.push_back(issue_id);
        if (auto issue = issue_svc.get_issue(row.issue_id))
            issue_cache[issue_id] = *issue;
    }

    std::unordered_map<std::string, std::vector<domain::bond_issue_call_date>> call_dates;
    for (auto& row : repository::read_call_dates_by_issue_ids(ctx_, issue_ids))
        call_dates[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    std::unordered_map<std::string, std::vector<domain::bond_issue_conversion_target>>
        conversion_targets;
    for (auto& row : repository::read_conversion_targets_by_issue_ids(ctx_, issue_ids))
        conversion_targets[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    const auto leg_family = read_family_rows(ctx_, trade_ids);
    const auto issue_family = read_issue_family_rows(ctx_, issue_ids);

    // An instrument with no rows of its own still reads, so the two
    // lookups fall back to empty row sets rather than skipping the build.
    static const instrument_rows no_trade_rows;
    static const issue_leg_rows no_issue_rows;

    for (auto& row : rows) {
        const auto id = boost::uuids::to_string(row.identity.trade_id);
        const auto issue_id = boost::uuids::to_string(row.issue_id);
        domain::bond_instrument_data data;
        data.instrument = std::move(row);
        if (auto it = issue_cache.find(issue_id); it != issue_cache.end())
            data.issue = it->second;
        // Copied, not moved: one issue serves every instrument of a
        // security, so a moved list would leave the next one empty.
        if (auto it = call_dates.find(issue_id); it != call_dates.end())
            data.call_dates = it->second;
        if (auto it = conversion_targets.find(issue_id); it != conversion_targets.end())
            data.conversion_targets = it->second;
        const auto family = leg_family.find(id);
        const auto issue_family_row = issue_family.find(issue_id);
        const auto& trade_rows =
            family == leg_family.end() ? no_trade_rows : family->second;
        const auto& security_rows =
            issue_family_row == issue_family.end() ? no_issue_rows : issue_family_row->second;
        apply_leg_family(data, trade_rows, security_rows);
        apply_option_block(data, trade_rows);
        apply_strike(data, trade_rows);
        apply_forward(data, trade_rows);
        apply_delivery_basket(data, trade_rows);

        const auto& ttc = data.instrument.identity.trade_type_code;
        if (ttc == "BondOption")
            data.option = option_svc.get_option(row.identity.trade_id);
        else if (ttc == "BondTRS")
            data.trs = trs_svc.get_trs(row.identity.trade_id);
        else if (ttc == "BondRepo")
            data.repo = repo_svc.get_repo(row.identity.trade_id);
        else if (ttc == "BondFuture")
            data.future = future_svc.get_future(row.identity.trade_id);
        else if (ttc == "Ascot")
            data.ascot_row = ascot_svc.get_ascot(row.identity.trade_id);

        apply_option_residue(data);
        apply_trs_residue(data, trade_rows);

        result.emplace(id, std::move(data));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " bond instruments.";
    return result;
}

}
