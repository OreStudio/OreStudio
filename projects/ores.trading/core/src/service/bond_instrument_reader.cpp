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
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/service/ascot_service.hpp"
#include "ores.trading.core/service/bond_future_service.hpp"
#include "ores.trading.core/service/bond_instrument_service.hpp"
#include "ores.trading.core/service/bond_issue_service.hpp"
#include "ores.trading.core/service/bond_option_service.hpp"
#include "ores.trading.core/service/bond_repo_service.hpp"
#include "ores.trading.core/service/bond_trs_service.hpp"
#include <boost/uuid/uuid_io.hpp>
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

/**
 * @brief The leg family rows of one instrument, as the six tables hold them.
 *
 * The rows stay in the order their query returned: by instrument, then by
 * the leg's role and number, then by the child's own ordinal. A leg
 * therefore walks its children in document order without sorting again.
 */
struct instrument_rows final {
    std::vector<domain::bond_leg> legs;
    std::vector<domain::bond_leg_amount> amounts;
    std::vector<domain::bond_leg_rate> rates;
    std::vector<domain::bond_leg_amortization> amortizations;
    std::vector<domain::instrument_schedule> schedules;
    std::vector<domain::instrument_schedule_date> schedule_dates;
};

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
read_family_rows(ores::database::context ctx, const std::vector<std::string>& instrument_ids) {
    std::unordered_map<std::string, instrument_rows> rows;

    for (auto& row : repository::read_legs_by_instrument_ids(ctx, instrument_ids))
        rows[boost::uuids::to_string(row.instrument_id)].legs.push_back(std::move(row));

    for (auto& row : repository::read_leg_amounts_by_instrument_ids(ctx, instrument_ids))
        rows[boost::uuids::to_string(row.instrument_id)].amounts.push_back(std::move(row));

    for (auto& row : repository::read_leg_rates_by_instrument_ids(ctx, instrument_ids))
        rows[boost::uuids::to_string(row.instrument_id)].rates.push_back(std::move(row));

    for (auto& row : repository::read_leg_amortizations_by_instrument_ids(ctx, instrument_ids))
        rows[boost::uuids::to_string(row.instrument_id)].amortizations.push_back(std::move(row));

    for (auto& row : repository::read_schedules_by_instrument_ids(ctx, instrument_ids))
        rows[boost::uuids::to_string(row.instrument_id)].schedules.push_back(std::move(row));

    for (auto& row : repository::read_schedule_dates_by_instrument_ids(ctx, instrument_ids))
        rows[boost::uuids::to_string(row.instrument_id)].schedule_dates.push_back(std::move(row));

    return rows;
}

domain::bond_schedule_rules to_rules(const domain::instrument_schedule& row) {
    domain::bond_schedule_rules rules;
    rules.start_date = row.start_date.value_or("");
    rules.end_date = row.end_date;
    rules.adjust_end_date_to_previous_month_end = row.adjust_end_date_to_previous_month_end;
    rules.tenor = row.tenor.value_or("");
    rules.calendar = row.calendar;
    rules.convention = row.convention.value_or("");
    rules.term_convention = row.term_convention;
    rules.rule = row.rule;
    rules.end_of_month = row.end_of_month;
    rules.end_of_month_convention = row.end_of_month_convention;
    rules.first_date = row.first_date;
    rules.last_date = row.last_date;
    rules.remove_first_date = row.remove_first_date;
    rules.remove_last_date = row.remove_last_date;
    return rules;
}

domain::bond_schedule_data
to_schedule_data(const std::vector<const domain::instrument_schedule*>& rows,
                 const std::vector<const domain::instrument_schedule_date*>& date_rows) {
    std::unordered_map<int, std::vector<const domain::instrument_schedule_date*>> by_sequence;
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
                dates.dates.push_back(date_row->schedule_date);
        schedule.dates.push_back(std::move(dates));
    }
    return schedule;
}

/**
 * @brief Reads one of a leg's schedules, which its role names.
 */
domain::bond_schedule_data schedule_for(const instrument_rows& rows,
                                        const domain::bond_leg& leg,
                                        std::string_view role) {
    std::vector<const domain::instrument_schedule*> schedules;
    for (const auto& row : rows.schedules)
        if (row.owner_role == leg.leg_role && row.owner_number == leg.leg_number &&
            row.schedule_role == role)
            schedules.push_back(&row);

    std::vector<const domain::instrument_schedule_date*> dates;
    for (const auto& row : rows.schedule_dates)
        if (row.owner_role == leg.leg_role && row.owner_number == leg.leg_number &&
            row.schedule_role == role)
            dates.push_back(&row);

    return to_schedule_data(schedules, dates);
}

leg_amounts collect_amounts(const instrument_rows& rows, const domain::bond_leg& leg) {
    leg_amounts amounts;
    for (const auto& row : rows.amounts) {
        if (row.leg_role != leg.leg_role || row.leg_number != leg.leg_number)
            continue;
        domain::bond_float_data amount{row.value, row.start_date};
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

domain::bond_leg_rate_data to_rate_data(domain::bond_leg_rate row,
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
        floating.front_stub_interpolation =
            to_interpolation(row.front_stub_short_index,
                             row.front_stub_long_index,
                             row.front_stub_rounding_type,
                             row.front_stub_rounding_precision);
        floating.back_stub_interpolation =
            to_interpolation(row.back_stub_short_index,
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

domain::bond_leg_data build_leg(const instrument_rows& rows, const domain::bond_leg& row) {
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
        leg.settlement = domain::bond_settlement_data{*row.settlement_fx_index,
                                                      row.settlement_fixing_date};

    const auto amounts = collect_amounts(rows, row);
    leg.notionals = amounts.notional;

    for (const auto& amortization : rows.amortizations) {
        if (amortization.leg_role != row.leg_role || amortization.leg_number != row.leg_number)
            continue;
        leg.amortizations.push_back({amortization.amortization_type,
                                     amortization.value,
                                     amortization.start_date,
                                     amortization.end_date,
                                     amortization.frequency,
                                     amortization.underflow});
    }

    leg.schedule = schedule_for(rows, row, "schedule");
    leg.payment_schedule = schedule_for(rows, row, "payment_schedule");
    const auto payment_dates = schedule_for(rows, row, "payment_dates");
    for (const auto& block : payment_dates.dates)
        for (const auto& date : block.dates)
            leg.payment_dates.push_back(date);

    for (const auto& rate_row : rows.rates) {
        if (rate_row.leg_role != row.leg_role || rate_row.leg_number != row.leg_number)
            continue;
        leg.rate = to_rate_data(rate_row,
                                amounts,
                                schedule_for(rows, row, "fixing_schedule"),
                                schedule_for(rows, row, "reset_schedule"));
        break;
    }
    return leg;
}

void apply_leg_family(domain::bond_instrument_data& data, const instrument_rows& rows) {
    for (const auto& row : rows.legs) {
        if (row.leg_role == "bond") {
            data.bond_legs.push_back(build_leg(rows, row));
            continue;
        }
        if (row.leg_role == "trs_funding")
            data.trs_funding_leg = build_leg(rows, row);
        else if (row.leg_role == "repo")
            data.repo_leg = build_leg(rows, row);
        else if (row.leg_role == "ascot_swap")
            data.ascot_swap_leg = build_leg(rows, row);
    }
}

}

bond_instrument_reader::bond_instrument_reader(context ctx)
    : ctx_(std::move(ctx)) {}

std::unordered_map<std::string, domain::bond_instrument_data>
bond_instrument_reader::read_instruments(
    const std::vector<std::string>& instrument_ids) const {
    std::unordered_map<std::string, domain::bond_instrument_data> result;
    if (instrument_ids.empty())
        return result;

    bond_instrument_service instrument_svc(ctx_);
    bond_issue_service issue_svc(ctx_);
    bond_option_service option_svc(ctx_);
    bond_trs_service trs_svc(ctx_);
    bond_repo_service repo_svc(ctx_);
    bond_future_service future_svc(ctx_);
    ascot_service ascot_svc(ctx_);

    auto rows = instrument_svc.get_bond_instruments(instrument_ids);

    std::unordered_map<std::string, domain::bond_issue> issue_cache;
    std::vector<std::string> issue_ids;
    for (const auto& row : rows) {
        const auto issue_id = boost::uuids::to_string(row.issue_id);
        if (issue_cache.contains(issue_id))
            continue;
        issue_ids.push_back(issue_id);
        if (auto issue = issue_svc.get_issue(issue_id))
            issue_cache[issue_id] = *issue;
    }

    std::unordered_map<std::string, std::vector<domain::bond_issue_call_date>> call_dates;
    for (auto& row : repository::read_call_dates_by_issue_ids(ctx_, issue_ids))
        call_dates[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    std::unordered_map<std::string, std::vector<domain::bond_issue_conversion_target>>
        conversion_targets;
    for (auto& row : repository::read_conversion_targets_by_issue_ids(ctx_, issue_ids))
        conversion_targets[boost::uuids::to_string(row.issue_id)].push_back(std::move(row));

    const auto leg_family = read_family_rows(ctx_, instrument_ids);

    for (auto& row : rows) {
        const auto id = boost::uuids::to_string(row.identity.instrument_id);
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
        if (auto it = leg_family.find(id); it != leg_family.end())
            apply_leg_family(data, it->second);

        const auto& ttc = data.instrument.identity.trade_type_code;
        if (ttc == "BondOption")
            data.option = option_svc.get_option(id);
        else if (ttc == "BondTRS")
            data.trs = trs_svc.get_trs(id);
        else if (ttc == "BondRepo")
            data.repo = repo_svc.get_repo(id);
        else if (ttc == "BondFuture")
            data.future = future_svc.get_future(id);
        else if (ttc == "Ascot")
            data.ascot = ascot_svc.get_ascot(id);

        result.emplace(id, std::move(data));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " bond instruments.";
    return result;
}

}
