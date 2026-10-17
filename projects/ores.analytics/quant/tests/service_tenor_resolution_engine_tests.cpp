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
#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include "ores.analytics.quant/domain/calendar_ruleset.hpp"
#include "ores.analytics.quant/domain/tenor_resolution.hpp"
#include "ores.analytics.quant/service/calendar_rule_engine.hpp"
#include "ores.analytics.quant/service/quantlib_calendar_rulesets.hpp"
#include "ores.analytics.quant/service/tenor_resolution_engine.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <span>
#include <utility>
#include <vector>

namespace {

using ores::analytics::quant::domain::business_day_calendar_set;
using ores::analytics::quant::domain::business_day_convention;
using ores::analytics::quant::domain::calendar_date_row;
using ores::analytics::quant::domain::calendar_ruleset;
using ores::analytics::quant::domain::date_period;
using ores::analytics::quant::domain::period_unit;
using ores::analytics::quant::domain::resolution_algorithm;
using ores::analytics::quant::domain::resolution_status;
using ores::analytics::quant::domain::schedule_date;
using ores::analytics::quant::domain::schedule_kind;
using ores::analytics::quant::domain::tenor_anchor;
using ores::analytics::quant::domain::tenor_kind;
using ores::analytics::quant::domain::tenor_resolution_request;
using ores::analytics::quant::domain::tenor_rule;
using ores::analytics::quant::domain::tenor_spec;
using ores::analytics::quant::service::calendar_rule_engine;
using ores::analytics::quant::service::quantlib_calendar_rulesets;
using ores::analytics::quant::service::tenor_resolution_engine;

using namespace std::chrono;

const std::string tags("[tenor_resolution_engine]");

/// The two calendars every case indexes: a real one with holidays, and one
/// that only knows about weekends.
constexpr std::size_t target_calendar = 0;
constexpr std::size_t weekends_only_calendar = 1;

business_day_calendar_set make_calendars() {
    const std::vector<calendar_ruleset> rulesets = {
        quantlib_calendar_rulesets::target(),
        quantlib_calendar_rulesets::weekends_only(),
    };

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        rulesets, year{2025} / January / 1, year{2028} / December / 31);

    std::vector<calendar_date_row> rows;
    rows.reserve(holidays.size());
    for (const auto& holiday : holidays)
        rows.push_back({holiday.calendar_index, holiday.date});

    std::vector<std::bitset<7>> weekend_masks;
    for (const auto& ruleset : rulesets)
        weekend_masks.push_back(ruleset.weekend_mask);

    return business_day_calendar_set::from_rows(rows, rulesets.size(), std::move(weekend_masks));
}

tenor_spec period_tenor(period_unit unit, int multiplier) {
    return {tenor_kind::period, date_period{unit, multiplier}};
}

tenor_spec special_tenor() {
    return {tenor_kind::special, date_period{}};
}

tenor_resolution_request request_for(tenor_spec tenor,
                                     std::optional<tenor_rule> rule,
                                     year_month_day horizon,
                                     std::size_t calendar_index = target_calendar,
                                     std::optional<year_month_day> spot_date = std::nullopt) {
    return {.tenor = tenor,
            .rule = rule,
            .horizon = horizon,
            .spot_date = spot_date,
            .calendar_index = calendar_index};
}

}

TEST_CASE("a period tenor resolves from its spot anchor by its own unit", tags) {
    // 1M counts one month from the spot date, which is the anchor the
    // convention measures from.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / January / 15,
                    target_calendar,
                    year{2026} / January / 19),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::ok);
    CHECK(results[0].anchor_date == year{2026} / January / 19);
    CHECK(results[0].resolved_date == year{2026} / February / 19);
}

TEST_CASE("the rule's business day convention decides where a weekend date lands", tags) {
    // One month from 31 January 2026 is 28 February, a Saturday. Following
    // takes it into March; ModifiedFollowing will not leave February and
    // takes it back to Friday the 27th instead.
    const auto spot = year{2026} / January / 31;

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / January / 15,
                    target_calendar,
                    spot),
        request_for(
            period_tenor(period_unit::month, 1),
            tenor_rule{.anchor = tenor_anchor::spot, .roll = business_day_convention::following},
            year{2026} / January / 15,
            target_calendar,
            spot),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 2);
    CHECK(results[0].resolved_date == year{2026} / February / 27);
    CHECK(results[1].resolved_date == year{2026} / March / 2);
}

TEST_CASE("a holiday calendar is consulted and a weekends-only one is not", tags) {
    // 1 May 2026 is a Friday and a TARGET holiday. A rule resolving to that
    // day moves to the Monday under TARGET, and stays put on a calendar that
    // only knows about weekends.
    const auto may_day = year{2026} / May / 1;

    // A zero-length period puts the resolved date on the anchor itself, so
    // what the roll convention does with that date is the whole question.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 0),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / April / 1,
                    target_calendar,
                    may_day),
        request_for(period_tenor(period_unit::month, 0),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / April / 1,
                    weekends_only_calendar,
                    may_day),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 2);
    CHECK(results[0].resolved_date == year{2026} / May / 4);
    CHECK(results[1].resolved_date == year{2026} / May / 1);
}

TEST_CASE("a special tenor takes its duration from the rule rather than from itself", tags) {
    // O/N has no period of its own: a one-day offset from the horizon, which
    // a day-counted walk spends as a business day.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(
            special_tenor(),
            tenor_rule{.anchor = tenor_anchor::today, .offset = date_period{period_unit::day, 1}},
            year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::ok);
    CHECK(results[0].anchor_date == year{2026} / January / 15);
    CHECK(results[0].resolved_date == year{2026} / January / 16);
}

TEST_CASE("the same special tenor resolves differently under different rules", tags) {
    // Spot/forward O/N is tomorrow; the near leg of an FX swap is today. The
    // same code, a different date, purely because the rules differ.
    const auto horizon = year{2026} / January / 15;

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(
            special_tenor(),
            tenor_rule{.anchor = tenor_anchor::today, .offset = date_period{period_unit::day, 1}},
            horizon),
        request_for(
            special_tenor(),
            tenor_rule{.anchor = tenor_anchor::today, .offset = date_period{period_unit::day, 0}},
            horizon),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 2);
    CHECK(results[0].resolved_date == year{2026} / January / 16);
    CHECK(results[1].resolved_date == year{2026} / January / 15);
}

TEST_CASE("a tenor with no resolution row under the convention is refused, not thrown", tags) {
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1), std::nullopt, year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::tenor_not_in_convention);
}

TEST_CASE("a period tenor that states no period of its own is refused", tags) {
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::none, 0),
                    tenor_rule{.anchor = tenor_anchor::today},
                    year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::period_tenor_without_period);
}

TEST_CASE("a special tenor whose rule states no offset is refused", tags) {
    // A SPECIAL tenor has no duration of its own, so a rule without an offset
    // leaves it with none at all.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(
            special_tenor(), tenor_rule{.anchor = tenor_anchor::today}, year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::special_tenor_without_offset);
}

TEST_CASE("an anchor whose meaning is unsettled is refused rather than guessed", tags) {
    const auto horizon = year{2026} / January / 15;

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::imm_roll},
                    horizon),
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::near_leg},
                    horizon),
        request_for(
            period_tenor(period_unit::month, 1), tenor_rule{.anchor = tenor_anchor::none}, horizon),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 3);
    for (const auto& result : results)
        CHECK(result.status == resolution_status::unsupported_anchor);
}

TEST_CASE("spot is an input and is never derived from the horizon", tags) {
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::missing_spot_date);
}

TEST_CASE("a calendar the set was not built with is reported, not assumed open", tags) {
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::today},
                    year{2026} / January / 15,
                    9),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::calendar_index_out_of_range);
}

TEST_CASE("the cds quarterly roll walks to the twentieth of a quarter month", tags) {
    // CREDIT_CDS_IMM: a one-year tenor from spot, then one step along the
    // closed-form quarterly roll. 2 January 2026 plus a year is 2 January
    // 2027, whose first roll date is 20 March 2027 -- a Saturday, which
    // ModifiedFollowing takes to the Monday, since the 20th itself is stated
    // without adjustment and moving it is the convention's job.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::year, 1),
                    tenor_rule{.anchor = tenor_anchor::spot,
                               .algorithm = resolution_algorithm::schedule_step,
                               .schedule = schedule_kind::cds_roll_quarter,
                               .schedule_step_count = 1},
                    year{2026} / January / 1,
                    target_calendar,
                    year{2026} / January / 2),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::ok);
    CHECK(results[0].resolved_date == year{2027} / March / 22);
}

TEST_CASE("a cds roll that lands on a business day is left where the schedule put it", tags) {
    // 20 March 2026 is a Friday, so the first roll date from a walk start
    // inside the first quarter is the 20th itself.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(special_tenor(),
                    tenor_rule{.anchor = tenor_anchor::spot,
                               .algorithm = resolution_algorithm::schedule_step,
                               .offset = date_period{period_unit::day, 0},
                               .schedule = schedule_kind::cds_roll_quarter,
                               .schedule_step_count = 1},
                    year{2026} / January / 1,
                    target_calendar,
                    year{2026} / January / 2),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].resolved_date == year{2026} / March / 20);
}

TEST_CASE("a schedule step that names no axis resolves on the calendar axis", tags) {
    // A membership-only row: the tenor belongs to the convention but walks no
    // schedule, so it resolves exactly as ANCHOR_OFFSET resolves it.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step},
                    year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::ok);
    // 15 February 2026 is a Sunday.
    CHECK(results[0].resolved_date == year{2026} / February / 16);
}

TEST_CASE("a schedule step that names exactly one of axis and step count is refused", tags) {
    const auto horizon = year{2026} / January / 15;

    const auto requests = std::vector<tenor_resolution_request>{
        // An axis with no step count.
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .schedule = schedule_kind::cds_roll_quarter},
                    horizon),
        // A step count with no axis.
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .schedule_step_count = 2},
                    horizon),
        // A negative step count, which names no date at all.
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .schedule = schedule_kind::event_lookup,
                               .schedule_step_count = -1},
                    horizon),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 3);
    for (const auto& result : results)
        CHECK(result.status == resolution_status::inconsistent_schedule_declaration);
}

TEST_CASE("an event lookup schedule walks the dates the caller supplies", tags) {
    // A central bank's meeting diary: the row asks for the second meeting on
    // or after the walk start, which is 18 March 2026.
    const auto schedule = std::vector<schedule_date>{
        {0, year{2026} / January / 20},
        {0, year{2026} / March / 18},
        {0, year{2026} / April / 29},
    };

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(special_tenor(),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .offset = date_period{period_unit::day, 0},
                               .schedule = schedule_kind::event_lookup,
                               .schedule_step_count = 2},
                    year{2026} / January / 15),
    };

    const auto results =
        tenor_resolution_engine::resolve_batch(requests, make_calendars(), schedule);

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::ok);
    CHECK(results[0].resolved_date == year{2026} / March / 18);
}

TEST_CASE("a schedule step counts the walk start's own date when the schedule holds it", tags) {
    const auto schedule = std::vector<schedule_date>{
        {0, year{2026} / January / 15},
        {0, year{2026} / February / 11},
    };

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(special_tenor(),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .offset = date_period{period_unit::day, 0},
                               .schedule = schedule_kind::event_lookup,
                               .schedule_step_count = 2},
                    year{2026} / January / 15),
    };

    const auto results =
        tenor_resolution_engine::resolve_batch(requests, make_calendars(), schedule);

    REQUIRE(results.size() == 1);
    CHECK(results[0].resolved_date == year{2026} / February / 11);
}

TEST_CASE("the schedule walk starts from the unadjusted anchor, not from the rolled one", tags) {
    // The schedule holds Saturday 3 January 2026, which only an unadjusted
    // walk start can reach: rolling first would step past it to the 7th.
    const auto schedule = std::vector<schedule_date>{
        {0, year{2026} / January / 3},
        {0, year{2026} / January / 7},
    };

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(special_tenor(),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .offset = date_period{period_unit::day, 0},
                               .schedule = schedule_kind::event_lookup,
                               .schedule_step_count = 1},
                    year{2026} / January / 3),
    };

    const auto results =
        tenor_resolution_engine::resolve_batch(requests, make_calendars(), schedule);

    REQUIRE(results.size() == 1);
    CHECK(results[0].resolved_date == year{2026} / January / 5);
}

TEST_CASE("a schedule with no dates at all is reported as such", tags) {
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(special_tenor(),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .offset = date_period{period_unit::day, 0},
                               .schedule = schedule_kind::event_lookup,
                               .schedule_step_count = 1},
                    year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::schedule_without_dates);
}

TEST_CASE("a schedule with too few remaining dates is reported as exhausted", tags) {
    const auto schedule = std::vector<schedule_date>{
        {0, year{2026} / January / 20},
    };

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(special_tenor(),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .offset = date_period{period_unit::day, 0},
                               .schedule = schedule_kind::event_lookup,
                               .schedule_step_count = 3},
                    year{2026} / January / 15),
    };

    const auto results =
        tenor_resolution_engine::resolve_batch(requests, make_calendars(), schedule);

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::schedule_exhausted);
}

TEST_CASE("schedule dates the caller left out of order are reported, not mis-stepped", tags) {
    // A walk reads forward from its start, so rows out of order answer with a
    // different date than the one asked for. That has to be an error, not a
    // quiet wrong answer.
    const auto schedule = std::vector<schedule_date>{
        {0, year{2026} / March / 18},
        {0, year{2026} / January / 20},
    };

    const auto requests = std::vector<tenor_resolution_request>{
        request_for(special_tenor(),
                    tenor_rule{.anchor = tenor_anchor::today,
                               .algorithm = resolution_algorithm::schedule_step,
                               .offset = date_period{period_unit::day, 0},
                               .schedule = schedule_kind::event_lookup,
                               .schedule_step_count = 1},
                    year{2026} / January / 15),
    };

    const auto results =
        tenor_resolution_engine::resolve_batch(requests, make_calendars(), schedule);

    REQUIRE(results.size() == 1);
    CHECK(results[0].status == resolution_status::schedule_dates_not_ordered);
}

TEST_CASE("a batch answers every row in order, and one bad row does not spoil the rest", tags) {
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / January / 15,
                    target_calendar,
                    year{2026} / January / 19),
        // No spot date, so this one cannot resolve.
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / January / 15),
        // ... and this one still does.
        request_for(period_tenor(period_unit::month, 3),
                    tenor_rule{.anchor = tenor_anchor::today},
                    year{2026} / January / 15),
        request_for(period_tenor(period_unit::month, 1), std::nullopt, year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == requests.size());
    CHECK(results[0].status == resolution_status::ok);
    CHECK(results[0].resolved_date == year{2026} / February / 19);
    CHECK(results[1].status == resolution_status::missing_spot_date);
    CHECK(results[2].status == resolution_status::ok);
    CHECK(results[2].resolved_date == year{2026} / April / 15);
    CHECK(results[3].status == resolution_status::tenor_not_in_convention);
}

TEST_CASE("an unadjusted rule leaves a weekend date exactly where the period put it", tags) {
    // The persisted default is ModifiedFollowing, but a rule may say
    // Unadjusted, and then the answer is a Sunday.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(
            period_tenor(period_unit::month, 1),
            tenor_rule{.anchor = tenor_anchor::today, .roll = business_day_convention::unadjusted},
            year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].resolved_date == year{2026} / February / 15);
}

TEST_CASE("end of month on the rule snaps a month counted period to the month's end", tags) {
    // 31 January 2026 is the last business day of its month, so a one-month
    // period snaps to the end of February and then back to the Friday,
    // because the snap carries its own Preceding and the 28th is a Saturday.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::today, .end_of_month = true},
                    year{2026} / January / 31),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].resolved_date == year{2026} / February / 27);
}

TEST_CASE("an empty batch is an empty answer rather than an error", tags) {
    const auto results = tenor_resolution_engine::resolve_batch(
        std::span<const tenor_resolution_request>{}, make_calendars());

    CHECK(results.empty());
}

TEST_CASE("an answer reports the date the roll rule moved", tags) {
    // One month from 31 January 2026 is 28 February, a Saturday, which
    // ModifiedFollowing takes back to the 27th. Both dates are the answer's,
    // because a screen that shows only the last one cannot show what moved.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::month, 1),
                    tenor_rule{.anchor = tenor_anchor::spot},
                    year{2026} / January / 15,
                    target_calendar,
                    year{2026} / January / 31),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].unadjusted_date == year{2026} / February / 28);
    CHECK(results[0].resolved_date == year{2026} / February / 27);
}

TEST_CASE("a business day walk has no pre-roll date, so it reports the one date twice", tags) {
    // A day-counted period is walked onto a business day, so the convention
    // never sees it. Reporting a different unadjusted date here would be
    // reporting a date the arithmetic never produced.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(
            special_tenor(),
            tenor_rule{.anchor = tenor_anchor::today, .offset = date_period{period_unit::day, 1}},
            year{2026} / January / 15),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].unadjusted_date == year{2026} / January / 16);
    CHECK(results[0].resolved_date == year{2026} / January / 16);
}

TEST_CASE("a schedule step reports the date the schedule gave, before the roll", tags) {
    // The CDS roll date is stated as the 20th, unadjusted, and the convention
    // moves it: 20 March 2027 is a Saturday and ModifiedFollowing takes it to
    // the Monday. Both dates travel, because the schedule's own answer and the
    // convention's answer are different claims.
    const auto requests = std::vector<tenor_resolution_request>{
        request_for(period_tenor(period_unit::year, 1),
                    tenor_rule{.anchor = tenor_anchor::spot,
                               .algorithm = resolution_algorithm::schedule_step,
                               .schedule = schedule_kind::cds_roll_quarter,
                               .schedule_step_count = 1},
                    year{2026} / January / 1,
                    target_calendar,
                    year{2026} / January / 2),
    };

    const auto results = tenor_resolution_engine::resolve_batch(requests, make_calendars());

    REQUIRE(results.size() == 1);
    CHECK(results[0].unadjusted_date == year{2027} / March / 20);
    CHECK(results[0].resolved_date == year{2027} / March / 22);
}
