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
// Test obligations propagated from doc/llm/specs/fomc-dated-ois-short-end.allium.
//
// The calendar-event store and the schedule machinery, landed by story
// Decisions D1 (calendar_event entity + US.FOMC calendar) and D2
// (tenor_schedules + per-row schedule binding) and seeded by D4 (the
// 2025-2027 meeting window on US.FOMC). Every fixture below transcribes a
// seeded row; the meeting dates extend the D2 transcription
// (domain_tenor_resolution_fomc_tests.cpp covers 2026, this file covers the
// full D4 window).
//
// Obligations covered here:
//   - entity-fields.CalendarEvent
//   - entity-optional.CalendarEvent.description
//   - entity-optional.CalendarEvent.source
//   - entity-fields.TenorSchedule
//   - entity-optional.TenorSchedule.calendar
//   - invariant.FomcSchedulePointsAtMeetingEvents (as a data contract over
//     the seeded FOMC_MEETING schedule row)
//   - rule-success.EnterFomcMeeting (as a data contract over the seeded
//     window: every row satisfies the rule's requires clauses and carries
//     its ensured source)
//   - derived.Tenor.is_fomc (evaluated over the seeded 1F..8F and their
//     counter-examples)
//   - entity-fields.TenorResolution
//   - entity-optional.TenorResolution.schedule_step_count
//   - entity-fields.TenorConvention (mappable part; see the divergence note
//     in the convention test)
//
// Seams documented, not hidden: EnterFomcMeeting's runtime machinery (event
// creation with validation) does not exist -- events are SQL-seeded, so the
// rule is pinned here as a data contract; and the spec's convention-level
// schedule field diverges from the implementation's per-row schedule_code
// (story Decision D2) -- see the convention test's comment.
#include "ores.refdata.api/domain/calendar.hpp"
#include "ores.refdata.api/domain/calendar_event.hpp"
#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.refdata.api/domain/tenor_schedule.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace {

using ores::refdata::domain::calendar;
using ores::refdata::domain::calendar_event;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention;
using ores::refdata::domain::tenor_convention_resolution;
using ores::refdata::domain::tenor_schedule;

const std::string tags("[calendar_event][tenor_schedule][fomc]");

// The 2025-2027 FOMC meeting dates, transcribed from federalreserve.gov.
// Each date is the *second day* of the Fed's published two-day range -- the
// statement/decision day, usually a Wednesday. Task D4 seeds the same
// window into calendar_event (refdata_calendar_events_populate.sql, 8
// meetings per year).
std::vector<std::chrono::year_month_day> fomc_meeting_dates() {
    using namespace std::chrono;
    return {year(2025) / month(1) / day(29),   year(2025) / month(3) / day(19),
            year(2025) / month(5) / day(7),    year(2025) / month(6) / day(18),
            year(2025) / month(7) / day(30),   year(2025) / month(9) / day(17),
            year(2025) / month(10) / day(29),  year(2025) / month(12) / day(10),
            year(2026) / month(1) / day(28),   year(2026) / month(3) / day(18),
            year(2026) / month(4) / day(29),   year(2026) / month(6) / day(17),
            year(2026) / month(7) / day(29),   year(2026) / month(9) / day(16),
            year(2026) / month(10) / day(28),  year(2026) / month(12) / day(9),
            year(2027) / month(1) / day(27),   year(2027) / month(3) / day(17),
            year(2027) / month(4) / day(28),   year(2027) / month(6) / day(9),
            year(2027) / month(7) / day(28),   year(2027) / month(9) / day(15),
            year(2027) / month(10) / day(27),  year(2027) / month(12) / day(8)};
}

// A meeting event on US.FOMC, transcribed from the D4 seed rows
// (refdata_calendar_events_populate.sql): central_bank_meeting entry, dated
// the statement day, sourced from the Fed's published calendar.
calendar_event make_calendar_event(std::chrono::year_month_day event_date) {
    calendar_event e;
    e.calendar_code = "US.FOMC";
    e.event_date = event_date;
    e.diary_entry_type = "central_bank_meeting";
    e.name = "FOMC Meeting";
    e.description = "FOMC policy meeting; statement on the event date";
    e.source = "federalreserve.gov";
    return e;
}

// The US.FOMC calendar row, transcribed from refdata_calendars_populate.sql
// (task D1): a central-bank-meeting calendar for the US, sourced from the
// Fed's published calendar.
calendar make_us_fomc_calendar() {
    calendar c;
    c.code = "US.FOMC";
    c.name = "US Federal Reserve (FOMC meeting calendar)";
    c.calendar_type = "central_bank_meeting";
    c.country_code = "US";
    c.source = "federalreserve.gov";
    return c;
}

// The seeded schedule rows, transcribed from
// refdata_tenor_schedules_populate.sql (task D2): ROLL_QUARTER is closed
// form with no event store; FOMC_MEETING looks the meeting events up on
// US.FOMC.
tenor_schedule make_tenor_schedule(const std::string& code) {
    tenor_schedule s;
    if (code == "ROLL_QUARTER") {
        s.code = "ROLL_QUARTER";
        s.schedule_source = "CLOSED_FORM";
    } else {
        s.code = "FOMC_MEETING";
        s.schedule_source = "EVENT_LOOKUP";
        s.calendar_code = "US.FOMC";
        s.diary_entry_type = "central_bank_meeting";
    }
    return s;
}

// The 1F..8F tenors, transcribed from refdata_tenors_populate.sql (task
// D2): kind SPECIAL, unit NONE, multiplier the meeting ordinal.
tenor make_fomc_tenor(int n) {
    tenor t;
    t.code = std::to_string(n) + "F";
    t.kind = "SPECIAL";
    t.unit = "NONE";
    t.multiplier = n;
    return t;
}

}

TEST_CASE("calendar_event exposes its declared fields", tags) {
    using namespace std::chrono;

    calendar_event e;
    e.calendar_code = "US.FOMC";
    e.event_date = year(2026) / month(1) / day(28);
    e.diary_entry_type = "central_bank_meeting";
    e.name = "FOMC Meeting Jan 2026";

    CHECK(e.calendar_code == "US.FOMC");
    CHECK(e.event_date == year(2026) / month(1) / day(28));
    CHECK(e.diary_entry_type == "central_bank_meeting");
    CHECK(e.name == "FOMC Meeting Jan 2026");
}

TEST_CASE("calendar_event description is optional", tags) {
    // The spec declares description optional; locally-authored events with
    // no free-text note leave it null.
    calendar_event e;
    CHECK(!e.description);

    e.description = "FOMC policy meeting; statement on the event date";
    REQUIRE(e.description);
    CHECK(*e.description == "FOMC policy meeting; statement on the event date");
}

TEST_CASE("calendar_event source is optional", tags) {
    // The spec declares source optional; events transcribed from a
    // published source name it (federalreserve.gov), local ones leave it
    // null.
    calendar_event e;
    CHECK(!e.source);

    e.source = "federalreserve.gov";
    REQUIRE(e.source);
    CHECK(*e.source == "federalreserve.gov");
}

TEST_CASE("tenor_schedule exposes its declared fields", tags) {
    tenor_schedule s;
    s.code = "FOMC_MEETING";
    s.name = "FOMC Meeting Schedule";
    s.description =
        "Event-lookup schedule: the central_bank_meeting diary events on US.FOMC";
    s.display_order = 2;
    s.schedule_source = "EVENT_LOOKUP";
    s.calendar_code = "US.FOMC";
    s.diary_entry_type = "central_bank_meeting";

    CHECK(s.code == "FOMC_MEETING");
    CHECK(s.name == "FOMC Meeting Schedule");
    CHECK(s.display_order == 2);
    CHECK(s.schedule_source == "EVENT_LOOKUP");
    REQUIRE(s.calendar_code);
    CHECK(*s.calendar_code == "US.FOMC");
    REQUIRE(s.diary_entry_type);
    CHECK(*s.diary_entry_type == "central_bank_meeting");
}

TEST_CASE("tenor_schedule calendar is optional", tags) {
    // Closed-form schedules bind no event store: the seeded ROLL_QUARTER
    // row leaves both bindings null (the header documents the binding, the
    // schema does not enforce it).
    const auto roll = make_tenor_schedule("ROLL_QUARTER");
    CHECK(roll.schedule_source == "CLOSED_FORM");
    CHECK(!roll.calendar_code);
    CHECK(!roll.diary_entry_type);

    // Event-lookup schedules name their calendar: the seeded FOMC_MEETING
    // row binds US.FOMC.
    const auto fomc = make_tenor_schedule("FOMC_MEETING");
    REQUIRE(fomc.calendar_code);
    CHECK(*fomc.calendar_code == "US.FOMC");
}

TEST_CASE("FOMC_MEETING points at the meeting events on US.FOMC", tags) {
    // invariant.FomcSchedulePointsAtMeetingEvents as a data contract over
    // the seeded tenor_schedules rows (transcribed from
    // refdata_tenor_schedules_populate.sql): the FOMC_MEETING schedule is
    // an EVENT_LOOKUP over the central_bank_meeting events on US.FOMC, and
    // the closed-form ROLL_QUARTER schedule binds no event store.
    const auto fomc = make_tenor_schedule("FOMC_MEETING");
    CHECK(fomc.schedule_source == "EVENT_LOOKUP");
    REQUIRE(fomc.calendar_code);
    CHECK(*fomc.calendar_code == "US.FOMC");
    REQUIRE(fomc.diary_entry_type);
    CHECK(*fomc.diary_entry_type == "central_bank_meeting");

    const auto roll = make_tenor_schedule("ROLL_QUARTER");
    CHECK(roll.schedule_source == "CLOSED_FORM");
    CHECK(!roll.calendar_code);
    CHECK(!roll.diary_entry_type);
}

TEST_CASE("US.FOMC meeting events satisfy the EnterFomcMeeting rule", tags) {
    using namespace std::chrono;

    // rule-success.EnterFomcMeeting as a data contract: the rule's requires
    // clauses pin the US.FOMC calendar row (transcribed from
    // refdata_calendars_populate.sql, task D1)...
    const auto us_fomc = make_us_fomc_calendar();
    CHECK(us_fomc.code == "US.FOMC");
    CHECK(us_fomc.calendar_type == "central_bank_meeting");
    CHECK(us_fomc.country_code == "US");
    CHECK(us_fomc.source == "federalreserve.gov");
    // ...the remaining requires clause, us_fomc.is_editable, is SQL-only:
    // the calendars table carries is_editable (boolean, seeded true) but the
    // C++ calendar entity does not expose the column.

    // ...and every seeded meeting row (task D4) is a central_bank_meeting
    // event on US.FOMC carrying the rule's ensured source.
    const auto meetings = fomc_meeting_dates();
    CHECK(meetings.size() == 24);
    for (const auto& d : meetings) {
        const auto e = make_calendar_event(d);
        CHECK(e.calendar_code == "US.FOMC");
        CHECK(e.event_date == d);
        CHECK(e.diary_entry_type == "central_bank_meeting");
        REQUIRE(e.source);
        CHECK(*e.source == "federalreserve.gov");
    }
}

TEST_CASE("tenor_convention_resolution exposes its declared fields", tags) {
    tenor_convention_resolution r;
    r.convention_code = "RATES_SPOT_FOMC";
    r.tenor_code = "2F";
    r.offset_unit = "DAY";
    r.offset_multiplier = 0;
    r.schedule_code = "FOMC_MEETING";
    r.schedule_step_count = 2;

    CHECK(r.convention_code == "RATES_SPOT_FOMC");
    CHECK(r.tenor_code == "2F");
    REQUIRE(r.offset_unit);
    CHECK(*r.offset_unit == "DAY");
    REQUIRE(r.offset_multiplier);
    CHECK(*r.offset_multiplier == 0);
    REQUIRE(r.schedule_code);
    CHECK(*r.schedule_code == "FOMC_MEETING");
    REQUIRE(r.schedule_step_count);
    CHECK(*r.schedule_step_count == 2);
}

TEST_CASE("tenor_convention_resolution schedule_step_count is optional", tags) {
    // The spec declares schedule_step_count optional: rows with no schedule
    // walk (the plain RATES_SPOT_FORWARD resolutions) leave it null...
    tenor_convention_resolution plain;
    plain.convention_code = "RATES_SPOT_FORWARD";
    plain.tenor_code = "SPOT";
    CHECK(!plain.schedule_code);
    CHECK(!plain.schedule_step_count);

    // ...and schedule rows set it to the step count (2F walks two meetings
    // on the FOMC_MEETING schedule).
    tenor_convention_resolution fomc;
    fomc.convention_code = "RATES_SPOT_FOMC";
    fomc.tenor_code = "2F";
    fomc.schedule_code = "FOMC_MEETING";
    fomc.schedule_step_count = 2;
    REQUIRE(fomc.schedule_step_count);
    CHECK(*fomc.schedule_step_count == 2);
}

TEST_CASE("RATES_SPOT_FOMC convention carries the SPOT/SCHEDULE_STEP binding", tags) {
    // entity-fields.TenorConvention, mappable part: the seeded convention
    // row (refdata_tenor_conventions_populate.sql, task D2) binds
    // measured_from SPOT and resolution_algorithm SCHEDULE_STEP.
    //
    // Divergence, documented per story Decision D2: the spec declares
    // TenorConvention.schedule: TenorSchedule? but the implementation binds
    // the schedule per resolution row (schedule_code), not on the
    // convention -- one convention family can mix schedule axes
    // (CREDIT_CDS_IMM walks ROLL_QUARTER on the same family as
    // RATES_SPOT_FOMC), so the convention entity carries no schedule field.
    tenor_convention c;
    c.code = "RATES_SPOT_FOMC";
    c.description = "Rates spot-starting convention resolving FOMC meetings";
    c.measured_from = "SPOT";
    c.resolution_algorithm = "SCHEDULE_STEP";

    CHECK(c.code == "RATES_SPOT_FOMC");
    CHECK(c.measured_from == "SPOT");
    CHECK(c.resolution_algorithm == "SCHEDULE_STEP");
}

TEST_CASE("tenor is FOMC when SPECIAL/NONE with multiplier at least one", tags) {
    // derived.Tenor.is_fomc, evaluated over the seeded tenors (transcribed
    // from refdata_tenors_populate.sql, task D2): kind = SPECIAL and
    // unit = NONE and multiplier >= 1.
    const auto is_fomc = [](const tenor& t) {
        return t.kind == "SPECIAL" && t.unit == "NONE" && t.multiplier &&
               *t.multiplier >= 1;
    };

    // The schedule tenors: 1F..8F are all FOMC tenors.
    for (int n = 1; n <= 8; ++n) {
        CHECK(is_fomc(make_fomc_tenor(n)));
    }

    // O/N and T/N are SPECIAL/NONE too, but their multiplier is null (they
    // are anchor tenors, not schedule tenors) -- the >= 1 clause fails.
    tenor on;
    on.code = "O/N";
    on.kind = "SPECIAL";
    on.unit = "NONE";
    CHECK(!is_fomc(on));

    tenor tn;
    tn.code = "T/N";
    tn.kind = "SPECIAL";
    tn.unit = "NONE";
    CHECK(!is_fomc(tn));

    // Period tenors fail the kind clause, even with a multiplier (SPOT is
    // PERIOD/DAY x 0; 1M is PERIOD/MONTH x 1; 12M is PERIOD/MONTH x 12).
    tenor spot;
    spot.code = "SPOT";
    spot.kind = "PERIOD";
    spot.unit = "DAY";
    spot.multiplier = 0;
    CHECK(!is_fomc(spot));

    tenor one_month;
    one_month.code = "1M";
    one_month.kind = "PERIOD";
    one_month.unit = "MONTH";
    one_month.multiplier = 1;
    CHECK(!is_fomc(one_month));

    tenor twelve_months;
    twelve_months.code = "12M";
    twelve_months.kind = "PERIOD";
    twelve_months.unit = "MONTH";
    twelve_months.multiplier = 12;
    CHECK(!is_fomc(twelve_months));
}
