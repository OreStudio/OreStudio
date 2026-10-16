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
#include "ores.refdata.api/generators/calendar_generator.hpp"
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.refdata.core/repository/calendar_exception_repository.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.refdata.core/repository/tenor_repository.hpp"
#include "ores.refdata.core/repository/tenor_schedule_repository.hpp"
#include "ores.refdata.core/service/calendar_materialisation_service.hpp"
#include "ores.refdata.core/service/tenor_resolution_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::refdata::generators;

using ores::refdata::domain::calendar_exception;
using ores::refdata::domain::tenor;
using ores::refdata::domain::tenor_convention;
using ores::refdata::domain::tenor_convention_resolution;
using ores::refdata::domain::tenor_schedule;
using ores::refdata::messaging::resolve_tenor_request;
using ores::refdata::messaging::resolve_tenor_response;
using ores::refdata::repository::calendar_exception_repository;
using ores::refdata::repository::calendar_repository;
using ores::refdata::repository::country_repository;
using ores::refdata::repository::tenor_convention_repository;
using ores::refdata::repository::tenor_convention_resolution_repository;
using ores::refdata::repository::tenor_repository;
using ores::refdata::repository::tenor_schedule_repository;
using ores::refdata::service::calendar_materialisation_service;
using ores::refdata::service::tenor_resolution_service;
using ores::testing::scoped_database_helper;
using ores::utility::domain::outcome;

namespace {

const std::string tags("[service][tenor_resolution]");

constexpr auto horizon = std::chrono::year{2026} / std::chrono::January / 15;
constexpr auto spot = std::chrono::year{2026} / std::chrono::January / 19;

/// The calendars carry a country code, and the sentinel is the one a
/// synthetic row may name.
void write_zz_country_sentinel(scoped_database_helper& h,
                               ores::utility::generation::generation_context& gctx) {
    country_repository{}.write(h.context(), {generate_country_sentinel(gctx)});
}

/// Everything a resolution reads, written as rows rather than generated: the
/// service's whole job is mapping these, so a test that let a generator pick
/// the codes would not be testing the mapping.
struct fixture {
    scoped_database_helper h;
    std::string calendar_code;
    std::string convention_code = "TEST_SPOT_FORWARD";
    std::string tenor_code = "1M";

    explicit fixture(bool materialise = true, const std::string& algorithm = "ANCHOR_OFFSET") {
        auto gctx = ores::testing::make_generation_context(h);
        write_zz_country_sentinel(h, gctx);

        auto calendar = generate_synthetic_calendar(gctx);
        calendar.source = "user";
        calendar.is_editable = true;
        calendar_code = calendar.code;
        calendar_repository{}.write(h.context(), {calendar});

        write_tenor("1M", "PERIOD", "MONTH", 1);
        write_convention("SPOT", algorithm);
        write_resolution();

        if (materialise) {
            calendar_materialisation_service service(h.context());
            service.regenerate(calendar_code, horizon.year());
        }
    }

    void write_tenor(const std::string& code,
                     const std::string& kind,
                     const std::string& unit,
                     int multiplier) {
        tenor t;
        t.tenant_id = h.tenant_id();
        t.code = code;
        t.display_name = code;
        t.kind = kind;
        t.unit = unit;
        t.multiplier = multiplier;
        t.modified_by = h.db_user();
        t.performed_by = h.db_user();
        t.change_reason_code = "system.test";
        t.change_commentary = "Synthetic test data";
        tenor_repository{}.write(h.context(), {t});
    }

    void write_convention(const std::string& measured_from = "SPOT",
                          const std::string& algorithm = "ANCHOR_OFFSET",
                          const std::string& roll = "ModifiedFollowing") {
        tenor_convention c;
        c.tenant_id = h.tenant_id();
        c.code = convention_code;
        c.description = "Synthetic test convention";
        c.measured_from = measured_from;
        c.resolution_algorithm = algorithm;
        c.business_day_convention_type = roll;
        c.modified_by = h.db_user();
        c.performed_by = h.db_user();
        c.change_reason_code = "system.test";
        c.change_commentary = "Synthetic test data";
        tenor_convention_repository{}.write(h.context(), {c});
    }

    void write_resolution(std::optional<std::string> schedule_code = std::nullopt,
                          std::optional<int> step_count = std::nullopt) {
        tenor_convention_resolution r;
        r.tenant_id = h.tenant_id().to_string();
        r.convention_code = convention_code;
        r.tenor_code = tenor_code;
        r.schedule_code = schedule_code;
        r.schedule_step_count = step_count;
        r.modified_by = h.db_user();
        r.performed_by = h.db_user();
        r.change_reason_code = "system.test";
        r.change_commentary = "Synthetic test data";
        tenor_convention_resolution_repository{h.context()}.write({r});
    }

    void write_schedule(const std::string& code, const std::string& source) {
        tenor_schedule s;
        s.tenant_id = h.tenant_id();
        s.code = code;
        s.name = code;
        s.schedule_source = source;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "Synthetic test data";
        tenor_schedule_repository{}.write(h.context(), {s});
    }

    void close_on(std::chrono::year_month_day date) {
        static boost::uuids::random_generator gen;
        calendar_exception e;
        e.id = gen();
        e.tenant_id = h.tenant_id();
        e.calendar_code = calendar_code;
        e.exception_date = date;
        e.is_business_day = false;
        e.description = "Synthetic test exception";
        e.modified_by = h.db_user();
        e.performed_by = h.db_user();
        e.change_reason_code = "system.test";
        e.change_commentary = "Synthetic test data";
        calendar_exception_repository{}.write(h.context(), {e});

        calendar_materialisation_service service(h.context());
        service.regenerate(calendar_code, date.year());
    }

    resolve_tenor_request request(std::optional<std::chrono::year_month_day> spot_date = spot) {
        resolve_tenor_request req;
        req.tenor_code = tenor_code;
        req.convention_code = convention_code;
        req.calendar_code = calendar_code;
        req.horizon = horizon;
        req.spot = spot_date;
        return req;
    }

    resolve_tenor_response resolve(std::optional<std::chrono::year_month_day> spot_date = spot) {
        tenor_resolution_service service(h.context());
        return service.resolve(request(spot_date));
    }
};

}

TEST_CASE("resolves a period tenor to the date its convention rolls it to", tags) {
    fixture f;

    // One month from 31 January 2026 is 28 February, a Saturday, which the
    // convention's ModifiedFollowing takes back to the Friday. Both dates
    // travel: the one the arithmetic reached and the one the roll made.
    auto req = f.request();
    req.horizon = std::chrono::year{2026} / std::chrono::January / 15;
    req.spot = std::chrono::year{2026} / std::chrono::January / 31;

    tenor_resolution_service service(f.h.context());
    const auto response = service.resolve(req);

    CHECK(response.result.outcome == outcome::ok);
    CHECK(response.anchor_date == std::chrono::year{2026} / std::chrono::January / 31);
    CHECK(response.date_before_roll == std::chrono::year{2026} / std::chrono::February / 28);
    CHECK(response.adjusted_date == std::chrono::year{2026} / std::chrono::February / 27);
    CHECK(response.roll_rule == "ModifiedFollowing");
    CHECK(response.calendar == f.calendar_code);
}

TEST_CASE("a holiday the calendar materialised moves the date the rule reached", tags) {
    // Not materialised up front: materialisation is extend-only, so a
    // calendar already written for 2026 would not be reopened to see the
    // exception added after it.
    fixture f(false);

    // Close the Friday the month end would have rolled back onto, so the
    // resolution has to walk back past it to the Thursday. That is the
    // calendar's own contribution, distinct from the weekend's.
    f.close_on(std::chrono::year{2026} / std::chrono::February / 27);

    auto req = f.request();
    req.spot = std::chrono::year{2026} / std::chrono::January / 31;

    tenor_resolution_service service(f.h.context());
    const auto response = service.resolve(req);

    CHECK(response.result.outcome == outcome::ok);
    CHECK(response.date_before_roll == std::chrono::year{2026} / std::chrono::February / 28);
    CHECK(response.adjusted_date == std::chrono::year{2026} / std::chrono::February / 26);
}

TEST_CASE("a tenor the convention does not carry is refused with the row that is missing", tags) {
    fixture f;
    f.tenor_code = "3M";
    f.write_tenor("3M", "PERIOD", "MONTH", 3);

    const auto response = f.resolve();

    CHECK(response.result.outcome == outcome::missing);
    CHECK(response.result.code == "tenor_not_in_convention");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "tenor_convention_resolutions");
}

TEST_CASE("a calendar nobody materialised is refused rather than read as all business days", tags) {
    fixture f(false);

    const auto response = f.resolve();

    CHECK(response.result.outcome == outcome::missing);
    CHECK(response.result.code == "calendar_not_materialised");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "calendar_code");
}

TEST_CASE("a spot anchored rule refuses when no spot date is supplied, and names it", tags) {
    fixture f;

    const auto response = f.resolve(std::nullopt);

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "missing_spot_date");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "spot");
    // Nothing was counted, so nothing is claimed: a spot-anchored rule has no
    // anchor until the caller says what spot is, and an invented one would be
    // worse than an empty field.
    CHECK_FALSE(response.anchor_date.ok());
}

TEST_CASE("a refusal that got as far as an anchor reports the date it counted from", tags) {
    fixture f;
    f.tenor_code = "O/N";
    f.write_tenor("O/N", "SPECIAL", "NONE", 0);
    // A row for the tenor, but with no offset: a SPECIAL tenor has no
    // duration of its own, so the rule has to supply one and this one does
    // not. The anchor resolved before that was discovered.
    f.write_resolution();

    const auto response = f.resolve();

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "special_tenor_without_offset");
    CHECK(response.anchor_date == spot);
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "tenor_convention_resolutions");
}

TEST_CASE("a tenor that does not exist is refused with the code it was asked for", tags) {
    fixture f;
    auto req = f.request();
    req.tenor_code = "NOSUCH";

    tenor_resolution_service service(f.h.context());
    const auto response = service.resolve(req);

    CHECK(response.result.outcome == outcome::missing);
    CHECK(response.result.code == "tenor_not_found");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "tenor_code");
}

TEST_CASE("a closed form schedule resolves a year tenor to its quarterly roll date", tags) {
    // SCHEDULE_STEP is what makes the row's schedule worth reading; under
    // ANCHOR_OFFSET the engine resolves on the calendar axis and the schedule
    // is not consulted at all.
    fixture f(true, "SCHEDULE_STEP");
    f.tenor_code = "1Y";
    f.write_tenor("1Y", "PERIOD", "YEAR", 1);
    f.write_schedule("ROLL_QUARTER", "CLOSED_FORM");
    f.write_resolution("ROLL_QUARTER", 1);

    {
        tenor_convention_resolution_repository probe{f.h.context()};
        const auto rows = probe.read_latest_by_convention(f.convention_code);
        const auto written = std::find_if(
            rows.begin(), rows.end(), [](const auto& row) { return row.tenor_code == "1Y"; });
        REQUIRE(written != rows.end());
        REQUIRE(written->schedule_code.has_value());
        CHECK(*written->schedule_code == "ROLL_QUARTER");
        CHECK(written->schedule_step_count.value_or(0) == 1);
    }

    auto req = f.request(std::chrono::year{2026} / std::chrono::January / 2);
    req.horizon = std::chrono::year{2026} / std::chrono::January / 1;

    tenor_resolution_service service(f.h.context());
    const auto response = service.resolve(req);

    CHECK(response.result.outcome == outcome::ok);
    // A year from spot is 2 January 2027; the first quarterly roll date on or
    // after it is 20 March 2027, which is a Saturday, and the convention
    // takes it to the Monday.
    CHECK(response.date_before_roll == std::chrono::year{2027} / std::chrono::March / 20);
    CHECK(response.adjusted_date == std::chrono::year{2027} / std::chrono::March / 22);
}

TEST_CASE("a closed form schedule the engine cannot walk is refused, not approximated", tags) {
    fixture f(true, "SCHEDULE_STEP");
    f.write_schedule("SOME_OTHER_FORM", "CLOSED_FORM");
    f.write_resolution("SOME_OTHER_FORM", 1);

    const auto response = f.resolve();

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "schedule_not_walkable");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "schedule_code");
}

TEST_CASE("a row naming a schedule under an anchor-offset convention is refused", tags) {
    // The engine never consults a schedule for ANCHOR_OFFSET, so a row that
    // names one states something the resolution will ignore. Refusing says so
    // rather than resolving one of the two readings and leaving the other
    // silently unread.
    fixture f;
    f.write_schedule("ROLL_QUARTER", "CLOSED_FORM");
    f.write_resolution("ROLL_QUARTER", 1);

    const auto response = f.resolve();

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "schedule_under_anchor_offset");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "resolution_algorithm");
}
