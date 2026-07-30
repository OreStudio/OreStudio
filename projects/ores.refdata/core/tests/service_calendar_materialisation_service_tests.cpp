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
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_exception.hpp"
#include "ores.refdata.api/domain/calendar_rule.hpp"
#include "ores.refdata.api/generators/calendar_generator.hpp"
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.refdata.core/repository/calendar_exception_repository.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include "ores.refdata.core/repository/calendar_rule_repository.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/service/calendar_materialisation_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::calendar_exception;
using ores::refdata::domain::calendar_rule;
using ores::refdata::repository::calendar_date_repository;
using ores::refdata::repository::calendar_exception_repository;
using ores::refdata::repository::calendar_repository;
using ores::refdata::repository::calendar_rule_repository;
using ores::refdata::repository::country_repository;
using ores::refdata::service::calendar_materialisation_service;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[service][calendar_materialisation]");

void write_zz_country_sentinel(scoped_database_helper& h,
                               ores::utility::generation::generation_context& gctx) {
    country_repository cty_repo;
    cty_repo.write(h.context(), {generate_country_sentinel(gctx)});
}

// Nth occurrence of a weekday in a given month/year -- Monday is never a
// weekend day under the engine's default weekend_mask, so dates built this
// way are safe to use as deterministic, weekday-independent test fixtures
// (unlike a fixed month/day, whose weekday -- and thus business-day status
// -- depends on which year "next year" happens to be at test-run time).
std::chrono::year_month_day
nth_monday_of_june(std::chrono::year y, unsigned occurrence) {
    using namespace std::chrono;
    const year_month_weekday ymw{y, June, weekday_indexed{Monday, occurrence}};
    return year_month_day{sys_days{ymw}};
}

calendar_rule make_first_monday_of_june_rule(scoped_database_helper& h,
                                             const std::string& calendar_code) {
    static boost::uuids::random_generator gen;
    calendar_rule r;
    r.id = gen();
    r.tenant_id = h.tenant_id();
    r.calendar_code = calendar_code;
    r.kind = "nth_weekday_of_month";
    r.month = 6;
    r.weekday = 1; // Monday, std::chrono::weekday's c_encoding.
    r.occurrence = 1;
    r.shift = "none";
    r.modified_by = h.db_user();
    r.performed_by = h.db_user();
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    return r;
}

calendar_exception make_exception(scoped_database_helper& h,
                                  const std::string& calendar_code,
                                  std::chrono::year_month_day date,
                                  bool is_business_day) {
    static boost::uuids::random_generator gen;
    calendar_exception e;
    e.id = gen();
    e.tenant_id = h.tenant_id();
    e.calendar_code = calendar_code;
    e.exception_date = date;
    e.is_business_day = is_business_day;
    e.description = "Synthetic test exception";
    e.modified_by = h.db_user();
    e.performed_by = h.db_user();
    e.change_reason_code = "system.test";
    e.change_commentary = "Synthetic test data";
    return e;
}

} // namespace

TEST_CASE("regenerate_base_less_calendar_applies_rules_and_exceptions", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    calendar_repository cal_repo;
    calendar_rule_repository rule_repo;
    calendar_exception_repository exc_repo;
    calendar_date_repository date_repo(h.context());
    calendar_materialisation_service service(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);

    auto cal = generate_synthetic_calendar(gctx);
    cal.source = "user";
    cal.is_editable = true;
    cal_repo.write(h.context(), {cal});

    rule_repo.write(h.context(), {make_first_monday_of_june_rule(h, cal.code)});

    const auto today_y = std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(
                                                         std::chrono::system_clock::now())}
                             .year();
    const auto next_year = today_y + std::chrono::years{1};
    const auto rule_holiday = nth_monday_of_june(next_year, 1);
    // An additional, exception-driven holiday on the 2nd Monday of June --
    // distinct from the rule-generated one, exercising the
    // is_business_day=false exception path.
    const auto extra_holiday = nth_monday_of_june(next_year, 2);
    // The 3rd Monday of June has no rule or exception against it: a plain
    // business day, used to confirm the calendar isn't over-eagerly
    // marking every Monday as a holiday.
    const auto plain_business_day = nth_monday_of_june(next_year, 3);

    exc_repo.write(h.context(), {make_exception(h, cal.code, extra_holiday, false)});

    const auto written = service.regenerate(cal.code, next_year);
    CHECK(written > 0);

    auto rows = date_repo.read_latest_by_calendar(cal.code);
    BOOST_LOG_SEV(lg, debug) << "Materialised " << rows.size() << " calendar_date rows";
    REQUIRE(!rows.empty());

    bool found_rule_holiday = false;
    bool found_extra_holiday = false;
    bool found_plain_business_day = false;
    for (const auto& row : rows) {
        CHECK(row.source == "user_defined");
        if (row.date == rule_holiday) {
            found_rule_holiday = true;
            CHECK_FALSE(row.is_business_day);
        }
        if (row.date == extra_holiday) {
            found_extra_holiday = true;
            CHECK_FALSE(row.is_business_day);
        }
        if (row.date == plain_business_day) {
            found_plain_business_day = true;
            CHECK(row.is_business_day);
        }
    }
    CHECK(found_rule_holiday);
    CHECK(found_extra_holiday);
    CHECK(found_plain_business_day);
}

TEST_CASE("regenerate_exception_overrides_rule_holiday_back_to_business_day", tags) {
    scoped_database_helper h;
    calendar_repository cal_repo;
    calendar_rule_repository rule_repo;
    calendar_exception_repository exc_repo;
    calendar_date_repository date_repo(h.context());
    calendar_materialisation_service service(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);

    auto cal = generate_synthetic_calendar(gctx);
    cal.source = "user";
    cal.is_editable = true;
    cal_repo.write(h.context(), {cal});
    rule_repo.write(h.context(), {make_first_monday_of_june_rule(h, cal.code)});

    const auto today_y = std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(
                                                         std::chrono::system_clock::now())}
                             .year();
    const auto next_year = today_y + std::chrono::years{1};
    const auto rule_holiday = nth_monday_of_june(next_year, 1);

    // Override the rule-generated holiday back to an open business day.
    exc_repo.write(h.context(), {make_exception(h, cal.code, rule_holiday, true)});

    CHECK(service.regenerate(cal.code, next_year) > 0);

    auto rows = date_repo.read_latest_by_calendar(cal.code);
    bool found = false;
    for (const auto& row : rows) {
        if (row.date == rule_holiday) {
            found = true;
            CHECK(row.is_business_day);
        }
    }
    CHECK(found);
}

TEST_CASE("regenerate_based_calendar_inherits_base_and_overlays_own_exceptions", tags) {
    scoped_database_helper h;
    calendar_repository cal_repo;
    calendar_rule_repository rule_repo;
    calendar_exception_repository exc_repo;
    calendar_date_repository date_repo(h.context());
    calendar_materialisation_service service(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);

    auto base = generate_synthetic_calendar(gctx);
    base.source = "user";
    base.is_editable = true;

    auto derived = generate_synthetic_calendar(gctx);
    derived.source = "user";
    derived.is_editable = true;
    derived.base_calendar_code = base.code;

    cal_repo.write(h.context(), {base});
    cal_repo.write(h.context(), {derived});
    rule_repo.write(h.context(), {make_first_monday_of_june_rule(h, base.code)});

    const auto today_y = std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(
                                                         std::chrono::system_clock::now())}
                             .year();
    const auto next_year = today_y + std::chrono::years{1};
    const auto rule_holiday = nth_monday_of_june(next_year, 1);

    // Derived calendar overrides the base's rule holiday back to a business
    // day -- only the derived calendar's own dates should reflect this, not
    // the base's.
    exc_repo.write(h.context(), {make_exception(h, derived.code, rule_holiday, true)});

    CHECK(service.regenerate(base.code, next_year) > 0);
    CHECK(service.regenerate(derived.code, next_year) > 0);

    auto base_rows = date_repo.read_latest_by_calendar(base.code);
    auto derived_rows = date_repo.read_latest_by_calendar(derived.code);
    REQUIRE(!base_rows.empty());
    REQUIRE(!derived_rows.empty());
    CHECK(base_rows.size() == derived_rows.size());

    for (const auto& row : base_rows)
        if (row.date == rule_holiday)
            CHECK_FALSE(row.is_business_day);

    for (const auto& row : derived_rows) {
        CHECK(row.source == "user_adjustment");
        if (row.date == rule_holiday)
            CHECK(row.is_business_day);
    }
}

TEST_CASE("regenerate_is_extend_only_and_does_not_rewrite_existing_watermark", tags) {
    scoped_database_helper h;
    calendar_repository cal_repo;
    calendar_rule_repository rule_repo;
    calendar_date_repository date_repo(h.context());
    calendar_materialisation_service service(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);

    auto cal = generate_synthetic_calendar(gctx);
    cal.source = "user";
    cal.is_editable = true;
    cal_repo.write(h.context(), {cal});
    rule_repo.write(h.context(), {make_first_monday_of_june_rule(h, cal.code)});

    const auto today_y = std::chrono::year_month_day{std::chrono::floor<std::chrono::days>(
                                                         std::chrono::system_clock::now())}
                             .year();
    const auto next_year = today_y + std::chrono::years{1};

    const auto first_pass = service.regenerate(cal.code, next_year);
    CHECK(first_pass > 0);

    // Re-running with the same horizon must not rewrite anything already
    // materialised below the watermark.
    const auto second_pass = service.regenerate(cal.code, next_year);
    CHECK(second_pass == 0);

    // Extending the horizon further only writes the newly-covered days.
    const auto extended_pass = service.regenerate(cal.code, next_year + std::chrono::years{1});
    CHECK(extended_pass > 0);
}

TEST_CASE("regenerate_unknown_calendar_throws", tags) {
    scoped_database_helper h;
    calendar_materialisation_service service(h.context());

    CHECK_THROWS_AS(service.regenerate("NO-SUCH-CALENDAR"), std::runtime_error);
}

TEST_CASE("regenerate_throws_when_base_calendar_removed_after_being_referenced", tags) {
    scoped_database_helper h;
    calendar_repository cal_repo;
    calendar_materialisation_service service(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);

    // calendars_insert_fn's soft-FK trigger validates base_calendar_code
    // against an existing calendar row at write time, so a genuine cycle
    // (or a base_calendar_code that was never valid) can never be
    // persisted in the first place. What *can* happen is a
    // base_calendar_code that pointed at a real calendar when set, whose
    // target was since removed -- regenerate() must surface that as a
    // clear error rather than crash, which is what this exercises.
    auto base = generate_synthetic_calendar(gctx);
    base.source = "user";
    base.is_editable = true;

    auto derived = generate_synthetic_calendar(gctx);
    derived.source = "user";
    derived.is_editable = true;
    derived.base_calendar_code = base.code;

    cal_repo.write(h.context(), {base});
    cal_repo.write(h.context(), {derived});
    cal_repo.remove(h.context(), base.code);

    CHECK_THROWS_AS(service.regenerate(derived.code), std::runtime_error);
}
