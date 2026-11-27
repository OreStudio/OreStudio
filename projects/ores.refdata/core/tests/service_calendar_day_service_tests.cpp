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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.refdata.api/generators/calendar_generator.hpp"
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/calendar_exception_repository.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include "ores.refdata.core/repository/calendar_rule_repository.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/service/calendar_day_service.hpp"
#include "ores.refdata.core/service/calendar_materialisation_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

using namespace ores::refdata::generators;
using ores::refdata::domain::calendar_exception;
using ores::refdata::domain::calendar_rule;
using ores::refdata::repository::calendar_exception_repository;
using ores::refdata::repository::calendar_repository;
using ores::refdata::repository::calendar_rule_repository;
using ores::refdata::repository::country_repository;
using ores::refdata::repository::party_repository;
using ores::refdata::service::calendar_day_service;
using ores::refdata::service::calendar_materialisation_service;
using ores::testing::scoped_database_helper;

namespace {

void calendar_day_write_zz_country_sentinel(scoped_database_helper& h,
                                            ores::utility::generation::generation_context& gctx) {
    country_repository cty_repo;
    cty_repo.write(h.context(), {generate_country_sentinel(gctx)});
}

bool calendar_day_open_on(const std::vector<ores::refdata::domain::calendar_date>& days,
                          std::chrono::year_month_day date) {
    for (const auto& day : days)
        if (day.date == date)
            return day.is_business_day;
    return false;
}

std::chrono::year_month_day calendar_day_next_year_first_monday(unsigned occurrence) {
    using namespace std::chrono;
    const auto today = year_month_day{floor<days>(system_clock::now())};
    const year_month_weekday ymw{
        today.year() + years{1}, June, weekday_indexed{Monday, occurrence}};
    return year_month_day{sys_days{ymw}};
}

/// The days of a calendar every party shares, with the installation closing the
/// second Monday of June and no party opening it.
calendar_rule calendar_day_make_rule(scoped_database_helper& h, const std::string& calendar_code) {
    static boost::uuids::random_generator gen;
    calendar_rule r;
    r.id = gen();
    r.tenant_id = h.tenant_id();
    r.calendar_code = calendar_code;
    r.kind = "nth_weekday_of_month";
    r.month = 6;
    r.weekday = 1;
    r.occurrence = 1;
    r.shift = "none";
    r.modified_by = h.db_user();
    r.performed_by = h.db_user();
    r.change_reason_code = "system.test";
    r.change_commentary = "Synthetic test data";
    return r;
}

calendar_exception calendar_day_make_exception(scoped_database_helper& h,
                                               const std::string& calendar_code,
                                               std::chrono::year_month_day date,
                                               bool is_business_day,
                                               std::optional<boost::uuids::uuid> party_id) {
    static boost::uuids::random_generator gen;
    calendar_exception e;
    e.id = gen();
    e.tenant_id = h.tenant_id();
    e.calendar_code = calendar_code;
    e.exception_date = date;
    e.is_business_day = is_business_day;
    e.party_id = party_id;
    e.description = "Synthetic test exception";
    e.modified_by = h.db_user();
    e.performed_by = h.db_user();
    e.change_reason_code = "system.test";
    e.change_commentary = "Synthetic test data";
    return e;
}

/// A party this tenant already holds, so a test never takes the tenant's one
/// root-party slot from a suite that expects it.
std::optional<boost::uuids::uuid> calendar_day_existing_party(scoped_database_helper& h) {
    party_repository party_repo;
    for (const auto& party : party_repo.read_latest(h.context()))
        if (party.tenant_id == h.tenant_id())
            return party.id;
    return std::nullopt;
}

ores::refdata::domain::party
calendar_day_make_party(scoped_database_helper& h,
                        ores::utility::generation::generation_context& gctx,
                        std::optional<boost::uuids::uuid> parent) {
    party_repository party_repo;
    auto party = generate_synthetic_party(gctx);
    party.tenant_id = h.tenant_id();
    if (parent)
        party.parent_party_id = parent;
    else
        // The tenant holds one root party, and other suites write it. A root
        // this test needs is the System category, which the index leaves out.
        party.party_category = "System";
    party.change_reason_code = "system.test";
    party_repo.write(h.context(), party);
    return party;
}

} // namespace

TEST_CASE("a_party_reads_its_own_exception_over_the_days_the_installation_states",
          "[service][calendar_day]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    calendar_repository cal_repo;
    calendar_rule_repository rule_repo;
    calendar_exception_repository exc_repo;

    auto gctx = ores::testing::make_generation_context(h);
    calendar_day_write_zz_country_sentinel(h, gctx);

    auto cal = generate_synthetic_calendar(gctx);
    cal.source = "user";
    cal.is_editable = true;
    cal_repo.write(h.context(), {cal});
    rule_repo.write(h.context(), {calendar_day_make_rule(h, cal.code)});

    const auto rule_holiday = calendar_day_next_year_first_monday(1);
    const auto installation_holiday = calendar_day_next_year_first_monday(2);
    const auto plain_business_day = calendar_day_next_year_first_monday(3);

    const auto reopened_by_one_party =
        calendar_day_make_exception(h, cal.code, installation_holiday, false, std::nullopt);
    exc_repo.write(h.context(), {reopened_by_one_party});

    auto parent = calendar_day_existing_party(h);
    if (!parent)
        parent = calendar_day_make_party(h, gctx, std::nullopt).id;
    const auto first = calendar_day_make_party(h, gctx, parent);
    const auto second = calendar_day_make_party(h, gctx, parent);
    exc_repo.write(
        h.context(),
        {calendar_day_make_exception(h, cal.code, installation_holiday, true, first.id)});

    calendar_materialisation_service materialiser(h.context());
    const auto written = materialiser.regenerate(cal.code, rule_holiday.year());
    REQUIRE(written > 0);

    const auto first_ctx = h.context().with_party(h.tenant_id(), first.id, {first.id}, h.db_user());
    const auto second_ctx =
        h.context().with_party(h.tenant_id(), second.id, {second.id}, h.db_user());

    const auto first_days = calendar_day_service(first_ctx).days(cal.code);
    const auto second_days = calendar_day_service(second_ctx).days(cal.code);
    const auto no_party_days = calendar_day_service(h.context()).days(cal.code);

    REQUIRE(!first_days.empty());
    REQUIRE(!second_days.empty());

    // The party states the day itself, and only its day moves.
    CHECK(calendar_day_open_on(first_days, installation_holiday));
    CHECK_FALSE(calendar_day_open_on(second_days, installation_holiday));
    CHECK_FALSE(calendar_day_open_on(no_party_days, installation_holiday));

    // Every other day is the installation's answer for every reader.
    CHECK_FALSE(calendar_day_open_on(first_days, rule_holiday));
    CHECK_FALSE(calendar_day_open_on(second_days, rule_holiday));
    CHECK(calendar_day_open_on(first_days, plain_business_day));
    CHECK(calendar_day_open_on(second_days, plain_business_day));
    CHECK(calendar_day_open_on(no_party_days, plain_business_day));
}

TEST_CASE("a_calendar_of_the_installation_is_built_once_and_read_by_a_tenant",
          "[service][calendar_day]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    calendar_repository cal_repo;
    calendar_rule_repository rule_repo;

    auto gctx = ores::testing::make_generation_context(h);

    const auto system_ctx =
        h.context().with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user());
    {
        country_repository cty_repo;
        cty_repo.write(system_ctx, {generate_country_sentinel(gctx)});
    }

    auto shared = generate_synthetic_calendar(gctx);
    shared.tenant_id = ores::utility::uuid::tenant_id::system();
    shared.source = "user";
    shared.is_editable = true;
    cal_repo.write(system_ctx, {shared});

    auto rule = calendar_day_make_rule(h, shared.code);
    rule.tenant_id = ores::utility::uuid::tenant_id::system();
    rule_repo.write(system_ctx, {rule});

    const auto rule_holiday = calendar_day_next_year_first_monday(1);

    // A tenant reads the calendar, and its own run writes nothing for it: the
    // installation's copy is the one every tenant reads.
    calendar_materialisation_service tenant_materialiser(h.context());
    CHECK(tenant_materialiser.regenerate(shared.code, rule_holiday.year()) == 0);

    calendar_materialisation_service system_materialiser(system_ctx);
    CHECK(system_materialiser.regenerate(shared.code, rule_holiday.year()) > 0);

    const auto days = calendar_day_service(h.context()).days(shared.code);
    REQUIRE(!days.empty());
    CHECK_FALSE(calendar_day_open_on(days, rule_holiday));
    CHECK(calendar_day_open_on(days, calendar_day_next_year_first_monday(3)));
}
