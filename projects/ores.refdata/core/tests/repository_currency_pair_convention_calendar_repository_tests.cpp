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
#include "ores.refdata.api/domain/currency_pair_convention_calendar.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_calendar_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/calendar_generator.hpp"
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.api/generators/currency_pair_convention_generator.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_calendar_repository.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::currency_pair_convention_calendar;
using ores::refdata::repository::currency_pair_convention_calendar_repository;
using ores::refdata::repository::currency_pair_convention_repository;
using ores::refdata::repository::calendar_repository;
using ores::refdata::repository::country_repository;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository][currency_pair_convention_calendar]");

// Total number of test cases that write currency_pair_convention_calendar
// records. Each test uses a different slice of the synthetic
// conventions/calendars lists to avoid interfering with records
// written by other tests in the same shared tenant database.
constexpr std::size_t total_slots = 12;

currency_pair_convention_calendar make_pair_convention_calendar(scoped_database_helper& h,
                                                                const std::string& pair_code,
                                                                const std::string& calendar_code) {
    currency_pair_convention_calendar pcc;
    pcc.tenant_id = h.tenant_id().to_string();
    pcc.pair_code = pair_code;
    pcc.calendar_code = calendar_code;
    pcc.modified_by = h.db_user();
    pcc.change_reason_code = "system.test";
    pcc.change_commentary = "Synthetic test data";
    pcc.performed_by = h.db_user();
    return pcc;
}

// Every synthetic calendar carries the ZZ sentinel as its country_code
// (calendar_generator.cpp), and the calendars table's insert trigger
// validates country_code against the countries table -- so the ZZ row
// must exist before any calendar can be written.
void write_zz_country_sentinel(scoped_database_helper& h,
                               ores::utility::generation::generation_context& gctx) {
    country_repository cty_repo;
    cty_repo.write(h.context(), {generate_country_sentinel(gctx)});
}

}

TEST_CASE("write_single_currency_pair_convention_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_pair_convention_repository conv_repo;
    calendar_repository cal_repo;
    currency_pair_convention_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto conventions = generate_synthetic_currency_pair_conventions(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    conv_repo.write(h.context(), {conventions[0]});
    cal_repo.write(h.context(), {calendars[0]});

    auto pcc = make_pair_convention_calendar(h, conventions[0].pair_code, calendars[0].code);
    BOOST_LOG_SEV(lg, debug) << "Currency pair convention calendar: " << pcc;
    CHECK_NOTHROW(repo.write(pcc));
}

TEST_CASE("write_multiple_currency_pair_convention_calendars", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_pair_convention_repository conv_repo;
    calendar_repository cal_repo;
    currency_pair_convention_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto conventions = generate_synthetic_currency_pair_conventions(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    conv_repo.write(h.context(), {conventions[1]});
    std::vector<ores::refdata::domain::calendar> test_calendars = {calendars[1], calendars[2]};
    cal_repo.write(h.context(), test_calendars);

    std::vector<currency_pair_convention_calendar> pccs;
    for (const auto& c : test_calendars) {
        pccs.push_back(make_pair_convention_calendar(h, conventions[1].pair_code, c.code));
    }

    BOOST_LOG_SEV(lg, debug) << "Currency pair convention calendars: " << pccs;
    CHECK_NOTHROW(repo.write(pccs));
}

TEST_CASE("read_latest_currency_pair_convention_calendars_by_pair", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_pair_convention_repository conv_repo;
    calendar_repository cal_repo;
    currency_pair_convention_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto conventions = generate_synthetic_currency_pair_conventions(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    conv_repo.write(h.context(), {conventions[3]});
    cal_repo.write(h.context(), {calendars[3]});

    auto pcc = make_pair_convention_calendar(h, conventions[3].pair_code, calendars[3].code);
    repo.write(pcc);

    auto read_pccs = repo.read_latest_by_pair(conventions[3].pair_code);
    BOOST_LOG_SEV(lg, debug) << "Read currency pair convention calendars: " << read_pccs;

    REQUIRE(!read_pccs.empty());
    bool found = false;
    for (const auto& r : read_pccs) {
        if (r.calendar_code == calendars[3].code) {
            found = true;
            break;
        }
    }
    CHECK(found);
}

// Proves the AdvanceCalendar-style multi-calendar case (a convention linked
// to more than one calendar, joined at the ORE XML export boundary -- see
// conventions_mapper::reverse_fx, which never touches this table) is
// correctly resolvable at export time from the junction table alone: read
// back exactly the set of calendars written, no more, no less.
TEST_CASE("read_latest_currency_pair_convention_calendars_returns_full_set_for_pair", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_pair_convention_repository conv_repo;
    calendar_repository cal_repo;
    currency_pair_convention_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto conventions = generate_synthetic_currency_pair_conventions(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    conv_repo.write(h.context(), {conventions[7]});
    std::vector<ores::refdata::domain::calendar> test_calendars = {calendars[7], calendars[8]};
    cal_repo.write(h.context(), test_calendars);

    std::vector<currency_pair_convention_calendar> pccs;
    for (const auto& c : test_calendars)
        pccs.push_back(make_pair_convention_calendar(h, conventions[7].pair_code, c.code));
    repo.write(pccs);

    auto read_pccs = repo.read_latest_by_pair(conventions[7].pair_code);
    BOOST_LOG_SEV(lg, debug) << "Read currency pair convention calendars: " << read_pccs;

    std::vector<std::string> read_codes;
    read_codes.reserve(read_pccs.size());
    for (const auto& r : read_pccs)
        read_codes.push_back(r.calendar_code);
    std::ranges::sort(read_codes);

    std::vector<std::string> expected_codes = {calendars[7].code, calendars[8].code};
    std::ranges::sort(expected_codes);

    CHECK(read_codes == expected_codes);
}

TEST_CASE("read_latest_currency_pair_convention_calendars_by_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_pair_convention_repository conv_repo;
    calendar_repository cal_repo;
    currency_pair_convention_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto conventions = generate_synthetic_currency_pair_conventions(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    conv_repo.write(h.context(), {conventions[4]});
    cal_repo.write(h.context(), {calendars[4]});

    auto pcc = make_pair_convention_calendar(h, conventions[4].pair_code, calendars[4].code);
    repo.write(pcc);

    auto read_pccs = repo.read_latest_by_calendar(calendars[4].code);
    BOOST_LOG_SEV(lg, debug) << "Read currency pair convention calendars: " << read_pccs;

    REQUIRE(read_pccs.size() >= 1);
    CHECK(read_pccs[0].pair_code == conventions[4].pair_code);
}

TEST_CASE("remove_currency_pair_convention_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_pair_convention_repository conv_repo;
    calendar_repository cal_repo;
    currency_pair_convention_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto conventions = generate_synthetic_currency_pair_conventions(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    conv_repo.write(h.context(), {conventions[5]});
    cal_repo.write(h.context(), {calendars[5]});

    auto pcc = make_pair_convention_calendar(h, conventions[5].pair_code, calendars[5].code);
    repo.write(pcc);

    CHECK_NOTHROW(repo.remove(conventions[5].pair_code, calendars[5].code));

    auto read_pccs = repo.read_latest_by_calendar(calendars[5].code);
    CHECK(read_pccs.empty());
}

TEST_CASE("remove_by_pair_currency_pair_convention_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_pair_convention_repository conv_repo;
    calendar_repository cal_repo;
    currency_pair_convention_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto conventions = generate_synthetic_currency_pair_conventions(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    conv_repo.write(h.context(), {conventions[6]});
    cal_repo.write(h.context(), {calendars[6]});

    auto pcc = make_pair_convention_calendar(h, conventions[6].pair_code, calendars[6].code);
    repo.write(pcc);

    CHECK_NOTHROW(repo.remove_by_pair(conventions[6].pair_code));

    auto read_pccs = repo.read_latest_by_pair(conventions[6].pair_code);
    CHECK(read_pccs.empty());
}

TEST_CASE("read_nonexistent_currency_pair_convention_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    currency_pair_convention_calendar_repository repo(h.context());

    auto read_pccs = repo.read_latest_by_pair("ZZZ/ZZZ");
    CHECK(read_pccs.empty());
}
