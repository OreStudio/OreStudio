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
#include "ores.refdata.api/domain/currency_calendar.hpp"
#include "ores.refdata.api/domain/currency_calendar_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/calendar_generator.hpp"
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/currency_calendar_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::currency_calendar;
using ores::refdata::repository::currency_calendar_repository;
using ores::refdata::repository::currency_repository;
using ores::refdata::repository::calendar_repository;
using ores::refdata::repository::country_repository;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository][currency_calendar]");

// Total number of test cases that write currency_calendar records. Each
// test uses a different slice of the fictional currencies/calendars
// lists to avoid interfering with records written by other tests in
// the same shared tenant database.
constexpr std::size_t total_slots = 10;

currency_calendar make_currency_calendar(scoped_database_helper& h,
                                         const std::string& currency_iso_code,
                                         const std::string& calendar_code) {
    currency_calendar cc;
    cc.tenant_id = h.tenant_id();
    cc.currency_iso_code = currency_iso_code;
    cc.calendar_code = calendar_code;
    cc.modified_by = h.db_user();
    cc.change_reason_code = "system.test";
    cc.change_commentary = "Synthetic test data";
    cc.performed_by = h.db_user();
    return cc;
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

TEST_CASE("write_single_currency_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    calendar_repository cal_repo;
    currency_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[0]});
    cal_repo.write(h.context(), {calendars[0]});

    auto cc = make_currency_calendar(h, currencies[0].iso_code, calendars[0].code);
    BOOST_LOG_SEV(lg, debug) << "Currency calendar: " << cc;
    CHECK_NOTHROW(repo.write(cc));
}

TEST_CASE("write_multiple_currency_calendars", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    calendar_repository cal_repo;
    currency_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[1]});
    std::vector<ores::refdata::domain::calendar> test_calendars = {calendars[1], calendars[2]};
    cal_repo.write(h.context(), test_calendars);

    std::vector<currency_calendar> ccs;
    for (const auto& c : test_calendars) {
        ccs.push_back(make_currency_calendar(h, currencies[1].iso_code, c.code));
    }

    BOOST_LOG_SEV(lg, debug) << "Currency calendars: " << ccs;
    CHECK_NOTHROW(repo.write(ccs));
}

TEST_CASE("read_latest_currency_calendars_by_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    calendar_repository cal_repo;
    currency_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[3]});
    cal_repo.write(h.context(), {calendars[3]});

    auto cc = make_currency_calendar(h, currencies[3].iso_code, calendars[3].code);
    repo.write(cc);

    auto read_ccs = repo.read_latest_by_currency(currencies[3].iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read currency calendars: " << read_ccs;

    REQUIRE(!read_ccs.empty());
    bool found = false;
    for (const auto& r : read_ccs) {
        if (r.calendar_code == calendars[3].code) {
            found = true;
            break;
        }
    }
    CHECK(found);
}

TEST_CASE("read_latest_currency_calendars_by_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    calendar_repository cal_repo;
    currency_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[4]});
    cal_repo.write(h.context(), {calendars[4]});

    auto cc = make_currency_calendar(h, currencies[4].iso_code, calendars[4].code);
    repo.write(cc);

    auto read_ccs = repo.read_latest_by_calendar(calendars[4].code);
    BOOST_LOG_SEV(lg, debug) << "Read currency calendars: " << read_ccs;

    REQUIRE(read_ccs.size() >= 1);
    CHECK(read_ccs[0].currency_iso_code == currencies[4].iso_code);
}

TEST_CASE("remove_currency_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    calendar_repository cal_repo;
    currency_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[5]});
    cal_repo.write(h.context(), {calendars[5]});

    auto cc = make_currency_calendar(h, currencies[5].iso_code, calendars[5].code);
    repo.write(cc);

    CHECK_NOTHROW(repo.remove(currencies[5].iso_code, calendars[5].code));

    auto read_ccs = repo.read_latest_by_calendar(calendars[5].code);
    CHECK(read_ccs.empty());
}

TEST_CASE("remove_by_currency_currency_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    calendar_repository cal_repo;
    currency_calendar_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    write_zz_country_sentinel(h, gctx);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto calendars = generate_synthetic_calendars(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[6]});
    cal_repo.write(h.context(), {calendars[6]});

    auto cc = make_currency_calendar(h, currencies[6].iso_code, calendars[6].code);
    repo.write(cc);

    CHECK_NOTHROW(repo.remove_by_currency(currencies[6].iso_code));

    auto read_ccs = repo.read_latest_by_currency(currencies[6].iso_code);
    CHECK(read_ccs.empty());
}

TEST_CASE("read_nonexistent_currency_calendar", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    currency_calendar_repository repo(h.context());

    auto read_ccs = repo.read_latest_by_currency("ZZZ");
    CHECK(read_ccs.empty());
}
