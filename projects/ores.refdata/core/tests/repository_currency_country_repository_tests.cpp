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
#include "ores.refdata.api/domain/currency_country.hpp"
#include "ores.refdata.api/domain/currency_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/country_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.core/repository/country_repository.hpp"
#include "ores.refdata.core/repository/currency_country_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::currency_country;
using ores::refdata::repository::currency_country_repository;
using ores::refdata::repository::currency_repository;
using ores::refdata::repository::country_repository;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository][currency_country]");

// Total number of test cases that write currency_country records. Each
// test uses a different slice of the fictional currencies/countries
// lists to avoid interfering with records written by other tests in
// the same shared tenant database.
constexpr std::size_t total_slots = 10;

currency_country make_currency_country(scoped_database_helper& h,
                                       const std::string& currency_iso_code,
                                       const std::string& country_alpha2_code) {
    currency_country cc;
    cc.tenant_id = h.tenant_id().to_string();
    cc.currency_iso_code = currency_iso_code;
    cc.country_alpha2_code = country_alpha2_code;
    cc.modified_by = h.db_user();
    cc.change_reason_code = "system.test";
    cc.change_commentary = "Synthetic test data";
    cc.performed_by = h.db_user();
    return cc;
}

}

TEST_CASE("write_single_currency_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    country_repository cty_repo;
    currency_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto countries = generate_fictional_countries(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[0]});
    cty_repo.write(h.context(), {countries[0]});

    auto cc = make_currency_country(h, currencies[0].iso_code, countries[0].alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Currency country: " << cc;
    CHECK_NOTHROW(repo.write(cc));
}

TEST_CASE("write_multiple_currency_countries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    country_repository cty_repo;
    currency_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto countries = generate_fictional_countries(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[1]});
    std::vector<ores::refdata::domain::country> test_countries = {countries[1], countries[2]};
    cty_repo.write(h.context(), test_countries);

    std::vector<currency_country> ccs;
    for (const auto& c : test_countries) {
        ccs.push_back(make_currency_country(h, currencies[1].iso_code, c.alpha2_code));
    }

    BOOST_LOG_SEV(lg, debug) << "Currency countries: " << ccs;
    CHECK_NOTHROW(repo.write(ccs));
}

TEST_CASE("read_latest_currency_countries_by_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    country_repository cty_repo;
    currency_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto countries = generate_fictional_countries(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[3]});
    cty_repo.write(h.context(), {countries[3]});

    auto cc = make_currency_country(h, currencies[3].iso_code, countries[3].alpha2_code);
    repo.write(cc);

    auto read_ccs = repo.read_latest_by_currency(currencies[3].iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read currency countries: " << read_ccs;

    REQUIRE(!read_ccs.empty());
    bool found = false;
    for (const auto& r : read_ccs) {
        if (r.country_alpha2_code == countries[3].alpha2_code) {
            found = true;
            break;
        }
    }
    CHECK(found);
}

TEST_CASE("read_latest_currency_countries_by_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    country_repository cty_repo;
    currency_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto countries = generate_fictional_countries(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[4]});
    cty_repo.write(h.context(), {countries[4]});

    auto cc = make_currency_country(h, currencies[4].iso_code, countries[4].alpha2_code);
    repo.write(cc);

    auto read_ccs = repo.read_latest_by_country(countries[4].alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Read currency countries: " << read_ccs;

    REQUIRE(read_ccs.size() >= 1);
    CHECK(read_ccs[0].currency_iso_code == currencies[4].iso_code);
}

TEST_CASE("remove_currency_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    country_repository cty_repo;
    currency_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto countries = generate_fictional_countries(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[5]});
    cty_repo.write(h.context(), {countries[5]});

    auto cc = make_currency_country(h, currencies[5].iso_code, countries[5].alpha2_code);
    repo.write(cc);

    CHECK_NOTHROW(repo.remove(currencies[5].iso_code, countries[5].alpha2_code));

    auto read_ccs = repo.read_latest_by_country(countries[5].alpha2_code);
    CHECK(read_ccs.empty());
}

TEST_CASE("remove_by_currency_currency_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    currency_repository ccy_repo;
    country_repository cty_repo;
    currency_country_repository repo(h.context());

    auto gctx = ores::testing::make_generation_context(h);
    auto currencies = generate_fictional_currencies(total_slots, gctx);
    auto countries = generate_fictional_countries(total_slots, gctx);
    ccy_repo.write(h.context(), {currencies[6]});
    cty_repo.write(h.context(), {countries[6]});

    auto cc = make_currency_country(h, currencies[6].iso_code, countries[6].alpha2_code);
    repo.write(cc);

    CHECK_NOTHROW(repo.remove_by_currency(currencies[6].iso_code));

    auto read_ccs = repo.read_latest_by_currency(currencies[6].iso_code);
    CHECK(read_ccs.empty());
}

TEST_CASE("read_nonexistent_currency_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    currency_country_repository repo(h.context());

    auto read_ccs = repo.read_latest_by_currency("ZZZ");
    CHECK(read_ccs.empty());
}
