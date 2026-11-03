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
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.api/generators/market_observation_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/observation_reader.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "tick_handler_support.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

// Reads stored observations through the reader as a backtest will: a series and a window
// of days, every point each day with its coordinate. Each test makes its own series, so it
// reads only what it wrote.

namespace observation_reader_test {

using namespace std::chrono;
using ores::marketdata::service::observation_reader;

constexpr year_month_day day_of(int y, unsigned m, unsigned d) {
    return year_month_day{year{y}, month{m}, std::chrono::day{d}};
}

sys_seconds at(year_month_day day, int hour) {
    return sys_days{day} + hours(hour);
}

struct observation_fixture {
    ores::testing::scoped_database_helper db;
    boost::uuids::uuid party;
    ores::database::context owner;

    observation_fixture()
        : party(ores::marketdata::tests::system_party_of_the_test_tenant(db))
        , owner(db.context().with_party(db.tenant_id(), party, {party}, db.db_user())) {}

    /// A fresh FX option series of EUR/USD; its points add an expiry and a strike label.
    static std::string series_uri() {
        const auto tag = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
        return "oresmd://fx/EUR?type=series&instrument=fx_option&quote=rate_lnvol&ccy=USD&tag=" +
               tag;
    }

    static std::string point(const std::string& series, const std::string& label) {
        return series + "&expiry=1Y&strike_label=" + label;
    }

    boost::uuids::uuid series(const std::string& uri, const std::string& provider) {
        auto generation = ores::testing::make_generation_context(db);
        auto row = ores::marketdata::generators::generate_synthetic_market_series(generation);
        row.party_id = party;
        row.oresmd_uri = uri;
        row.provider_code = provider;
        ores::marketdata::repository::market_series_repository{}.write(db.context(), row);
        return row.id;
    }

    void observe(const boost::uuids::uuid& series_id,
                 system_clock::time_point when,
                 const std::string& coordinate,
                 const std::string& value) {
        auto generation = ores::testing::make_generation_context(db);
        auto row = ores::marketdata::generators::generate_synthetic_market_observation(generation);
        row.party_id = party;
        row.series_id = series_id;
        row.observation_datetime = when;
        row.oresmd_uri = coordinate;
        row.value = value;
        row.source = "derived";
        ores::marketdata::repository::market_observation_repository{}.write(db.context(), row);
    }

    ores::marketdata::messaging::get_observation_window_response
    window(const std::string& uri, year_month_day to, int days, const std::string& provider = {}) {
        return observation_reader::window(
            owner, {.item = uri, .provider_code = provider, .to = to, .days = days});
    }
};

}

using observation_reader_test::at;
using observation_reader_test::day_of;
using observation_reader_test::observation_fixture;
using ores::marketdata::service::observation_reader;
using ores::utility::domain::outcome;

TEST_CASE("a_window_returns_every_point_of_each_day_oldest_first_and_by_coordinate",
          "[observation_reader][integration]") {
    observation_fixture f;
    const auto uri = observation_fixture::series_uri();
    const auto s = f.series(uri, "composite");
    const auto atm = observation_fixture::point(uri, "ATM");
    const auto rr = observation_fixture::point(uri, "25RR");
    f.observe(s, at(day_of(2026, 10, 21), 17), rr, "-0.004");
    f.observe(s, at(day_of(2026, 10, 21), 17), atm, "0.081");
    f.observe(s, at(day_of(2026, 10, 20), 17), atm, "0.080");
    f.observe(s, at(day_of(2026, 10, 18), 17), atm, "0.079");

    const auto r = f.window(uri, day_of(2026, 10, 21), 3);
    REQUIRE(r.result.outcome == outcome::ok);
    CHECK(r.provider_code == "composite");
    CHECK(r.days == 3);
    REQUIRE(r.used == 3);
    CHECK(r.observed_at[0] == at(day_of(2026, 10, 20), 17));
    CHECK(r.coordinates == std::vector<std::string>{atm, rr, atm});
    CHECK(r.values == std::vector<std::string>{"0.080", "-0.004", "0.081"});
    CHECK(r.sources[0] == "derived");
}

TEST_CASE("a_window_reads_its_last_day_to_the_end_and_nothing_after",
          "[observation_reader][integration]") {
    observation_fixture f;
    const auto uri = observation_fixture::series_uri();
    const auto s = f.series(uri, "composite");
    const auto atm = observation_fixture::point(uri, "ATM");
    f.observe(s, at(day_of(2026, 10, 21), 0), atm, "0.070");
    f.observe(s,
              std::chrono::sys_days{day_of(2026, 10, 21)} + std::chrono::hours(23) +
                  std::chrono::minutes(59),
              atm,
              "0.071");
    f.observe(s, at(day_of(2026, 10, 22), 0), atm, "0.072");

    const auto r = f.window(uri, day_of(2026, 10, 21), 1);
    REQUIRE(r.result.outcome == outcome::ok);
    CHECK(r.values == std::vector<std::string>{"0.070", "0.071"});
}

TEST_CASE("a_correction_stands_in_for_the_value_it_replaced", "[observation_reader][integration]") {
    observation_fixture f;
    const auto uri = observation_fixture::series_uri();
    const auto s = f.series(uri, "composite");
    const auto atm = observation_fixture::point(uri, "ATM");
    f.observe(s, at(day_of(2026, 10, 21), 17), atm, "0.081");
    f.observe(s, at(day_of(2026, 10, 21), 17), atm, "0.083");

    const auto r = f.window(uri, day_of(2026, 10, 21), 1);
    REQUIRE(r.result.outcome == outcome::ok);
    CHECK(r.values == std::vector<std::string>{"0.083"});
}

TEST_CASE("a_window_of_another_provider_reads_nothing_of_this_one",
          "[observation_reader][integration]") {
    observation_fixture f;
    const auto uri = observation_fixture::series_uri();
    const auto s = f.series(uri, "composite");
    f.observe(s, at(day_of(2026, 10, 21), 17), observation_fixture::point(uri, "ATM"), "0.081");
    const auto r = f.window(uri, day_of(2026, 10, 21), 5, "bloomberg");
    CHECK(r.result.outcome != outcome::ok);
    CHECK(r.used == 0);
}

TEST_CASE("an_empty_or_too_long_window_is_refused", "[observation_reader][integration]") {
    observation_fixture f;
    const auto uri = observation_fixture::series_uri();
    f.series(uri, "composite");
    CHECK(f.window(uri, day_of(2026, 10, 21), 0).result.outcome != outcome::ok);
    CHECK(f.window(uri, day_of(2026, 10, 21), observation_reader::max_days + 1).result.outcome !=
          outcome::ok);
}

TEST_CASE("a_window_holding_more_than_the_ceiling_is_refused_rather_than_cut",
          "[observation_reader][integration]") {
    observation_fixture f;
    const auto uri = observation_fixture::series_uri();
    const auto s = f.series(uri, "composite");
    std::vector<ores::marketdata::domain::market_observation> rows;
    auto generation = ores::testing::make_generation_context(f.db);
    for (std::size_t i = 0; i <= observation_reader::max_observations; ++i) {
        auto row = ores::marketdata::generators::generate_synthetic_market_observation(generation);
        row.party_id = f.party;
        row.series_id = s;
        row.observation_datetime =
            at(day_of(2026, 10, 21), 0) + std::chrono::seconds(static_cast<int>(i));
        row.oresmd_uri = observation_fixture::point(uri, "ATM");
        row.value = "0.08";
        rows.push_back(row);
    }
    ores::marketdata::repository::market_observation_repository{}.write(f.db.context(), rows);
    const auto r = f.window(uri, day_of(2026, 10, 21), 1);
    CHECK(r.result.outcome != outcome::ok);
    CHECK(r.used == 0);
}
