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
#include "ores.marketdata.api/generators/market_data_mapping_generator.hpp"
#include "ores.marketdata.api/generators/market_data_vendor_instrument_generator.hpp"
#include "ores.marketdata.api/generators/market_fixing_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_data_mapping_repository.hpp"
#include "ores.marketdata.core/repository/market_data_vendor_instrument_repository.hpp"
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/fixing_reader.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "tick_handler_support.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

// Reads stored fixings through the reader as the calibration will: by item and date, with
// where and when each value came from. Each test makes its own items, so it reads only what
// it wrote.

namespace fixing_reader_test {

using namespace std::chrono;
using ores::marketdata::service::fixing_reader;
using ores::utility::domain::outcome;

constexpr year_month_day d(int y, unsigned m, unsigned day) {
    return year_month_day{year{y}, month{m}, std::chrono::day{day}};
}

struct reader_fixture {
    ores::testing::scoped_database_helper db;
    boost::uuids::uuid party;
    boost::uuids::uuid other;
    ores::database::context owner;

    reader_fixture()
        : party(ores::marketdata::tests::system_party_of_the_test_tenant(db))
        , other(boost::uuids::random_generator{}())
        , owner(db.context().with_party(db.tenant_id(), party, {party, other}, db.db_user())) {}

    /// A fresh EUR/USD fixing item of the publisher @p key.
    static std::string item(const std::string& key) {
        const auto tag = boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
        return "oresmd://fx/EUR?type=fixing&index=fx&source=" + key + tag + "&ccy=USD";
    }

    boost::uuids::uuid series(const std::string& uri,
                              const std::string& provider,
                              std::optional<boost::uuids::uuid> owner_party = {}) {
        auto generation = ores::testing::make_generation_context(db);
        auto row = ores::marketdata::generators::generate_synthetic_market_series(generation);
        row.party_id = owner_party.value_or(party);
        row.oresmd_uri = uri;
        row.provider_code = provider;
        ores::marketdata::repository::market_series_repository{}.write(db.context(), row);
        return row.id;
    }

    void fix(const boost::uuids::uuid& series_id,
             year_month_day date,
             const std::string& value,
             std::optional<boost::uuids::uuid> owner_party = {}) {
        auto generation = ores::testing::make_generation_context(db);
        auto row = ores::marketdata::generators::generate_synthetic_market_fixing(generation);
        row.party_id = owner_party.value_or(party);
        row.series_id = series_id;
        row.fixing_date = date;
        row.value = value;
        row.source = "test-source";
        ores::marketdata::repository::market_fixing_repository{}.insert(db.context(), row);
    }

    void map(const std::string& uri, const std::string& provider, const std::string& vendor_id) {
        auto generation = ores::testing::make_generation_context(db);
        auto vi = ores::marketdata::generators::generate_synthetic_market_data_vendor_instrument(
            generation);
        vi.provider_code = provider;
        vi.id_scheme = "iso4217";
        vi.external_id = vendor_id;
        ores::marketdata::repository::market_data_vendor_instrument_repository{}.write(db.context(),
                                                                                       vi);
        auto m = ores::marketdata::generators::generate_synthetic_market_data_mapping(generation);
        m.vendor_instrument_id = vi.id;
        m.oresmd_uri = uri;
        m.effective_to.reset();
        ores::marketdata::repository::market_data_mapping_repository{}.write(db.context(), m);
    }

    ores::marketdata::messaging::get_fixing_response
    latest(const std::string& uri, year_month_day on, const std::string& provider = {}) {
        return fixing_reader::latest(owner,
                                     {.item = uri, .provider_code = provider, .on_or_before = on});
    }

    ores::marketdata::messaging::get_fixing_window_response
    window(const std::string& uri, year_month_day to, int days) {
        return fixing_reader::window(owner,
                                     {.item = uri, .provider_code = {}, .to = to, .days = days});
    }
};

}

using fixing_reader_test::d;
using fixing_reader_test::reader_fixture;
using ores::marketdata::service::fixing_reader;
using ores::utility::domain::outcome;

TEST_CASE("the_latest_fixing_on_a_gap_is_the_one_before_it", "[fixing_reader][integration]") {
    reader_fixture f;
    const auto uri = reader_fixture::item("GAP");
    const auto s = f.series(uri, "ore");
    f.map(uri, "ore", "USD");
    f.fix(s, d(2026, 10, 20), "1.1600");
    f.fix(s, d(2026, 10, 21), "1.1550");
    f.fix(s, d(2026, 10, 23), "1.1500");

    const auto r = f.latest(uri, d(2026, 10, 22));

    CHECK(r.result.outcome == outcome::ok);
    REQUIRE(r.fixing_date.has_value());
    CHECK(*r.fixing_date == d(2026, 10, 21));
    CHECK(r.value == "1.1550");
    CHECK(r.source == "test-source");
    CHECK(r.provider_code == "ore");
    CHECK(r.vendor_scheme == "iso4217");
    CHECK(r.vendor_id == "USD");
    REQUIRE(r.received_at.has_value());
    CHECK(*r.received_at <= std::chrono::system_clock::now());
}

TEST_CASE("a_date_before_the_first_fixing_finds_none", "[fixing_reader][integration]") {
    reader_fixture f;
    const auto uri = reader_fixture::item("EARLY");
    const auto s = f.series(uri, "ore");
    f.fix(s, d(2026, 10, 20), "1.1600");

    const auto r = f.latest(uri, d(2026, 10, 19));

    CHECK(r.result.outcome == outcome::ok);
    CHECK_FALSE(r.fixing_date.has_value());
    CHECK(r.provider_code == "ore");
    CHECK(r.vendor_id.empty());
}

TEST_CASE("a_window_skips_the_days_without_a_fixing_and_says_how_many_it_used",
          "[fixing_reader][integration]") {
    reader_fixture f;
    const auto uri = reader_fixture::item("WIN");
    const auto s = f.series(uri, "ore");
    f.fix(s, d(2026, 10, 19), "1.1700");
    f.fix(s, d(2026, 10, 20), "1.1600");
    f.fix(s, d(2026, 10, 21), "1.1550");
    f.fix(s, d(2026, 10, 23), "1.1500");

    const auto r = f.window(uri, d(2026, 10, 23), 4);

    CHECK(r.result.outcome == outcome::ok);
    CHECK(r.days == 4);
    CHECK(r.used == 3);
    CHECK(r.dates == std::vector{d(2026, 10, 20), d(2026, 10, 21), d(2026, 10, 23)});
    CHECK(r.values == std::vector<std::string>{"1.1600", "1.1550", "1.1500"});
    CHECK(r.sources.size() == 3);
    CHECK(r.received_at.size() == 3);
}

// One party holds one series of an index, so two providers of one item meet across the
// parties a session sees.
TEST_CASE("two_providers_of_one_item_are_ambiguous_until_the_request_names_one",
          "[fixing_reader][integration]") {
    reader_fixture f;
    const auto uri = reader_fixture::item("BOTH");
    f.fix(f.series(uri, "ore"), d(2026, 10, 23), "1.1500");
    f.fix(f.series(uri, "bloomberg", f.other), d(2026, 10, 23), "1.1497", f.other);

    const auto either = f.latest(uri, d(2026, 10, 23));
    CHECK(either.result.code == "series_ambiguous");
    CHECK_FALSE(either.fixing_date.has_value());

    const auto a = f.latest(uri, d(2026, 10, 23), "ore");
    const auto b = f.latest(uri, d(2026, 10, 23), "bloomberg");
    CHECK(a.provider_code == "ore");
    CHECK(a.value == "1.1500");
    CHECK(b.provider_code == "bloomberg");
    CHECK(b.value == "1.1497");
}

TEST_CASE("one_provider_under_two_parties_is_refused_and_the_refusal_names_both",
          "[fixing_reader][integration]") {
    reader_fixture f;
    const auto uri = reader_fixture::item("TWOPARTY");
    f.fix(f.series(uri, "ore"), d(2026, 10, 23), "1.1500");
    f.fix(f.series(uri, "ore", f.other), d(2026, 10, 23), "1.1501", f.other);

    const auto r = f.latest(uri, d(2026, 10, 23), "ore");

    CHECK(r.result.code == "series_ambiguous");
    CHECK(r.result.message.find("2 parties; providers ore") != std::string::npos);
}

TEST_CASE("an_item_that_names_no_series_is_refused_as_not_found", "[fixing_reader][integration]") {
    reader_fixture f;

    const auto r = f.latest(reader_fixture::item("NONE"), d(2026, 10, 23));

    CHECK(r.result.outcome == outcome::missing);
    CHECK(r.result.code == "not_found");
}

TEST_CASE("a_window_over_the_ceiling_or_of_no_days_is_refused", "[fixing_reader][integration]") {
    reader_fixture f;
    const auto uri = reader_fixture::item("CEIL");
    f.series(uri, "ore");

    CHECK(f.window(uri, d(2026, 10, 23), fixing_reader::max_days + 1).result.code ==
          "range_too_large");
    CHECK(f.window(uri, d(2026, 10, 23), 0).result.code == "range_inverted");
}

TEST_CASE("a_session_with_no_party_is_refused", "[fixing_reader][integration]") {
    reader_fixture f;
    const auto uri = reader_fixture::item("NOPARTY");
    f.series(uri, "ore");

    const auto r = fixing_reader::latest(
        f.db.context(), {.item = uri, .provider_code = {}, .on_or_before = d(2026, 10, 23)});

    CHECK(r.result.code == "party_required");
}
