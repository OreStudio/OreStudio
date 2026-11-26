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
#include "ores.marketdata.api/generators/market_fixing_generator.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/fixing_series_summaries.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace fixing_series_summaries_test {

using namespace std::chrono;
using ores::marketdata::messaging::list_fixing_series_summaries_request;
using ores::testing::scoped_database_helper;

/// A tag no other run states, so the series this file writes are its own.
std::string summaries_run_tag() {
    static boost::uuids::random_generator uuid;
    auto tag = boost::uuids::to_string(uuid()).substr(0, 8);
    std::ranges::transform(tag, tag.begin(), [](char c) { return c >= 'a' ? c - 'a' + 'A' : c; });
    return tag;
}

/// A fixing address of an FX pair, which states the pair and its source.
std::string fx_uri(const std::string& tag) {
    return "oresmd://fx/EUR?type=fixing&index=fx&source=" + tag + "&ccy=USD";
}

/// A fixing address of a term index, which states the index and its term.
std::string ir_uri(const std::string& tag) {
    return "oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M&source=" + tag;
}

struct seeded_series {
    std::string uri;
    boost::uuids::uuid id;
};

/// Writes one fixing series of the test tenant, and returns what it wrote.
seeded_series seed_series(scoped_database_helper& h,
                          const boost::uuids::uuid& party,
                          const std::string& uri,
                          const std::string& provider_code) {
    auto gen = ores::testing::make_generation_context(h);
    auto series = ores::marketdata::generators::generate_synthetic_market_series(gen);
    series.party_id = party;
    series.provider_code = provider_code;
    series.oresmd_uri = uri;
    series.series_subclass = "index_fixing";
    ores::marketdata::repository::market_series_repository{}.write(h.context(), series);
    return {.uri = uri, .id = series.id};
}

void seed_fixing(scoped_database_helper& h,
                 const boost::uuids::uuid& party,
                 const boost::uuids::uuid& series_id,
                 year_month_day date,
                 const std::string& value,
                 const std::string& source) {
    auto gen = ores::testing::make_generation_context(h);
    auto fixing = ores::marketdata::generators::generate_synthetic_market_fixing(gen);
    fixing.party_id = party;
    fixing.series_id = series_id;
    fixing.fixing_date = date;
    fixing.value = value;
    fixing.source = source;
    ores::marketdata::repository::market_fixing_repository{}.insert(h.context(), fixing);
}

/// Writes a party of the test tenant, which owns the series.
boost::uuids::uuid seed_party(scoped_database_helper& h) {
    auto gen = ores::testing::make_generation_context(h);
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    const auto existing = ores::refdata::repository::party_repository{}.read_latest(h.context());
    if (!existing.empty())
        party.parent_party_id = existing.front().id;
    ores::refdata::repository::party_repository{}.write(h.context(), party);
    return party.id;
}

}

using namespace fixing_series_summaries_test;

TEST_CASE("a_series_states_its_latest_day_its_count_and_what_it_fixes", "[app][fixings]") {
    scoped_database_helper h;
    const auto party = seed_party(h);
    const auto tag = summaries_run_tag();

    const auto fx = seed_series(h, party, fx_uri(tag), "bloomberg");
    seed_fixing(h, party, fx.id, year_month_day{year{2026}, October, day{21}}, "1.0840", tag);
    seed_fixing(h, party, fx.id, year_month_day{year{2026}, November, day{20}}, "1.0901", tag);
    seed_fixing(h, party, fx.id, year_month_day{year{2026}, November, day{19}}, "1.0875", tag);

    // A series that holds nothing is still one of the rows: a series that ended is a
    // fact the screen states rather than hides.
    const auto empty = seed_series(h, party, ir_uri(tag), "derived");

    const auto response = ores::marketdata::service::app::list_fixing_series_summaries(
        h.context(), list_fixing_series_summaries_request{});
    const auto find = [&](const std::string& uri) {
        return std::ranges::find_if(response.series,
                                    [&](const auto& s) { return s.oresmd_uri == uri; });
    };

    const auto fx_row = find(fx.uri);
    REQUIRE(fx_row != response.series.end());
    CHECK(fx_row->asset_class == "fx");
    CHECK(fx_row->index_label == "EUR/USD");
    CHECK(fx_row->provider_code == "bloomberg");
    CHECK(fx_row->source == tag);
    CHECK(fx_row->days == 3);
    CHECK(fx_row->latest_value == "1.0901");
    CHECK(fx_row->latest_date == year_month_day{year{2026}, November, day{20}});

    const auto ir_row = find(empty.uri);
    REQUIRE(ir_row != response.series.end());
    CHECK(ir_row->asset_class == "ir");
    CHECK(ir_row->index_label == "USD SOFR 1M");
    CHECK(ir_row->days == 0);
    CHECK(ir_row->latest_value.empty());
    CHECK_FALSE(ir_row->latest_date);
}

TEST_CASE("the_filters_name_the_asset_class_the_currency_and_the_source", "[app][fixings]") {
    scoped_database_helper h;
    const auto party = seed_party(h);
    const auto tag = summaries_run_tag();

    const auto fx = seed_series(h, party, fx_uri(tag), "bloomberg");
    seed_fixing(h, party, fx.id, year_month_day{year{2026}, November, day{20}}, "1.0901", tag);
    const auto ir = seed_series(h, party, ir_uri(tag), "derived");
    seed_fixing(h, party, ir.id, year_month_day{year{2026}, November, day{20}}, "0.039436", tag);

    const auto ask = [&](list_fixing_series_summaries_request request) {
        return ores::marketdata::service::app::list_fixing_series_summaries(h.context(), request);
    };
    const auto names = [](const auto& response, const std::string& uri) {
        return std::ranges::any_of(response.series,
                                   [&](const auto& s) { return s.oresmd_uri == uri; });
    };

    // The source is the one this run wrote under, so the pair is exactly the two rows.
    const auto this_run = ask({.source = tag});
    REQUIRE(this_run.series.size() == 2);

    const auto just_ir = ask({.asset_class = "ir"});
    CHECK(names(just_ir, ir.uri));
    CHECK_FALSE(names(just_ir, fx.uri));

    // An FX series is named by either of its currencies: the path states the base and
    // the query the quote.
    const auto eur = ask({.currency = "EUR"});
    CHECK(names(eur, fx.uri));

    const auto no_source = ask({.source = "no such source"});
    CHECK(no_source.series.empty());
}
