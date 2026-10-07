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
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.shell/app/commands/marketdata_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <cstddef>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::commands::marketdata_commands;
using ores::shell::app::commands::stream_output;
using ores::shell::app::commands::tick_source;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.application.tests");
const std::string tags("[commands][marketdata][stream]");

const std::string tenant_id("11111111-1111-1111-1111-111111111111");
const std::string party_id("3f2504e0-4f89-11d3-9a0c-0305e82c3301");

/// The URI the codec spells for the datum of the ORE key FX/RATE/EUR/USD.
const std::string eurusd_uri("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");

// A rating transition has two coordinate fields, from_rating and to_rating, so
// its point key is more than its series key plus one token. The broken series
// subject spelling was never exercised by a one-coordinate type.
const std::string rating_series_uri(
    "oresmd://rating/AAA?type=series&instrument=rating&quote=transition_probability");
const std::string rating_point_uri("oresmd://rating/AAA?type=quote&instrument=rating"
                                   "&quote=transition_probability&from_rating=AA&to_rating=A");
const std::string
    other_rating_point_uri("oresmd://rating/BBB?type=quote&instrument=rating"
                           "&quote=transition_probability&from_rating=AA&to_rating=A");

ores::marketdata::messaging::market_tick
tick(const std::string& uri, const std::string& value, const std::string& source, int hour) {
    ores::marketdata::messaging::market_tick made;
    made.oresmd_uri = uri;
    made.value = value;
    made.source = source;
    made.observation_time =
        std::chrono::sys_days{std::chrono::year{2026} / 10 / 6} + std::chrono::hours{hour};
    return made;
}

/// One batch of ticks, and then the end of them.
class stub_tick_source final : public tick_source {
public:
    explicit stub_tick_source(std::vector<ores::marketdata::messaging::market_tick> ticks)
        : ticks_(std::move(ticks)) {}

    std::pair<std::vector<ores::marketdata::messaging::market_tick>, bool> next() override {
        auto batch = std::move(ticks_);
        ticks_.clear();
        return {std::move(batch), false};
    }

private:
    std::vector<ores::marketdata::messaging::market_tick> ticks_;
};

}

TEST_CASE("marketdata_commands registers the stream verb", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    marketdata_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("marketdata ");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"marketdata stream"}) !=
          completions.end());
}

TEST_CASE("a_quote_uri_resolves_to_the_datum_key_subject", tags) {
    auto lg(make_logger(test_suite));

    const auto watch = marketdata_commands::stream_subjects(tenant_id, party_id, eurusd_uri);

    REQUIRE(watch.has_value());
    CHECK(watch->subjects == std::vector<std::string>{"marketdata.v1.ops.market_tick." + tenant_id +
                                                      "." + party_id + ".fx.rate.eur.usd"});
    CHECK_FALSE(watch->series.has_value());
}

TEST_CASE("a_series_uri_resolves_to_the_partys_whole_tick_subtree", tags) {
    auto lg(make_logger(test_suite));

    const auto watch = marketdata_commands::stream_subjects(tenant_id, party_id, rating_series_uri);

    REQUIRE(watch.has_value());
    // A series has no ORE key of its own, and a rating's two coordinates sit
    // around its identity field, so no single wildcard subject covers exactly
    // one series; the watch takes the whole subtree and filters.
    CHECK(watch->subjects == std::vector<std::string>{"marketdata.v1.ops.market_tick." + tenant_id +
                                                      "." + party_id + ".>"});
    REQUIRE(watch->series.has_value());
    CHECK(watch->series->is_series());
}

TEST_CASE("a_series_filter_keeps_only_the_points_of_the_series", tags) {
    auto lg(make_logger(test_suite));

    const auto series = ores::marketdata::datum::oresmd_uri_codec::read(rating_series_uri);
    REQUIRE(series.has_value());

    stub_tick_source source({tick(rating_point_uri, "0.01", "synthetic.rating", 9),
                             tick(other_rating_point_uri, "0.02", "synthetic.rating", 9)});
    std::ostringstream out;

    const auto output = marketdata_commands::run_stream(
        source,
        [] { return false; },
        "",
        out,
        [&series](const auto& t) { return marketdata_commands::tick_in_series(t, *series); });

    // The matching point is kept and the other series' point is dropped.
    CHECK(output.count == 1);
    REQUIRE(output.screen_lines.size() == 1);
    CHECK(output.screen_lines[0] ==
          "2026-10-06 09:00:00Z  " + rating_point_uri + "  0.01  synthetic.rating");
    CHECK(output.rows.size() == 2);
}

TEST_CASE("an_unparseable_uri_is_refused_with_the_codecs_message", tags) {
    auto lg(make_logger(test_suite));

    const auto subjects = marketdata_commands::stream_subjects(
        tenant_id, party_id, "oresmd://fx/EUR?type=quote&instrument=fx_spot");

    REQUIRE_FALSE(subjects.has_value());
    // The codec quotes the URI it refused and says what the grammar did not
    // admit; the command prints that text rather than a subject error.
    CHECK(subjects.error().find("oresmd://fx/EUR?type=quote&instrument=fx_spot") !=
          std::string::npos);
    CHECK(subjects.error().find("quote") != std::string::npos);
}

TEST_CASE("two_ticks_render_the_expected_screen_lines_and_csv_rows", tags) {
    auto lg(make_logger(test_suite));

    stub_tick_source source({tick(eurusd_uri, "1.0812", "synthetic.eurusd", 9),
                             tick(eurusd_uri, "1.0815", "synthetic.eurusd", 10)});
    std::ostringstream out;

    const auto output = marketdata_commands::run_stream(source, [] { return false; }, "", out);

    CHECK(output.count == 2);
    REQUIRE(output.screen_lines.size() == 2);
    CHECK(output.screen_lines[0] ==
          "2026-10-06 09:00:00Z  " + eurusd_uri + "  1.0812  synthetic.eurusd");
    CHECK(output.screen_lines[1] ==
          "2026-10-06 10:00:00Z  " + eurusd_uri + "  1.0815  synthetic.eurusd");
    CHECK(out.str() == output.screen_lines[0] + "\n" + output.screen_lines[1] + "\n");

    REQUIRE(output.rows.size() == 3);
    CHECK(output.rows[0] == "observation_time,oresmd_uri,value,source");
    CHECK(output.rows[1] == "2026-10-06 09:00:00Z," + eurusd_uri + ",1.0812,synthetic.eurusd");
    CHECK(output.rows[2] == "2026-10-06 10:00:00Z," + eurusd_uri + ",1.0815,synthetic.eurusd");
}

TEST_CASE("a_csv_field_holding_a_separator_is_quoted", tags) {
    auto lg(make_logger(test_suite));

    const auto row =
        marketdata_commands::tick_row(tick(eurusd_uri, "1.0812", "synthetic, quoted", 9));

    CHECK(row == "2026-10-06 09:00:00Z," + eurusd_uri + ",1.0812,\"synthetic, quoted\"");
}

TEST_CASE("a_stream_that_receives_nothing_still_writes_the_header", tags) {
    auto lg(make_logger(test_suite));

    stub_tick_source source({});
    std::ostringstream out;

    const auto output = marketdata_commands::run_stream(source, [] { return false; }, "", out);

    CHECK(output.count == 0);
    CHECK(output.screen_lines.empty());
    REQUIRE(output.rows.size() == 1);
    CHECK(output.rows[0] == "observation_time,oresmd_uri,value,source");
    CHECK(out.str().empty());
}

TEST_CASE("an_unwritable_csv_path_is_reported", tags) {
    auto lg(make_logger(test_suite));

    stub_tick_source source({tick(eurusd_uri, "1.0812", "synthetic.eurusd", 9)});
    std::ostringstream out;
    const std::string unwritable("/no/such/directory/ticks.csv");

    const auto output =
        marketdata_commands::run_stream(source, [] { return false; }, unwritable, out);

    // The tick still reaches the screen, and the failed file is named rather
    // than reported as written.
    CHECK(output.count == 1);
    CHECK(output.csv_error == unwritable);
}
