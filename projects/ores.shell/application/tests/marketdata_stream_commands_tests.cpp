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
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.shell/app/commands/marketdata_commands.hpp"
#include "test_suite.hpp"
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

const std::string bloomberg_feed("0f1e2d3c-4b5a-6978-8796-a5b4c3d2e1f0");
const std::string lseg_feed("1f1e2d3c-4b5a-6978-8796-a5b4c3d2e1f0");
const std::vector<ores::marketdata::messaging::watchable_feed> two_feeds{
    {"bloomberg", bloomberg_feed}, {"lseg", lseg_feed}};

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

TEST_CASE("marketdata_commands registers the stream verb", "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    marketdata_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("marketdata ");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"stream"}) !=
          completions.end());
}

TEST_CASE("a_quote_uri_watches_its_own_subject_on_each_feed", "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));
    const auto datum = ores::marketdata::datum::oresmd_uri_codec::read(eurusd_uri).value();

    const auto subjects = marketdata_commands::stream_subjects(two_feeds, eurusd_uri);

    REQUIRE(subjects.has_value());
    CHECK(*subjects == std::vector<std::string>{*ores::marketdata::domain::feed_tick_watch_subject(
                                                    "bloomberg", bloomberg_feed, datum),
                                                *ores::marketdata::domain::feed_tick_watch_subject(
                                                    "lseg", lseg_feed, datum)});
}

TEST_CASE("a_series_uri_watches_the_points_of_the_series_and_no_other",
          "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));
    using ores::marketdata::domain::subject_matches;
    const auto published = [](const std::string& uri) {
        const auto datum = ores::marketdata::datum::oresmd_uri_codec::read(uri).value();
        const auto series = ores::marketdata::datum::oresmd_uri_codec::write(
                                ores::marketdata::datum::series_of(datum))
                                .value();
        return ores::marketdata::domain::feed_tick_subject(
            "bloomberg", bloomberg_feed, datum, series);
    };

    const auto subjects = marketdata_commands::stream_subjects(two_feeds, rating_series_uri);

    REQUIRE(subjects.has_value());
    REQUIRE(subjects->size() == 2);
    CHECK(subject_matches(subjects->front(), published(rating_point_uri)));
    CHECK_FALSE(subject_matches(subjects->front(), published(other_rating_point_uri)));
    CHECK_FALSE(subject_matches(subjects->back(), published(rating_point_uri)));
}

TEST_CASE("a_party_that_may_watch_no_feed_watches_no_subject", "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));

    const auto subjects = marketdata_commands::stream_subjects({}, eurusd_uri);

    REQUIRE(subjects.has_value());
    CHECK(subjects->empty());
}

TEST_CASE("an_unparseable_uri_is_refused_with_the_codecs_message",
          "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));

    const auto subjects = marketdata_commands::stream_subjects(
        two_feeds, "oresmd://fx/EUR?type=quote&instrument=fx_spot");

    REQUIRE_FALSE(subjects.has_value());
    // The codec quotes the URI it refused and says what the grammar did not
    // admit; the command prints that text rather than a subject error.
    CHECK(subjects.error().find("oresmd://fx/EUR?type=quote&instrument=fx_spot") !=
          std::string::npos);
    CHECK(subjects.error().find("quote") != std::string::npos);
}

TEST_CASE("a_watch_drops_the_feeds_the_party_lost_and_adds_the_feeds_it_gained",
          "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));

    const auto change = marketdata_commands::change_watch({"a", "b"}, {"b", "c", "c"});

    CHECK(change.added == std::vector<std::string>{"c"});
    CHECK(change.dropped == std::vector<std::string>{"a"});
}

TEST_CASE("a_watch_whose_feeds_did_not_change_changes_nothing", "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));

    const auto change = marketdata_commands::change_watch({"a", "b"}, {"b", "a"});

    CHECK(change.added.empty());
    CHECK(change.dropped.empty());
}

TEST_CASE("two_ticks_render_the_expected_screen_lines_and_csv_rows",
          "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));

    stub_tick_source source({tick(eurusd_uri, "1.0812", "synthetic.eurusd", 9),
                             tick(eurusd_uri, "1.0815", "synthetic.eurusd", 10)});
    std::ostringstream out;

    const auto output = marketdata_commands::run_stream(source, [] { return false; }, "", out);

    CHECK(output.count == 2);
    REQUIRE(output.screen_lines.size() == 2);
    CHECK(output.screen_lines[0] ==
          "2026-10-06 09:00:00.000000Z  " + eurusd_uri + "  1.0812  synthetic.eurusd");
    CHECK(output.screen_lines[1] ==
          "2026-10-06 10:00:00.000000Z  " + eurusd_uri + "  1.0815  synthetic.eurusd");
    CHECK(out.str() == output.screen_lines[0] + "\n" + output.screen_lines[1] + "\n");

    REQUIRE(output.rows.size() == 3);
    CHECK(output.rows[0] == "observation_time,oresmd_uri,value,source");
    CHECK(output.rows[1] ==
          "2026-10-06 09:00:00.000000Z," + eurusd_uri + ",1.0812,synthetic.eurusd");
    CHECK(output.rows[2] ==
          "2026-10-06 10:00:00.000000Z," + eurusd_uri + ",1.0815,synthetic.eurusd");
}

TEST_CASE("a_csv_field_holding_a_separator_is_quoted", "[commands][marketdata][stream]") {
    auto lg(make_logger(test_suite));

    const auto row =
        marketdata_commands::tick_row(tick(eurusd_uri, "1.0812", "synthetic, quoted", 9));

    CHECK(row == "2026-10-06 09:00:00.000000Z," + eurusd_uri + ",1.0812,\"synthetic, quoted\"");
}

TEST_CASE("a_stream_that_receives_nothing_still_writes_the_header",
          "[commands][marketdata][stream]") {
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

TEST_CASE("an_unwritable_csv_path_is_reported", "[commands][marketdata][stream]") {
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
