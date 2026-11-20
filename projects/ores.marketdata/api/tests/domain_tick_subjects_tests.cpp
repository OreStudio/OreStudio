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
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/domain/adapter_subjects.hpp"
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

TEST_CASE("a_producer_publishes_on_its_source_subject", "[domain][tick_subjects]") {
    CHECK(ores::marketdata::domain::synthetic_tick_subject_for("usd.sofr") ==
          "synthetic.v1.ops.tick.usd.sofr");
}

TEST_CASE("a_subject_token_holds_only_what_a_subject_may", "[domain][tick_subjects]") {
    const std::string uri =
        "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD *>. \t";
    const auto token = ores::marketdata::domain::subject_token(uri);
    for (const char c : token) {
        const bool allowed = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
                             (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '%';
        CHECK(allowed);
    }
    CHECK(token.find('.') == std::string::npos);
    CHECK(token.find(' ') == std::string::npos);
    CHECK(token.find('*') == std::string::npos);
    CHECK(token.find('>') == std::string::npos);
}

TEST_CASE("a_subject_token_gives_its_text_back", "[domain][tick_subjects]") {
    using ores::marketdata::domain::subject_token;
    using ores::marketdata::domain::subject_token_text;
    for (const auto& text : {std::string(""),
                             std::string("plain-Text_9"),
                             std::string("oresmd://fx/EUR?type=quote&ccy=USD"),
                             std::string("a.b c*d>e%f"),
                             std::string("caf\xc3\xa9"),
                             std::string("tab\there\nnewline")}) {
        CAPTURE(text);
        CHECK(subject_token_text(subject_token(text)) == text);
    }
}

TEST_CASE("a_subject_token_refuses_text_it_could_not_have_made", "[domain][tick_subjects]") {
    using ores::marketdata::domain::subject_token_text;
    CHECK_FALSE(subject_token_text("a.b").has_value());
    CHECK_FALSE(subject_token_text("%").has_value());
    CHECK_FALSE(subject_token_text("%2").has_value());
    CHECK_FALSE(subject_token_text("%ZZ").has_value());
    CHECK_FALSE(subject_token_text("%2e").has_value());
    // The encoder leaves a plain byte as it is, so an escaped one has another maker.
    CHECK_FALSE(subject_token_text("%41").has_value());
    CHECK_FALSE(subject_token_text("a%5Fb").has_value());
}

TEST_CASE("different_data_never_share_a_token", "[domain][tick_subjects]") {
    using ores::marketdata::domain::subject_token;
    CHECK(subject_token("a.b") != subject_token("a%2Eb"));
    CHECK(subject_token("a%b") != subject_token("a b"));
}

namespace {

const std::string tick_subjects_feed = "0f1e2d3c-4b5a-6978-8796-a5b4c3d2e1f0";

/// The feed subject of @p uri, with its series URI written by the codec.
std::string tick_subjects_feed_subject(const std::string& uri) {
    using ores::marketdata::datum::oresmd_uri_codec;
    const auto datum = oresmd_uri_codec::read(uri).value();
    const auto series = oresmd_uri_codec::write(ores::marketdata::datum::series_of(datum)).value();
    return ores::marketdata::domain::feed_tick_subject(
        "bloomberg", tick_subjects_feed, datum, series);
}

}

TEST_CASE("a_feed_publishes_a_datum_under_its_provider_feed_type_series_and_coordinates",
          "[domain][tick_subjects]") {
    const auto subject = tick_subjects_feed_subject(
        "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=5Y");

    CHECK(subject == "marketdata.v1.ops.market_tick.bloomberg." + tick_subjects_feed + ".ir_swap." +
                         ores::marketdata::domain::subject_token(
                             "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate"
                             "&fwd_start=0D&tenor=3M") +
                         ".term%3D5Y");
}

TEST_CASE("a_datum_without_coordinates_has_the_fixed_coordinate_token", "[domain][tick_subjects]") {
    const auto subject = tick_subjects_feed_subject(
        "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");

    CHECK(subject == "marketdata.v1.ops.market_tick.bloomberg." + tick_subjects_feed + ".fx_spot." +
                         ores::marketdata::domain::subject_token(
                             "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD") +
                         "._");
}

TEST_CASE("one_wildcard_covers_a_series_and_nothing_else", "[domain][tick_subjects]") {
    using ores::marketdata::domain::subject_matches;
    const auto five_years = tick_subjects_feed_subject(
        "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=5Y");
    const auto ten_years = tick_subjects_feed_subject(
        "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=10Y");
    const auto six_months_tenor = tick_subjects_feed_subject(
        "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M&term=5Y");

    const auto series = five_years.substr(0, five_years.rfind('.')) + ".>";

    CHECK(five_years != ten_years);
    CHECK(subject_matches(series, five_years));
    CHECK(subject_matches(series, ten_years));
    CHECK_FALSE(subject_matches(series, six_months_tenor));
}

TEST_CASE("a_watcher_of_a_point_subscribes_to_the_subject_its_feed_publishes_on",
          "[domain][tick_subjects]") {
    const std::string uri =
        "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=5Y";
    const auto datum = ores::marketdata::datum::oresmd_uri_codec::read(uri).value();

    const auto watched =
        ores::marketdata::domain::feed_tick_watch_subject("bloomberg", tick_subjects_feed, datum);

    REQUIRE(watched.has_value());
    CHECK(*watched == tick_subjects_feed_subject(uri));
}

TEST_CASE("a_watcher_of_a_series_receives_its_points_and_no_other_series",
          "[domain][tick_subjects]") {
    using ores::marketdata::domain::subject_matches;
    const auto series = ores::marketdata::datum::oresmd_uri_codec::read(
                            "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate"
                            "&fwd_start=0D&tenor=3M")
                            .value();

    const auto watched =
        ores::marketdata::domain::feed_tick_watch_subject("bloomberg", tick_subjects_feed, series);

    REQUIRE(watched.has_value());
    CHECK(subject_matches(*watched,
                          tick_subjects_feed_subject("oresmd://ir/USD?type=quote&instrument=ir_swap"
                                                     "&quote=rate&fwd_start=0D&tenor=3M&term=5Y")));
    CHECK(
        subject_matches(*watched,
                        tick_subjects_feed_subject("oresmd://ir/USD?type=quote&instrument=ir_swap"
                                                   "&quote=rate&fwd_start=0D&tenor=3M&term=10Y")));
    CHECK_FALSE(subject_matches(
        *watched,
        tick_subjects_feed_subject("oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate"
                                   "&fwd_start=0D&tenor=6M&term=5Y")));
}

TEST_CASE("a_watcher_of_a_providers_series_hears_every_feed_of_the_provider",
          "[domain][tick_subjects]") {
    using ores::marketdata::domain::provider_series_watch_subjects;
    const std::string fx =
        "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD";

    const auto watched = provider_series_watch_subjects("bloomberg", {fx, "not a uri"});

    REQUIRE(watched.size() == 1);
    CHECK(watched.front() == "marketdata.v1.ops.market_tick.bloomberg.*.fx_spot." +
                                 ores::marketdata::domain::subject_token(fx) + ".*");
    // The subject a tick of the series travels on, on one of the provider's feeds, is one the
    // watch covers: it differs only in the feed token and the coordinate token.
    const auto heard = tick_subjects_feed_subject(
        "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");
    CHECK(heard.starts_with("marketdata.v1.ops.market_tick.bloomberg." + tick_subjects_feed +
                            ".fx_spot." + ores::marketdata::domain::subject_token(fx) + "."));
}
