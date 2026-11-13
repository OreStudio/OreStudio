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
#include "ores.analytics.quant/domain/market_board.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <stdexcept>
#include <string>
#include <vector>

using namespace ores::analytics::quant::domain;

namespace {

using board_clock = std::chrono::system_clock;

const std::chrono::year_month_day board_spot_date{
    std::chrono::year{2026}, std::chrono::November, std::chrono::day{16}};

std::chrono::year_month_day board_day(int offset) {
    return std::chrono::year_month_day(std::chrono::sys_days(board_spot_date) +
                                       std::chrono::days(offset));
}

/// A curve of three points on a flat three percent rate, so every factor is a number
/// a reader can check by hand.
std::vector<curve_point> board_curve() {
    std::vector<curve_point> points;
    for (const int offset : {0, 90, 365}) {
        const double years = static_cast<double>(offset) / 365.0;
        points.push_back({.date = board_day(offset), .discount_factor = std::exp(-0.03 * years)});
    }
    return points;
}

}

TEST_CASE("a member asked for twice is one member", "[market_board]") {
    market_board board;
    const auto first = board.member_of(market_member_kind::curve, "USD");
    const auto again = board.member_of(market_member_kind::curve, "USD");
    const auto other = board.member_of(market_member_kind::curve, "EUR");
    const auto pair = board.member_of(market_member_kind::spot, "EURUSD");

    CHECK(first == again);
    CHECK(first != other);
    CHECK(other != pair);
    CHECK(board.size() == 3);
}

TEST_CASE("a curve publishes its points and reads them back", "[market_board]") {
    market_board board;
    const auto id = board.member_of(market_member_kind::curve, "USD");
    const auto points = board_curve();
    const auto at = board_clock::time_point(std::chrono::seconds(1'000));

    board.publish_curve(id, board_spot_date, points, at);
    const auto view = board.view(id);

    CHECK(view.kind == market_member_kind::curve);
    CHECK(view.code == "USD");
    CHECK(view.published);
    CHECK(view.published_at == at);
    CHECK(view.spot_date == board_spot_date);
    REQUIRE(view.points.size() == points.size());
    for (std::size_t i = 0; i < points.size(); ++i) {
        CHECK(view.points[i].date == points[i].date);
        CHECK(view.points[i].discount_factor ==
              Catch::Approx(points[i].discount_factor).epsilon(1e-15));
    }
    CHECK(view.spot == 0.0);
}

TEST_CASE("a republish replaces the values and the timestamp", "[market_board]") {
    market_board board;
    const auto id = board.member_of(market_member_kind::curve, "USD");
    const auto at = board_clock::time_point(std::chrono::seconds(1'000));

    board.publish_curve(id, board_spot_date, board_curve(), at);

    const std::vector<curve_point> shorter{{.date = board_day(30), .discount_factor = 0.9}};
    const auto later = at + std::chrono::seconds(60);
    board.publish_curve(id, board_spot_date, shorter, later);

    const auto view = board.view(id);
    CHECK(view.published_at == later);
    REQUIRE(view.points.size() == 1);
    CHECK(view.points.front().date == board_day(30));
    CHECK(view.points.front().discount_factor == Catch::Approx(0.9).epsilon(1e-15));
}

TEST_CASE("a spot publishes its level and reads it back", "[market_board]") {
    market_board board;
    const auto id = board.member_of(market_member_kind::spot, "EURUSD");
    const auto at = board_clock::time_point(std::chrono::seconds(2'000));

    CHECK_FALSE(board.view(id).published);

    board.publish_spot(id, 1.0842, at);
    const auto view = board.view(id);
    CHECK(view.kind == market_member_kind::spot);
    CHECK(view.code == "EURUSD");
    CHECK(view.published);
    CHECK(view.published_at == at);
    CHECK(view.spot == Catch::Approx(1.0842).epsilon(1e-15));
    CHECK(view.points.empty());
}

TEST_CASE("a member nobody published reads as not published", "[market_board]") {
    market_board board;
    const auto id = board.member_of(market_member_kind::curve, "USD");
    const auto view = board.view(id);
    CHECK_FALSE(view.published);
    CHECK(view.points.empty());
}

TEST_CASE("finding a member never creates one", "[market_board]") {
    market_board board;
    const auto id = board.member_of(market_member_kind::curve, "USD");

    CHECK(*board.find(market_member_kind::curve, "USD") == id);
    CHECK_FALSE(board.find(market_member_kind::spot, "USD").has_value());
    CHECK_FALSE(board.find(market_member_kind::curve, "EUR").has_value());
    CHECK(board.size() == 1);
}

TEST_CASE("a member refuses the kind it is not", "[market_board]") {
    market_board board;
    const auto curve = board.member_of(market_member_kind::curve, "USD");
    const auto spot = board.member_of(market_member_kind::spot, "EURUSD");
    const auto at = board_clock::time_point(std::chrono::seconds(1'000));

    CHECK_THROWS_AS(board.publish_spot(curve, 1.0, at), std::invalid_argument);
    CHECK_THROWS_AS(board.publish_curve(spot, board_spot_date, board_curve(), at),
                    std::invalid_argument);
    CHECK_THROWS_AS(board.view(7), std::invalid_argument);
    CHECK_THROWS_AS(board.publish_spot(7, 1.0, at), std::invalid_argument);
}

TEST_CASE("a board with no room refuses to be built", "[market_board]") {
    CHECK_THROWS_AS(market_board{0}, std::invalid_argument);
}

TEST_CASE("a full board refuses another member", "[market_board]") {
    market_board board{2};
    static_cast<void>(board.member_of(market_member_kind::curve, "USD"));
    static_cast<void>(board.member_of(market_member_kind::curve, "EUR"));
    CHECK_THROWS_AS(board.member_of(market_member_kind::curve, "GBP"), std::length_error);
}

TEST_CASE("a publish reuses the member's storage once the size holds", "[market_board]") {
    // The claim the surface feed's tick depends on: after the first publish of a curve
    // of this size, a republish of the same size writes into the same buffer rather than
    // allocating a new one.
    market_board board;
    const auto id = board.member_of(market_member_kind::curve, "USD");
    const auto points = board_curve();
    const auto at = board_clock::time_point(std::chrono::seconds(1'000));

    board.publish_curve(id, board_spot_date, points, at);
    const auto first = board.view(id).points.data();

    for (int tick = 0; tick < 100; ++tick) {
        board.publish_curve(id, board_spot_date, points, at + std::chrono::seconds(tick));
        CHECK(board.view(id).points.data() == first);
    }
}

TEST_CASE("adding a member leaves the ones before it where they were", "[market_board]") {
    // An id is stable for the life of the board, which is what lets a feed keep it.
    market_board board;
    const auto usd = board.member_of(market_member_kind::curve, "USD");
    const auto at = board_clock::time_point(std::chrono::seconds(1'000));
    board.publish_curve(usd, board_spot_date, board_curve(), at);

    for (int i = 0; i < 32; ++i)
        static_cast<void>(
            board.member_of(market_member_kind::curve, std::string("CCY") + std::to_string(i)));

    const auto view = board.view(usd);
    CHECK(view.code == "USD");
    REQUIRE(view.points.size() == 3);
    CHECK(view.points.front().date == board_spot_date);
}
