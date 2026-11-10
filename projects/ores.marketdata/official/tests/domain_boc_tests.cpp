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
#include "ores.marketdata.official/domain/boc.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// Observations of CORRA as the Valet API sent them on 2026-11-10, cut short.
const std::string corra_json =
    R"({"terms":{"url":"https://www.bankofcanada.ca/terms/"},)"
    R"("seriesDetail":{"AVG.INTWO":{"label":"CORRA","description":"Canadian Overnight Repo Rate Average"}},)"
    R"("observations":[{"d":"2026-11-05","AVG.INTWO":{"v":"2.5100"}},)"
    R"({"d":"2026-11-06","AVG.INTWO":{"v":"2.5200"}}]})";

/// What the API sent for a window with no business day.
const std::string no_corra_json = R"({"terms":{},"seriesDetail":{},"observations":[]})";

/// A transport that gives its stored answers in order, and records each address asked.
class boc_transport final : public net::transport {
public:
    std::deque<net::response> answers;
    std::vector<std::string> asked;

    std::expected<net::response, net::failure> get(std::string_view url,
                                                   const net::request_limits&) override {
        asked.emplace_back(url);
        if (answers.empty())
            return std::unexpected(net::failure{net::failure_kind::network, "no stored answer"});
        auto answer = std::move(answers.front());
        answers.pop_front();
        return answer;
    }
};

constexpr year_month_day boc_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_corra_answer_reads_as_decimal_fixings_of_its_series", "[domain][boc]") {
    const auto fixings = parse_valet_observations(corra_json, "AVG.INTWO");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == boc_day(2026, 11, 5));
    CHECK((*fixings)[0].external_id == "AVG.INTWO");
    CHECK((*fixings)[0].value == "0.0251");
    CHECK((*fixings)[1].value == "0.0252");
}

TEST_CASE("a_valet_answer_with_no_observation_reads_as_no_fixing", "[domain][boc]") {
    const auto fixings = parse_valet_observations(no_corra_json, "AVG.INTWO");

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("an_observation_without_the_series_or_its_value_is_skipped", "[domain][boc]") {
    const auto fixings = parse_valet_observations(
        R"({"observations":[{"d":"2026-11-05"},{"d":"2026-11-06","AVG.INTWO":{"v":""}}]})",
        "AVG.INTWO");

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("a_malformed_valet_answer_is_an_error_and_not_a_fixing", "[domain][boc]") {
    CHECK_FALSE(parse_valet_observations("<html></html>", "AVG.INTWO").has_value());
    CHECK_FALSE(parse_valet_observations(R"({"message":"Series NOPE not found."})", "AVG.INTWO")
                    .has_value());
    CHECK_FALSE(
        parse_valet_observations(R"({"observations":[{"AVG.INTWO":{"v":"2.51"}}]})", "AVG.INTWO")
            .has_value());
    CHECK_FALSE(parse_valet_observations(
                    R"({"observations":[{"d":"2026-11-05","AVG.INTWO":{"v":2.51}}]})", "AVG.INTWO")
                    .has_value());
    CHECK_FALSE(parse_valet_observations(
                    R"({"observations":[{"d":"5 Nov","AVG.INTWO":{"v":"2.51"}}]})", "AVG.INTWO")
                    .has_value());
    CHECK_FALSE(parse_valet_observations(
                    R"({"observations":[{"d":"2026-11-05","AVG.INTWO":{"v":"n/a"}}]})", "AVG.INTWO")
                    .has_value());
}

TEST_CASE("the_boc_fetcher_reads_the_window_of_its_series", "[domain][boc]") {
    boc_transport transport;
    transport.answers.push_back({200, "application/json", corra_json});
    boc_fetcher fetcher(transport, "https://www.bankofcanada.ca/");

    const auto universe = fetcher.universe("AVG.INTWO");
    const auto fixings = fetcher.fixings("AVG.INTWO", boc_day(2026, 11, 5), boc_day(2026, 11, 6));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{"AVG.INTWO"});
    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
    CHECK(transport.asked ==
          std::vector<std::string>{"https://www.bankofcanada.ca/valet/observations/AVG.INTWO/"
                                   "json?start_date=2026-11-05&end_date=2026-11-06"});
}

TEST_CASE("an_unknown_valet_series_is_an_error_and_not_an_empty_read", "[domain][boc]") {
    boc_transport transport;
    transport.answers.push_back(
        {404, "application/json", R"({"message":"Series AVG.INTWO not found."})"});
    boc_fetcher fetcher(transport, "https://www.bankofcanada.ca");

    CHECK_FALSE(
        fetcher.fixings("AVG.INTWO", boc_day(2026, 11, 5), boc_day(2026, 11, 6)).has_value());
}

TEST_CASE("the_boc_fetcher_refuses_a_name_that_is_not_a_series_name", "[domain][boc]") {
    boc_transport transport;
    boc_fetcher fetcher(transport, "https://www.bankofcanada.ca");

    CHECK_FALSE(fetcher.universe("avg/../x").has_value());
    CHECK_FALSE(fetcher.fixings("", boc_day(2026, 11, 5), boc_day(2026, 11, 6)).has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("observations_of_which_none_names_the_series_are_an_error", "[domain][boc]") {
    CHECK_FALSE(
        parse_valet_observations(
            R"({"observations":[{"d":"2026-11-05","AVG.INTWO2":{"v":"2.51"}}]})", "AVG.INTWO")
            .has_value());
}
