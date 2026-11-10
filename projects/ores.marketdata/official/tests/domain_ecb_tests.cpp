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
#include "ores.marketdata.official/domain/ecb.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

const std::string estr_key = "EST.B.EU000A2X2A25.WT";

/// Rows of the €STR series as the ECB Data Portal sent them on 2026-11-10, cut short.
const std::string estr_csv = "KEY,FREQ,BENCHMARK_ITEM,DATA_TYPE_EST,TIME_PERIOD,OBS_VALUE\r\n"
                             "EST.B.EU000A2X2A25.WT,B,EU000A2X2A25,WT,2026-11-03,2.445\r\n"
                             "EST.B.EU000A2X2A25.WT,B,EU000A2X2A25,WT,2026-11-04,2.442\r\n";

/// The first row of the series, when €STR was negative.
const std::string first_estr_csv = "KEY,FREQ,BENCHMARK_ITEM,DATA_TYPE_EST,TIME_PERIOD,OBS_VALUE\n"
                                   "EST.B.EU000A2X2A25.WT,B,EU000A2X2A25,WT,2019-10-01,-0.549\n";

/// A transport that gives its stored answers in order, and records each address asked.
class ecb_transport final : public net::transport {
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

constexpr year_month_day ecb_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

const std::string estr_url =
    "https://data-api.ecb.europa.eu/service/data/EST/B.EU000A2X2A25.WT?startPeriod=2026-11-03&"
    "endPeriod=2026-11-04&format=csvdata&detail=dataonly";

}

TEST_CASE("an_estr_answer_reads_as_decimal_fixings_of_its_series", "[domain][ecb]") {
    const auto fixings = parse_ecb_csv(estr_csv, estr_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == ecb_day(2026, 11, 3));
    CHECK((*fixings)[0].external_id == estr_key);
    CHECK((*fixings)[0].value == "0.02445");
    CHECK((*fixings)[1].value == "0.02442");
}

TEST_CASE("a_negative_estr_keeps_its_sign", "[domain][ecb]") {
    const auto fixings = parse_ecb_csv(first_estr_csv, estr_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].value == "-0.00549");
}

TEST_CASE("an_empty_ecb_answer_reads_as_no_fixing", "[domain][ecb]") {
    CHECK(parse_ecb_csv("", estr_key).value().empty());
    CHECK(parse_ecb_csv("KEY,FREQ,BENCHMARK_ITEM,DATA_TYPE_EST,TIME_PERIOD,OBS_VALUE\n", estr_key)
              .value()
              .empty());
}

TEST_CASE("a_malformed_ecb_answer_is_an_error_and_not_a_fixing", "[domain][ecb]") {
    CHECK_FALSE(parse_ecb_csv("<html><h1>ECB Data Portal</h1></html>", estr_key).has_value());
    CHECK_FALSE(
        parse_ecb_csv("KEY,TIME_PERIOD,OBS_VALUE\nEST.B.EU000A2X2A25.WT\n", estr_key).has_value());
    CHECK_FALSE(
        parse_ecb_csv("KEY,TIME_PERIOD,OBS_VALUE\nEST.B.EU000A2X2A25.WT,3 Nov,2.445\n", estr_key)
            .has_value());
    CHECK_FALSE(
        parse_ecb_csv("KEY,TIME_PERIOD,OBS_VALUE\nEST.B.EU000A2X2A25.WT,2026-11-03,n/a\n", estr_key)
            .has_value());
}

TEST_CASE("the_ecb_fetcher_reads_the_window_of_its_series", "[domain][ecb]") {
    ecb_transport transport;
    transport.answers.push_back({200, "text/csv", estr_csv});
    ecb_fetcher fetcher(transport, "https://data-api.ecb.europa.eu/");

    const auto universe = fetcher.universe(estr_key);
    const auto fixings = fetcher.fixings(estr_key, ecb_day(2026, 11, 3), ecb_day(2026, 11, 4));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{estr_key});
    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
    CHECK(transport.asked == std::vector<std::string>{estr_url});
}

TEST_CASE("the_ecb_fetcher_asks_again_after_a_gateway_timeout", "[domain][ecb]") {
    ecb_transport transport;
    transport.answers.push_back({504, "text/html", "<html>Please try again</html>"});
    transport.answers.push_back({200, "text/csv", estr_csv});
    ecb_fetcher fetcher(transport, "https://data-api.ecb.europa.eu");

    const auto fixings = fetcher.fixings(estr_key, ecb_day(2026, 11, 3), ecb_day(2026, 11, 4));

    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
    CHECK(transport.asked.size() == 2);
}

TEST_CASE("the_ecb_fetcher_gives_up_after_three_gateway_timeouts", "[domain][ecb]") {
    ecb_transport transport;
    for (int i = 0; i < 4; ++i)
        transport.answers.push_back({504, "text/html", "<html>Please try again</html>"});
    ecb_fetcher fetcher(transport, "https://data-api.ecb.europa.eu");

    const auto fixings = fetcher.fixings(estr_key, ecb_day(2026, 11, 3), ecb_day(2026, 11, 4));

    CHECK_FALSE(fixings.has_value());
    CHECK(transport.asked.size() == 3);
}

TEST_CASE("the_ecb_fetcher_refuses_a_key_with_no_dataflow", "[domain][ecb]") {
    ecb_transport transport;
    ecb_fetcher fetcher(transport, "https://data-api.ecb.europa.eu");

    CHECK_FALSE(fetcher.universe("ESTR").has_value());
    CHECK_FALSE(fetcher.fixings("ESTR", ecb_day(2026, 11, 3), ecb_day(2026, 11, 4)).has_value());
    CHECK(transport.asked.empty());
}
