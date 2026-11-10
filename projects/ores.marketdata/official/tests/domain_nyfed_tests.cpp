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
#include "ores.marketdata.official/domain/nyfed.hpp"
#include "ores.marketdata.official/domain/published_text.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// Rows of =/api/rates/secured/sofr/search.json= as the New York Fed sent them on
/// 2026-11-09, cut short.
const std::string sofr_rates =
    R"({ "refRates": [ { "effectiveDate": "2026-11-03", "type": "SOFR" ,"percentRate": 3.97 )"
    R"(,"percentPercentile1": 3.88 ,"percentPercentile25": 3.95 ,"percentPercentile75": 4.03 )"
    R"(,"percentPercentile99": 4.05 ,"volumeInBillions": 3260 ,"revisionIndicator": "" }, )"
    R"({ "effectiveDate": "2026-11-02", "type": "SOFR" ,"percentRate": 3.95 )"
    R"(,"percentPercentile1": 3.90 ,"percentPercentile25": 3.92 ,"percentPercentile75": 4.00 )"
    R"(,"percentPercentile99": 4.03 ,"volumeInBillions": 3268 ,"revisionIndicator": "" } ] })";

/// What the New York Fed sent for a window with no business day.
const std::string no_sofr_rates = R"({ "refRates": [ ] })";

class nyfed_transport final : public net::transport {
public:
    std::map<std::string, std::string> bodies;
    std::vector<std::string> asked;

    std::expected<net::response, net::failure> get(std::string_view url,
                                                   const net::request_limits&) override {
        asked.emplace_back(url);
        const auto found = bodies.find(std::string(url));
        if (found == bodies.end())
            return std::unexpected(net::failure{net::failure_kind::network, "no stored body"});
        return net::response{200, "application/json", found->second};
    }
};

constexpr year_month_day nyfed_day(int month, int day) {
    return year_month_day{year{2026},
                          std::chrono::month{static_cast<unsigned>(month)},
                          std::chrono::day{static_cast<unsigned>(day)}};
}

}

TEST_CASE("a_sofr_response_reads_as_decimal_fixings_of_its_type", "[domain][nyfed]") {
    const auto fixings = parse_nyfed_rates(sofr_rates);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == nyfed_day(11, 3));
    CHECK((*fixings)[0].external_id == "SOFR");
    CHECK((*fixings)[0].value == "0.0397");
    CHECK((*fixings)[1].date == nyfed_day(11, 2));
    CHECK((*fixings)[1].value == "0.0395");
}

TEST_CASE("a_window_with_no_rate_reads_as_no_fixing", "[domain][nyfed]") {
    const auto fixings = parse_nyfed_rates(no_sofr_rates);

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("a_malformed_nyfed_response_is_an_error_and_not_a_fixing", "[domain][nyfed]") {
    CHECK_FALSE(parse_nyfed_rates("<html>Service Unavailable</html>").has_value());
    CHECK_FALSE(parse_nyfed_rates(R"({ "rates": [] })").has_value());
    CHECK_FALSE(parse_nyfed_rates(
                    R"({ "refRates": [ { "effectiveDate": "2026-11-03", "type": "SOFR" } ] })")
                    .has_value());
    CHECK_FALSE(
        parse_nyfed_rates(
            R"({ "refRates": [ { "effectiveDate": "3 Nov", "type": "SOFR", "percentRate": 3.97 } ] })")
            .has_value());
}

TEST_CASE("the_nyfed_fetcher_reads_the_window_of_its_rate", "[domain][nyfed]") {
    nyfed_transport transport;
    transport.bodies["https://markets.newyorkfed.org/api/rates/secured/sofr/"
                     "search.json?startDate=2026-11-02&endDate=2026-11-03"] = sofr_rates;
    nyfed_fetcher fetcher(transport, "https://markets.newyorkfed.org/");

    const auto universe = fetcher.universe("SOFR");
    const auto fixings = fetcher.fixings("SOFR", nyfed_day(11, 2), nyfed_day(11, 3));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{"SOFR"});
    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
    CHECK(transport.asked.size() == 1);
}

TEST_CASE("the_nyfed_fetcher_refuses_a_rate_it_does_not_know", "[domain][nyfed]") {
    nyfed_transport transport;
    nyfed_fetcher fetcher(transport, "https://markets.newyorkfed.org");

    CHECK_FALSE(fetcher.universe("LIBOR").has_value());
    CHECK_FALSE(fetcher.fixings("LIBOR", nyfed_day(11, 2), nyfed_day(11, 3)).has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("a_percent_becomes_its_decimal_with_every_digit_kept", "[domain][published_text]") {
    CHECK(decimal_of_percent("3.97") == "0.0397");
    CHECK(decimal_of_percent("5") == "0.05");
    CHECK(decimal_of_percent("123.4") == "1.234");
    CHECK(decimal_of_percent("-0.01") == "-0.0001");
    CHECK(decimal_of_percent("1e-05") == "0.0000001");
    CHECK(decimal_of_percent("2.5e3") == "25");
    CHECK(decimal_of_percent("0") == "0");
    CHECK(decimal_of_percent("-0") == "0");
    CHECK(decimal_of_percent("3.7352") == "0.037352");
    CHECK_FALSE(decimal_of_percent("").has_value());
    CHECK_FALSE(decimal_of_percent("n/a").has_value());
}

TEST_CASE("the_nyfed_fetcher_keeps_only_the_rates_inside_its_window", "[domain][nyfed]") {
    nyfed_transport transport;
    transport.bodies["https://markets.newyorkfed.org/api/rates/secured/sofr/"
                     "search.json?startDate=2026-11-03&endDate=2026-11-03"] = sofr_rates;
    nyfed_fetcher fetcher(transport, "https://markets.newyorkfed.org");

    const auto fixings = fetcher.fixings("SOFR", nyfed_day(11, 3), nyfed_day(11, 3));

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == nyfed_day(11, 3));
}
