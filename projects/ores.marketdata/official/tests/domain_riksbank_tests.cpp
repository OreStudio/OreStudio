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
#include "ores.marketdata.official/domain/riksbank.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// SWESTR as the API sent it on 2026-11-10, cut to two days.
const std::string swestr_json =
    R"([{"rate":1.644,"date":"2026-11-02","pctl12_5":1.60,"pctl87_5":1.70,"volume":85956,)"
    R"("alternativeCalculation":false,"alternativeCalculationReason":null,)"
    R"("publicationTime":"2026-11-03T08:00:00Z","republication":false,)"
    R"("numberOfTransactions":259,"numberOfAgents":5},)"
    R"({"rate":1.643,"date":"2026-11-03","pctl12_5":1.60,"pctl87_5":1.67,"volume":90676,)"
    R"("alternativeCalculation":false,"alternativeCalculationReason":null,)"
    R"("publicationTime":"2026-11-04T08:00:00Z","republication":false,)"
    R"("numberOfTransactions":267,"numberOfAgents":5}])";

/// What the API sent for the latest observation of SWESTR.
const std::string latest_json = R"({"rate":1.656,"date":"2026-11-06"})";

/// A transport that gives its stored answers in order, and records each address asked.
class riksbank_transport final : public net::transport {
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

constexpr year_month_day rb_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_swestr_answer_reads_as_decimal_fixings_of_its_key", "[domain][riksbank]") {
    const auto fixings = parse_swestr_rates(swestr_json, "SWESTR");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == rb_day(2026, 11, 2));
    CHECK((*fixings)[0].external_id == "SWESTR");
    CHECK((*fixings)[0].value == "0.01644");
    CHECK((*fixings)[1].value == "0.01643");
}

TEST_CASE("a_swestr_rate_of_null_is_skipped", "[domain][riksbank]") {
    const auto fixings = parse_swestr_rates(
        R"([{"rate":null,"date":"2026-11-02"},{"rate":1.6,"date":"2026-11-03"}])", "SWESTR");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].value == "0.016");
}

TEST_CASE("a_malformed_swestr_answer_is_an_error_and_not_a_fixing", "[domain][riksbank]") {
    CHECK_FALSE(parse_swestr_rates("<html></html>", "SWESTR").has_value());
    CHECK_FALSE(parse_swestr_rates(latest_json, "SWESTR").has_value());
    CHECK_FALSE(parse_swestr_rates(R"([{"date":"2026-11-02"}])", "SWESTR").has_value());
    CHECK_FALSE(
        parse_swestr_rates(R"([{"rate":"1.644","date":"2026-11-02"}])", "SWESTR").has_value());
    CHECK_FALSE(
        parse_swestr_rates(R"([{"rate":1.644,"date":"2 Nov 2026"}])", "SWESTR").has_value());
    CHECK_FALSE(parse_swestr_rates(R"([1.644])", "SWESTR").has_value());
}

TEST_CASE("the_riksbank_fetcher_reads_the_window_of_its_series", "[domain][riksbank]") {
    riksbank_transport transport;
    transport.answers.push_back({200, "application/json", swestr_json});
    riksbank_fetcher fetcher(transport, "https://api.riksbank.se/");

    const auto universe = fetcher.universe("SWESTR");
    const auto fixings = fetcher.fixings("SWESTR", rb_day(2026, 11, 3), rb_day(2026, 11, 6));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{"SWESTR"});
    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == rb_day(2026, 11, 3));
    CHECK(transport.asked ==
          std::vector<std::string>{"https://api.riksbank.se/swestr/v1/all/SWESTR?"
                                   "fromDate=2026-11-03&toDate=2026-11-06"});
}

TEST_CASE("an_empty_swestr_window_reads_as_no_fixing", "[domain][riksbank]") {
    riksbank_transport transport;
    transport.answers.push_back({404, "application/json", ""});
    transport.answers.push_back({200, "application/json", latest_json});
    riksbank_fetcher fetcher(transport, "https://api.riksbank.se");

    const auto fixings = fetcher.fixings("SWESTR", rb_day(2027, 1, 1), rb_day(2027, 1, 10));

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
    REQUIRE(transport.asked.size() == 2);
    CHECK(transport.asked[1] == "https://api.riksbank.se/swestr/v1/latest/SWESTR");
}

TEST_CASE("an_unknown_riksbank_series_is_an_error_and_not_an_empty_read", "[domain][riksbank]") {
    riksbank_transport transport;
    transport.answers.push_back({404, "application/json", ""});
    transport.answers.push_back({404, "application/json", ""});
    riksbank_fetcher fetcher(transport, "https://api.riksbank.se");

    CHECK_FALSE(fetcher.fixings("NOPE", rb_day(2026, 11, 2), rb_day(2026, 11, 6)).has_value());
}

TEST_CASE("the_riksbank_fetcher_refuses_a_name_that_is_not_a_series_name", "[domain][riksbank]") {
    riksbank_transport transport;
    riksbank_fetcher fetcher(transport, "https://api.riksbank.se");

    CHECK_FALSE(fetcher.universe("swestr").has_value());
    CHECK_FALSE(fetcher.universe("SWESTR/../x").has_value());
    CHECK_FALSE(fetcher.fixings("", rb_day(2026, 11, 2), rb_day(2026, 11, 6)).has_value());
    CHECK(transport.asked.empty());
}
