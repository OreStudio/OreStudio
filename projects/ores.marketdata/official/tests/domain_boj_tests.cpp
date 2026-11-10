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
#include "ores.marketdata.official/domain/boj.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// The call rate as the time-series API sent it on 2026-11-10, cut to four days.
const std::string call_rate_json = R"json({
"STATUS":200,
"MESSAGEID":"M181000I",
"MESSAGE":"Successfully completed",
"DATE":"2026-11-10T15:08:39.824+09:00",
"PARAMETER":{"FORMAT":"JSON","LANG":"EN","DB":"FM01","STARTDATE":"202611","ENDDATE":"202611","STARTPOSITION":""},
"NEXTPOSITION":null,
"RESULTSET":[
{
"SERIES_CODE":"STRDCLUCON",
"NAME_OF_TIME_SERIES":"Call Rate, Uncollateralized Overnight, Average (Daily)",
"UNIT":"percent per annum",
"FREQUENCY":"DAILY",
"CATEGORY":"Call Rate",
"LAST_UPDATE":20261110,
"VALUES":{
"SURVEY_DATES":[20261105,20261106,20261107,20261108],
"VALUES":[1.227,1.229,null,null]
}
}
]
})json";

/// What the API sent for months with no publication yet.
const std::string no_call_rate_json =
    R"({"STATUS":200,"MESSAGEID":"M181030I","MESSAGE":"Successfully completed with no applicable data",)"
    R"("NEXTPOSITION":null,"RESULTSET":[{"SERIES_CODE":"STRDCLUCON",)"
    R"("VALUES":{"SURVEY_DATES":[20270101,20270102],"VALUES":[null,null]}}]})";

/// A transport that gives its stored answers in order, and records each address asked.
class boj_transport final : public net::transport {
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

constexpr year_month_day boj_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

std::string series_with(std::string_view dates, std::string_view values) {
    return std::string(
               R"({"STATUS":200,"NEXTPOSITION":null,"RESULTSET":[{"SERIES_CODE":"STRDCLUCON",)") +
           R"("VALUES":{"SURVEY_DATES":)" + std::string(dates) + R"(,"VALUES":)" +
           std::string(values) + "}}]}";
}

}

TEST_CASE("a_call_rate_answer_reads_as_decimal_fixings_of_its_key", "[domain][boj]") {
    const auto fixings = parse_boj_series(call_rate_json, "FM01.STRDCLUCON");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == boj_day(2026, 11, 5));
    CHECK((*fixings)[0].external_id == "FM01.STRDCLUCON");
    CHECK((*fixings)[0].value == "0.01227");
    CHECK((*fixings)[1].date == boj_day(2026, 11, 6));
    CHECK((*fixings)[1].value == "0.01229");
}

TEST_CASE("months_with_no_publication_read_as_no_fixing", "[domain][boj]") {
    const auto fixings = parse_boj_series(no_call_rate_json, "FM01.STRDCLUCON");

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("a_malformed_boj_answer_is_an_error_and_not_a_fixing", "[domain][boj]") {
    const std::string key = "FM01.STRDCLUCON";
    CHECK_FALSE(parse_boj_series("<html></html>", key).has_value());
    CHECK_FALSE(
        parse_boj_series(
            R"({"STATUS":400,"MESSAGEID":"M181013E","MESSAGE":"Nonexistent series code"})", key)
            .has_value());
    CHECK_FALSE(parse_boj_series(R"({"STATUS":200,"NEXTPOSITION":null})", key).has_value());
    CHECK_FALSE(parse_boj_series(series_with("[20261105]", "[1.227,1.229]"), key).has_value());
    CHECK_FALSE(parse_boj_series(series_with(R"(["2026-11-05"])", "[1.227]"), key).has_value());
    CHECK_FALSE(parse_boj_series(series_with("[20261131]", "[1.227]"), key).has_value());
    CHECK_FALSE(parse_boj_series(series_with("[20261105]", R"(["1.227"])"), key).has_value());
}

TEST_CASE("an_answer_that_does_not_name_the_series_is_an_error", "[domain][boj]") {
    CHECK_FALSE(parse_boj_series(call_rate_json, "FM01.STRDCLUCOX").has_value());
}

TEST_CASE("a_paged_boj_answer_is_an_error_and_not_a_part_read", "[domain][boj]") {
    std::string paged = call_rate_json;
    const auto at = paged.find(R"("NEXTPOSITION":null)");
    paged.replace(at, 19, R"("NEXTPOSITION":2)");

    CHECK_FALSE(parse_boj_series(paged, "FM01.STRDCLUCON").has_value());
}

TEST_CASE("the_boj_fetcher_asks_for_the_months_of_the_window_and_keeps_its_days", "[domain][boj]") {
    boj_transport transport;
    transport.answers.push_back({200, "application/json", call_rate_json});
    boj_fetcher fetcher(transport, "https://www.stat-search.boj.or.jp/");

    const auto universe = fetcher.universe("FM01.STRDCLUCON");
    const auto fixings =
        fetcher.fixings("FM01.STRDCLUCON", boj_day(2026, 11, 6), boj_day(2026, 11, 30));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{"FM01.STRDCLUCON"});
    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == boj_day(2026, 11, 6));
    CHECK(transport.asked ==
          std::vector<std::string>{"https://www.stat-search.boj.or.jp/api/v1/getDataCode?"
                                   "format=json&lang=en&db=FM01&code=STRDCLUCON&startDate=202611"
                                   "&endDate=202611"});
}

TEST_CASE("an_unknown_boj_series_is_an_error_and_not_an_empty_read", "[domain][boj]") {
    boj_transport transport;
    transport.answers.push_back({400, "application/json", ""});
    boj_fetcher fetcher(transport, "https://www.stat-search.boj.or.jp");

    CHECK_FALSE(
        fetcher.fixings("FM01.NOSUCH", boj_day(2026, 11, 5), boj_day(2026, 11, 6)).has_value());
}

TEST_CASE("the_boj_fetcher_refuses_a_key_that_is_not_a_series_key", "[domain][boj]") {
    boj_transport transport;
    boj_fetcher fetcher(transport, "https://www.stat-search.boj.or.jp");

    CHECK_FALSE(fetcher.universe("STRDCLUCON").has_value());
    CHECK_FALSE(fetcher.universe("FM01.").has_value());
    CHECK_FALSE(fetcher.universe("FM01.STRD&code=X").has_value());
    CHECK_FALSE(
        fetcher.fixings("fm01.strdclucon", boj_day(2026, 11, 5), boj_day(2026, 11, 6)).has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("a_published_value_keeps_its_digits_when_it_has_no_exact_binary_form", "[domain][boj]") {
    const auto fixings = parse_boj_series(
        series_with("[20261102,20261104,20261105,20261106]", "[0.1,0.07,1.005,0.003]"),
        "FM01.STRDCLUCON");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 4);
    CHECK((*fixings)[0].value == "0.001");
    CHECK((*fixings)[1].value == "0.0007");
    CHECK((*fixings)[2].value == "0.01005");
    CHECK((*fixings)[3].value == "0.00003");
}

TEST_CASE("two_entries_that_name_the_series_are_an_error", "[domain][boj]") {
    const std::string twice =
        R"({"STATUS":200,"NEXTPOSITION":null,"RESULTSET":[)"
        R"({"SERIES_CODE":"STRDCLUCON","VALUES":{"SURVEY_DATES":[20261105],"VALUES":[1.227]}},)"
        R"({"SERIES_CODE":"STRDCLUCON","VALUES":{"SURVEY_DATES":[20261105],"VALUES":[9.9]}}]})";

    CHECK_FALSE(parse_boj_series(twice, "FM01.STRDCLUCON").has_value());
}
