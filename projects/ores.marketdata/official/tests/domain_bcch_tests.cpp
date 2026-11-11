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
#include "ores.marketdata.official/domain/bcch.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

const std::string tib_key = "F022.TIB.TIP.D001.NO.Z.D";
const bcch_account account{"someone@example.org", "p4ss-word"};

/**
 * The average overnight interbank rate as the BDE web service sent it on 2026-11-11, for the
 * 6th to the 10th of November. The service writes Latin-1, so the Spanish title holds the byte
 * 0xED for an accented i, which is not UTF-8.
 */
const std::string tib_json =
    "{\"Codigo\":0,\"Descripcion\":\"Success\",\"Series\":{\"descripEsp\":\"Tasa de pol\xEDtica\","
    "\"descripIng\":\"Average rate traded at interbank market (IBR) (average)\","
    "\"seriesId\":\"F022.TIB.TIP.D001.NO.Z.D\",\"Obs\":["
    "{\"indexDateString\":\"06-11-2026\",\"value\":\"4.5\",\"statusCode\":\"OK\"},"
    "{\"indexDateString\":\"07-11-2026\",\"value\":\"NaN\",\"statusCode\":\"ND\"},"
    "{\"indexDateString\":\"08-11-2026\",\"value\":\"NaN\",\"statusCode\":\"NA\"},"
    "{\"indexDateString\":\"09-11-2026\",\"value\":\"4.5\",\"statusCode\":\"OK\"},"
    "{\"indexDateString\":\"10-11-2026\",\"value\":\"4.49\",\"statusCode\":\"OK\"}]},"
    "\"SeriesInfos\":[]}";

/// What the service sent on 2026-11-11 for an unknown series.
const std::string unknown_json =
    R"({"Codigo":-50,"Descripcion":"An internal error has occurred, information is not available.","Series":{"descripEsp":null,"descripIng":null,"seriesId":null,"Obs":null},"SeriesInfos":[]})";

/// What the service sent on 2026-11-11 for an account whose API access was not active.
const std::string refused_json =
    R"({"Codigo":-5,"Descripcion":"Invalid username or password","Series":{"descripEsp":null,"descripIng":null,"seriesId":null,"Obs":null},"SeriesInfos":[]})";

/// A transport that gives its stored answers in order, and records each address asked.
class bcch_transport final : public net::transport {
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

constexpr year_month_day bcch_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_tib_answer_in_latin_1_reads_as_decimal_fixings_of_its_published_days",
          "[domain][bcch]") {
    const auto fixings = parse_bcch_series(tib_json, tib_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 3);
    CHECK((*fixings)[0].date == bcch_day(2026, 11, 6));
    CHECK((*fixings)[0].external_id == tib_key);
    CHECK((*fixings)[0].value == "0.045");
    CHECK((*fixings)[1].date == bcch_day(2026, 11, 9));
    CHECK((*fixings)[2].value == "0.0449");
}

TEST_CASE("a_bcch_error_code_is_an_error_that_gives_its_reason", "[domain][bcch]") {
    const auto unknown = parse_bcch_series(unknown_json, tib_key);
    const auto refused = parse_bcch_series(refused_json, tib_key);

    REQUIRE_FALSE(unknown.has_value());
    CHECK(unknown.error().find("-50") != std::string::npos);
    REQUIRE_FALSE(refused.has_value());
    CHECK(refused.error().find("Invalid username or password") != std::string::npos);
}

TEST_CASE("a_malformed_bcch_answer_is_an_error_and_not_a_fixing", "[domain][bcch]") {
    CHECK_FALSE(parse_bcch_series("<html>Error</html>", tib_key).has_value());
    CHECK_FALSE(parse_bcch_series(tib_json, "F022.TPM.TIN.D001.NO.Z.D").has_value());
    CHECK_FALSE(
        parse_bcch_series(
            R"({"Codigo":0,"Series":{"seriesId":"F022.TIB.TIP.D001.NO.Z.D","Obs":[{"indexDateString":"2026-11-06","value":"4.5","statusCode":"OK"}]}})",
            tib_key)
            .has_value());
    CHECK_FALSE(
        parse_bcch_series(
            R"({"Codigo":0,"Series":{"seriesId":"F022.TIB.TIP.D001.NO.Z.D","Obs":[{"indexDateString":"06-11-2026","value":"NaN","statusCode":"OK"}]}})",
            tib_key)
            .has_value());
    CHECK_FALSE(
        parse_bcch_series(
            R"({"Codigo":0,"Series":{"seriesId":"F022.TIB.TIP.D001.NO.Z.D","Obs":null}})", tib_key)
            .has_value());
}

TEST_CASE("the_bcch_fetcher_reads_the_window_of_its_series_with_its_account", "[domain][bcch]") {
    bcch_transport transport;
    transport.answers.push_back({200, "application/json", tib_json});
    bcch_fetcher fetcher(transport, "https://si3.bcentral.cl/", account);

    const auto fixings = fetcher.fixings(tib_key, bcch_day(2026, 11, 9), bcch_day(2026, 11, 10));

    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
    CHECK(transport.asked ==
          std::vector<std::string>{
              "https://si3.bcentral.cl/SieteRestWS/SieteRestWS.ashx?user=someone%40example.org"
              "&pass=p4ss-word&function=GetSeries&timeseries=F022.TIB.TIP.D001.NO.Z.D"
              "&firstdate=2026-11-09&lastdate=2026-11-10"});
}

TEST_CASE("a_refused_bcch_read_is_an_error_that_does_not_hold_the_account", "[domain][bcch]") {
    bcch_transport transport;
    transport.answers.push_back({503, "text/html", ""});
    bcch_fetcher fetcher(transport, "https://si3.bcentral.cl", account);

    const auto fixings = fetcher.fixings(tib_key, bcch_day(2026, 11, 6), bcch_day(2026, 11, 10));

    REQUIRE_FALSE(fixings.has_value());
    CHECK(fixings.error().find("503") != std::string::npos);
    CHECK(fixings.error().find("p4ss-word") == std::string::npos);
    CHECK(fixings.error().find("someone") == std::string::npos);
}

TEST_CASE("a_bcch_fetcher_without_an_account_reads_nothing_and_says_why", "[domain][bcch]") {
    bcch_transport transport;
    bcch_fetcher fetcher(transport,
                         "https://si3.bcentral.cl",
                         std::unexpected("the variable ORES_MARKETDATA_BCCH_USER is not set"));

    const auto universe = fetcher.universe(tib_key);
    const auto bad_id = fetcher.universe("F022/../x");

    REQUIRE_FALSE(universe.has_value());
    CHECK(universe.error().find("ORES_MARKETDATA_BCCH_USER") != std::string::npos);
    REQUIRE_FALSE(bad_id.has_value());
    CHECK(bad_id.error().find("is not a series id") != std::string::npos);
    CHECK(transport.asked.empty());
}

TEST_CASE("a_bcch_key_is_a_series_id", "[domain][bcch]") {
    bcch_transport transport;
    bcch_fetcher fetcher(transport, "https://si3.bcentral.cl", account);

    CHECK_FALSE(fetcher.universe("").has_value());
    CHECK_FALSE(fetcher.universe("f022.tib").has_value());
    CHECK_FALSE(fetcher.universe("F022.TIB&function=SearchSeries").has_value());
    CHECK(fetcher.universe(tib_key).has_value());
    CHECK(transport.asked.empty());
}
