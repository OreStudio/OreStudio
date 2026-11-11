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
#include "ores.marketdata.official/domain/bcb.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

const std::string cdi_key = "4389";

/// The annualised CDI as the SGS API sent it on 2026-11-11, cut to two days.
const std::string cdi_json =
    R"([{"data":"06/11/2026","valor":"13.65"},{"data":"09/11/2026","valor":"13.66"}])";

/// The 404 the API sent for a window with no value.
const std::string no_value_json =
    R"({"erro":{"statusCode":404,"detail":"br.gov.bcb.pec.sgs.comum.excecoes.SGSNegocioException: Value(s) not found"}})";

/// A transport that gives its stored answers in order, and records each address asked.
class bcb_transport final : public net::transport {
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

constexpr year_month_day bcb_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_cdi_answer_reads_as_decimal_fixings_of_its_key", "[domain][bcb]") {
    const auto fixings = parse_bcb_series(cdi_json, cdi_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == bcb_day(2026, 11, 6));
    CHECK((*fixings)[0].external_id == cdi_key);
    CHECK((*fixings)[0].value == "0.1365");
    CHECK((*fixings)[1].date == bcb_day(2026, 11, 9));
    CHECK((*fixings)[1].value == "0.1366");
}

TEST_CASE("a_cdi_entry_with_no_value_is_skipped", "[domain][bcb]") {
    const auto fixings = parse_bcb_series(
        R"([{"data":"06/11/2026","valor":""},{"data":"09/11/2026","valor":"13.65"}])", cdi_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == bcb_day(2026, 11, 9));
}

TEST_CASE("a_malformed_bcb_answer_is_an_error_and_not_a_fixing", "[domain][bcb]") {
    CHECK_FALSE(parse_bcb_series(no_value_json, cdi_key).has_value());
    CHECK_FALSE(parse_bcb_series("A serie nao existe!", cdi_key).has_value());
    CHECK_FALSE(parse_bcb_series(R"([{"data":"06/11/2026","valor":13.65}])", cdi_key).has_value());
    CHECK_FALSE(
        parse_bcb_series(R"([{"data":"2026-11-06","valor":"13.65"}])", cdi_key).has_value());
    CHECK_FALSE(
        parse_bcb_series(R"([{"data":"31/11/2026","valor":"13.65"}])", cdi_key).has_value());
    CHECK_FALSE(
        parse_bcb_series(R"([{"data":"06/11/-123","valor":"13.65"}])", cdi_key).has_value());
    CHECK_FALSE(
        parse_bcb_series(R"([{"data":"6a/11/2026","valor":"13.65"}])", cdi_key).has_value());
    CHECK_FALSE(
        parse_bcb_series(R"([{"data":"06/11/2026","valor":"13,65"}])", cdi_key).has_value());
}

TEST_CASE("the_bcb_fetcher_reads_the_window_of_its_series", "[domain][bcb]") {
    bcb_transport transport;
    transport.answers.push_back({200, "application/json", cdi_json});
    bcb_fetcher fetcher(transport, "https://api.bcb.gov.br/");

    const auto universe = fetcher.universe(cdi_key);
    const auto fixings = fetcher.fixings(cdi_key, bcb_day(2026, 11, 9), bcb_day(2026, 11, 10));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{cdi_key});
    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == bcb_day(2026, 11, 9));
    CHECK(transport.asked ==
          std::vector<std::string>{"https://api.bcb.gov.br/dados/serie/bcdata.sgs.4389/dados?"
                                   "formato=json&dataInicial=09/11/2026&dataFinal=10/11/2026"});
}

TEST_CASE("a_bcb_window_longer_than_five_years_is_read_in_parts", "[domain][bcb]") {
    bcb_transport transport;
    transport.answers.push_back(
        {200, "application/json", R"([{"data":"02/01/2014","valor":"9.77"}])"});
    transport.answers.push_back(
        {200, "application/json", R"([{"data":"02/01/2019","valor":"6.40"}])"});
    transport.answers.push_back({200, "application/json", cdi_json});
    bcb_fetcher fetcher(transport, "https://api.bcb.gov.br");

    const auto fixings = fetcher.fixings(cdi_key, bcb_day(2014, 1, 1), bcb_day(2026, 11, 10));

    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 4);
    const std::string series =
        "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4389/dados?formato=json";
    CHECK(transport.asked ==
          std::vector<std::string>{series + "&dataInicial=01/01/2014&dataFinal=31/12/2018",
                                   series + "&dataInicial=01/01/2019&dataFinal=31/12/2023",
                                   series + "&dataInicial=01/01/2024&dataFinal=10/11/2026"});
}

TEST_CASE("an_empty_bcb_window_of_a_known_series_reads_as_no_fixing", "[domain][bcb]") {
    bcb_transport transport;
    transport.answers.push_back({404, "application/json", no_value_json});
    transport.answers.push_back(
        {200, "application/json", R"([{"data":"09/11/2026","valor":"13.65"}])"});
    bcb_fetcher fetcher(transport, "https://api.bcb.gov.br");

    const auto fixings = fetcher.fixings(cdi_key, bcb_day(2026, 11, 1), bcb_day(2026, 11, 2));

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
    REQUIRE(transport.asked.size() == 2);
    CHECK(transport.asked[1] ==
          "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4389/dados/ultimos/1?formato=json");
}

TEST_CASE("an_unknown_bcb_series_is_an_error_and_not_an_empty_read", "[domain][bcb]") {
    bcb_transport transport;
    transport.answers.push_back({404, "text/plain", "A serie nao existe!"});
    transport.answers.push_back({404, "text/plain", "A serie nao existe!"});
    bcb_fetcher fetcher(transport, "https://api.bcb.gov.br");

    CHECK_FALSE(
        fetcher.fixings("999999999", bcb_day(2026, 11, 3), bcb_day(2026, 11, 10)).has_value());
}

TEST_CASE("a_bcb_key_is_a_series_number", "[domain][bcb]") {
    bcb_transport transport;
    bcb_fetcher fetcher(transport, "https://api.bcb.gov.br");

    CHECK_FALSE(fetcher.universe("").has_value());
    CHECK_FALSE(fetcher.universe("CDI").has_value());
    CHECK_FALSE(fetcher.universe("4389/../12").has_value());
    CHECK_FALSE(fetcher.universe("1234567890").has_value());
    CHECK(transport.asked.empty());
}
