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
#include "ores.marketdata.official/domain/norges_bank.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

const std::string nowa_key = "SHORT_RATES.B.NOWA.ON.R";

/// NOWA as the SDMX API sent it on 2026-11-10, cut to two days.
const std::string nowa_csv =
    "FREQ;Frequency;INSTRUMENT_TYPE;Instrument Type;TENOR;Tenor;UNIT_MEASURE;Unit of "
    "Measure;COLLECTION;DECIMALS;TIME_PERIOD;OBS_VALUE;CALC_METHOD;Calculation Method\r\n"
    "B;Business;NOWA;NOWA;ON;Overnight;R;Rate;;2;2026-11-05;4.5;N;Normal\r\n"
    "B;Business;NOWA;NOWA;ON;Overnight;R;Rate;;2;2026-11-06;4.51;N;Normal\r\n";

/// The 404 the API sent both for a window with no observation and for an unknown series.
const std::string no_data_xml =
    "<?xml version='1.0' encoding='UTF-8'?><message:Error><message:ErrorMessage code=\"100\">"
    "<com:Text>No data for data query</com:Text></message:ErrorMessage></message:Error>";

/// A transport that gives its stored answers in order, and records each address asked.
class nb_transport final : public net::transport {
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

constexpr year_month_day nb_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_nowa_answer_reads_as_decimal_fixings_of_its_key", "[domain][norges_bank]") {
    const auto fixings = parse_norges_bank_csv(nowa_csv, nowa_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == nb_day(2026, 11, 5));
    CHECK((*fixings)[0].external_id == nowa_key);
    CHECK((*fixings)[0].value == "0.045");
    CHECK((*fixings)[1].value == "0.0451");
}

TEST_CASE("a_nowa_row_with_no_value_is_skipped", "[domain][norges_bank]") {
    const auto fixings =
        parse_norges_bank_csv("TIME_PERIOD;OBS_VALUE\n2026-11-05;\n2026-11-06;4.5\n", nowa_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == nb_day(2026, 11, 6));
}

TEST_CASE("a_malformed_norges_bank_answer_is_an_error_and_not_a_fixing", "[domain][norges_bank]") {
    CHECK_FALSE(parse_norges_bank_csv(no_data_xml, nowa_key).has_value());
    CHECK_FALSE(
        parse_norges_bank_csv("TIME_PERIOD,OBS_VALUE\n2026-11-05,4.5\n", nowa_key).has_value());
    CHECK_FALSE(parse_norges_bank_csv("TIME_PERIOD;OBS_VALUE\n2026-11-05\n", nowa_key).has_value());
    CHECK_FALSE(
        parse_norges_bank_csv("TIME_PERIOD;OBS_VALUE\n05/11/2026;4.5\n", nowa_key).has_value());
    CHECK_FALSE(
        parse_norges_bank_csv("TIME_PERIOD;OBS_VALUE\n2026-11-05;4,5\n", nowa_key).has_value());
}

TEST_CASE("the_norges_bank_fetcher_reads_the_window_of_its_series", "[domain][norges_bank]") {
    nb_transport transport;
    transport.answers.push_back({200, "text/csv", nowa_csv});
    norges_bank_fetcher fetcher(transport, "https://data.norges-bank.no/");

    const auto universe = fetcher.universe(nowa_key);
    const auto fixings = fetcher.fixings(nowa_key, nb_day(2026, 11, 6), nb_day(2026, 11, 6));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{nowa_key});
    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == nb_day(2026, 11, 6));
    CHECK(transport.asked ==
          std::vector<std::string>{"https://data.norges-bank.no/api/data/SHORT_RATES/B.NOWA.ON.R?"
                                   "format=csv&startPeriod=2026-11-06&endPeriod=2026-11-06"
                                   "&locale=en"});
}

TEST_CASE("an_empty_window_of_a_known_series_reads_as_no_fixing", "[domain][norges_bank]") {
    nb_transport transport;
    transport.answers.push_back({404, "application/xml", no_data_xml});
    transport.answers.push_back({200, "text/csv", nowa_csv});
    norges_bank_fetcher fetcher(transport, "https://data.norges-bank.no");

    const auto fixings = fetcher.fixings(nowa_key, nb_day(2027, 1, 1), nb_day(2027, 1, 10));

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
    REQUIRE(transport.asked.size() == 2);
    CHECK(transport.asked[1] == "https://data.norges-bank.no/api/data/SHORT_RATES/B.NOWA.ON.R?"
                                "format=csv&lastNObservations=1&locale=en");
}

TEST_CASE("an_unknown_norges_bank_series_is_an_error_and_not_an_empty_read",
          "[domain][norges_bank]") {
    nb_transport transport;
    transport.answers.push_back({404, "application/xml", no_data_xml});
    transport.answers.push_back({404, "application/xml", no_data_xml});
    norges_bank_fetcher fetcher(transport, "https://data.norges-bank.no");

    CHECK_FALSE(fetcher.fixings("SHORT_RATES.B.NOPE.ON.R", nb_day(2026, 11, 5), nb_day(2026, 11, 6))
                    .has_value());
}

TEST_CASE("a_failed_norges_bank_read_is_an_error", "[domain][norges_bank]") {
    nb_transport transport;
    transport.answers.push_back({503, "text/html", ""});
    norges_bank_fetcher fetcher(transport, "https://data.norges-bank.no");

    CHECK_FALSE(fetcher.fixings(nowa_key, nb_day(2026, 11, 5), nb_day(2026, 11, 6)).has_value());
    CHECK(transport.asked.size() == 1);
}

TEST_CASE("the_norges_bank_fetcher_refuses_a_key_that_is_not_a_series_key",
          "[domain][norges_bank]") {
    nb_transport transport;
    norges_bank_fetcher fetcher(transport, "https://data.norges-bank.no");

    CHECK_FALSE(fetcher.universe("SHORT_RATES").has_value());
    CHECK_FALSE(fetcher.universe("SHORT_RATES..NOWA").has_value());
    CHECK_FALSE(fetcher.universe("SHORT_RATES.B.NOWA?x=1").has_value());
    CHECK_FALSE(fetcher.fixings("short_rates.b.nowa", nb_day(2026, 11, 5), nb_day(2026, 11, 6))
                    .has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("a_nowa_answer_that_starts_with_a_byte_order_mark_reads_as_fixings",
          "[domain][norges_bank]") {
    const auto fixings =
        parse_norges_bank_csv("\xEF\xBB\xBFTIME_PERIOD;OBS_VALUE\n2026-11-06;4.5\n", nowa_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].value == "0.045");
}
