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
#include "ores.marketdata.official/domain/rba.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

const std::string aonia_key = "f1.FIRMMCRID";

/// Table F1 as the RBA sent it on 2026-11-10, cut to its first four columns and three days. The
/// last day stops before the series, as the RBA writes a day it has not yet published.
const std::string f1_csv =
    "\xEF\xBB\xBF"
    "F1 INTEREST RATES AND YIELDS \xE2\x80\x93 MONEY MARKET\r\n"
    "Title,Cash Rate Target,Change in the Cash Rate Target,Interbank Overnight Cash Rate\r\n"
    "Units,Per cent,Per cent,Per cent\r\n"
    "\r\n"
    "Source,RBA,RBA,RBA\r\n"
    "Series ID,FIRMMCRTD,FIRMMCCRT,FIRMMCRID\r\n"
    "06-Nov-2026,4.85,,4.85\r\n"
    "09-Nov-2026,4.85,,4.86\r\n"
    "10-Nov-2026,,\r\n";

/// A transport that gives its stored answers in order, and records each address asked.
class rba_transport final : public net::transport {
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

constexpr year_month_day rba_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("an_rba_table_reads_as_decimal_fixings_of_its_series", "[domain][rba]") {
    const auto fixings = parse_rba_table(f1_csv, "FIRMMCRID", aonia_key);

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == rba_day(2026, 11, 6));
    CHECK((*fixings)[0].external_id == aonia_key);
    CHECK((*fixings)[0].value == "0.0485");
    CHECK((*fixings)[1].date == rba_day(2026, 11, 9));
    CHECK((*fixings)[1].value == "0.0486");
}

TEST_CASE("an_empty_rba_cell_is_skipped_and_not_read_from_another_column", "[domain][rba]") {
    const auto fixings = parse_rba_table(f1_csv, "FIRMMCCRT", "f1.FIRMMCCRT");

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("a_malformed_rba_answer_is_an_error_and_not_a_fixing", "[domain][rba]") {
    CHECK_FALSE(parse_rba_table("<html>Access denied</html>", "FIRMMCRID", aonia_key).has_value());
    CHECK_FALSE(parse_rba_table(f1_csv, "FIRMMNONE", "f1.FIRMMNONE").has_value());
    CHECK_FALSE(parse_rba_table("Series ID,FIRMMCRID\n2026-11-06,4.85\n", "FIRMMCRID", aonia_key)
                    .has_value());
    CHECK_FALSE(parse_rba_table("Series ID,FIRMMCRID\n31-Nov-2026,4.85\n", "FIRMMCRID", aonia_key)
                    .has_value());
    CHECK_FALSE(parse_rba_table("Series ID,FIRMMCRID\n06-Nov-2026,n/a\n", "FIRMMCRID", aonia_key)
                    .has_value());
}

TEST_CASE("a_row_after_the_header_without_a_date_is_an_error_and_not_a_skipped_row",
          "[domain][rba]") {
    CHECK_FALSE(parse_rba_table(f1_csv + ",,,4.85\r\n", "FIRMMCRID", aonia_key).has_value());
    CHECK_FALSE(parse_rba_table(f1_csv + "Note: rates are end of day\r\n", "FIRMMCRID", aonia_key)
                    .has_value());
}

TEST_CASE("the_rba_fetcher_reads_the_window_of_its_series", "[domain][rba]") {
    rba_transport transport;
    transport.answers.push_back({200, "application/octet-stream", f1_csv});
    rba_fetcher fetcher(transport, "https://www.rba.gov.au/");

    const auto universe = fetcher.universe(aonia_key);
    const auto fixings = fetcher.fixings(aonia_key, rba_day(2026, 11, 9), rba_day(2026, 11, 10));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{aonia_key});
    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == rba_day(2026, 11, 9));
    CHECK(transport.asked ==
          std::vector<std::string>{"https://www.rba.gov.au/statistics/tables/csv/f1-data.csv"});
}

TEST_CASE("an_rba_key_names_a_table_and_a_series", "[domain][rba]") {
    rba_transport transport;
    rba_fetcher fetcher(transport, "https://www.rba.gov.au");

    CHECK(fetcher.universe("f2.1.FCMYGBAG2").has_value());
    CHECK(fetcher.universe("f1-hist.FIRMMCRID").has_value());
    CHECK_FALSE(fetcher.universe("FIRMMCRID").has_value());
    CHECK_FALSE(fetcher.universe("F1.FIRMMCRID").has_value());
    CHECK_FALSE(fetcher.universe("f1.firmmcrid").has_value());
    CHECK_FALSE(fetcher.universe("../f1.FIRMMCRID").has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("a_failed_rba_read_is_an_error", "[domain][rba]") {
    rba_transport transport;
    transport.answers.push_back({403, "text/html", ""});
    rba_fetcher fetcher(transport, "https://www.rba.gov.au");

    CHECK_FALSE(fetcher.fixings(aonia_key, rba_day(2026, 11, 6), rba_day(2026, 11, 9)).has_value());
}
