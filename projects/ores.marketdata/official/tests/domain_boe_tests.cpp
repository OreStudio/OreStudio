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
#include "ores.marketdata.official/domain/boe.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// Rows of SONIA as the Bank of England's database sent them on 2026-11-10, cut short.
const std::string sonia_csv = "DATE,IUDSOIA\n"
                              "04 Nov 2026,3.7335\n"
                              "05 Nov 2026,3.9824\n";

/// The first row of the series.
const std::string first_sonia_csv = "DATE,IUDSOIA\r\n02 Jan 1997,5.94\r\n";

/// What the database sent for a window with no business day.
const std::string no_sonia_csv = "DATE,IUDSOIA\n";

/// A transport that gives its stored answers in order, and records each address asked.
class boe_transport final : public net::transport {
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

constexpr year_month_day boe_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_sonia_answer_reads_as_decimal_fixings_of_its_series", "[domain][boe]") {
    const auto fixings = parse_boe_csv(sonia_csv, "IUDSOIA");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 2);
    CHECK((*fixings)[0].date == boe_day(2026, 11, 4));
    CHECK((*fixings)[0].external_id == "IUDSOIA");
    CHECK((*fixings)[0].value == "0.037335");
    CHECK((*fixings)[1].date == boe_day(2026, 11, 5));
    CHECK((*fixings)[1].value == "0.039824");
}

TEST_CASE("a_sonia_answer_with_windows_line_ends_reads_the_same", "[domain][boe]") {
    const auto fixings = parse_boe_csv(first_sonia_csv, "IUDSOIA");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 1);
    CHECK((*fixings)[0].date == boe_day(1997, 1, 2));
    CHECK((*fixings)[0].value == "0.0594");
}

TEST_CASE("a_sonia_header_alone_reads_as_no_fixing", "[domain][boe]") {
    const auto fixings = parse_boe_csv(no_sonia_csv, "IUDSOIA");

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("a_malformed_boe_answer_is_an_error_and_not_a_fixing", "[domain][boe]") {
    CHECK_FALSE(parse_boe_csv("", "IUDSOIA").has_value());
    CHECK_FALSE(parse_boe_csv("<head><title>Object moved</title></head>", "IUDSOIA").has_value());
    CHECK_FALSE(parse_boe_csv("DATE,IUMABEDR\n04 Nov 2026,4.0\n", "IUDSOIA").has_value());
    CHECK_FALSE(parse_boe_csv("DATE,IUDSOIA\n2026-11-04,3.7335\n", "IUDSOIA").has_value());
    CHECK_FALSE(parse_boe_csv("DATE,IUDSOIA\n04 Nox 2026,3.7335\n", "IUDSOIA").has_value());
    CHECK_FALSE(parse_boe_csv("DATE,IUDSOIA\n04 Nov 2026\n", "IUDSOIA").has_value());
    CHECK_FALSE(parse_boe_csv("DATE,IUDSOIA\n04 Nov 2026,n/a\n", "IUDSOIA").has_value());
}

TEST_CASE("the_boe_fetcher_reads_the_window_of_its_series", "[domain][boe]") {
    boe_transport transport;
    transport.answers.push_back({200, "application/csv", sonia_csv});
    boe_fetcher fetcher(transport, "https://www.bankofengland.co.uk/");

    const auto universe = fetcher.universe("IUDSOIA");
    const auto fixings = fetcher.fixings("IUDSOIA", boe_day(2026, 11, 4), boe_day(2026, 11, 5));

    REQUIRE(universe.has_value());
    CHECK(*universe == std::vector<std::string>{"IUDSOIA"});
    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
    CHECK(transport.asked ==
          std::vector<std::string>{
              "https://www.bankofengland.co.uk/boeapps/database/"
              "_iadb-fromshowcolumns.asp?csv.x=yes&Datefrom=04/Nov/2026&Dateto=05/Nov/2026"
              "&SeriesCodes=IUDSOIA&CSVF=TN&UsingCodes=Y&VPD=Y&VFD=N"});
}

TEST_CASE("an_unknown_boe_series_is_an_error_and_not_an_empty_read", "[domain][boe]") {
    boe_transport transport;
    transport.answers.push_back({302, "text/html", "<head><title>Object moved</title></head>"});
    boe_fetcher fetcher(transport, "https://www.bankofengland.co.uk");

    CHECK_FALSE(fetcher.fixings("IUDSOIA", boe_day(2026, 11, 4), boe_day(2026, 11, 5)).has_value());
}

TEST_CASE("the_boe_fetcher_refuses_a_code_that_is_not_a_series_code", "[domain][boe]") {
    boe_transport transport;
    boe_fetcher fetcher(transport, "https://www.bankofengland.co.uk");

    CHECK_FALSE(fetcher.universe("iudsoia&x=1").has_value());
    CHECK_FALSE(fetcher.fixings("", boe_day(2026, 11, 4), boe_day(2026, 11, 5)).has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("a_sonia_answer_with_a_byte_order_mark_reads_the_same", "[domain][boe]") {
    const auto fixings = parse_boe_csv("\xEF\xBB\xBF" + sonia_csv, "IUDSOIA");

    REQUIRE(fixings.has_value());
    CHECK(fixings->size() == 2);
}
