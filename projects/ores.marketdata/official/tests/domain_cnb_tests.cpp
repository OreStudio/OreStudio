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
#include "ores.marketdata.official/domain/cnb.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// The first lines of the 2026 file as the bank sent it on 2026-11-12.
const std::string year_2026 = "Date|Volume (CZK mio)|Rate\n"
                              "02 Jan 2026|11671|3.07\n"
                              "05 Jan 2026|15597|3.21\n"
                              "06 Jan 2026|7576|2.88\n"
                              "\n";

/// The last lines of the 2025 file, in the same form.
const std::string year_2025 = "Date|Volume (CZK mio)|Rate\n"
                              "30 Dec 2025|10422|3.43\n"
                              "31 Dec 2025|6315|3.10\n";

/// What the bank sent on 2026-11-12 for 2027, a year with no rate yet.
const std::string empty_year = "\n";

/// A transport that gives its stored answers in order, and records each address asked.
class cnb_transport final : public net::transport {
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

constexpr year_month_day cnb_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_yearly_czeonia_file_reads_as_decimal_fixings", "[domain][cnb]") {
    const auto fixings = parse_cnb_czeonia(year_2026, "CZEONIA");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 3);
    CHECK((*fixings)[0].date == cnb_day(2026, 1, 2));
    CHECK((*fixings)[0].external_id == "CZEONIA");
    CHECK((*fixings)[0].value == "0.0307");
    CHECK((*fixings)[2].date == cnb_day(2026, 1, 6));
    CHECK((*fixings)[2].value == "0.0288");
}

TEST_CASE("a_year_with_no_czeonia_rate_reads_as_no_fixing", "[domain][cnb]") {
    const auto fixings = parse_cnb_czeonia(empty_year, "CZEONIA");

    REQUIRE(fixings.has_value());
    CHECK(fixings->empty());
}

TEST_CASE("a_malformed_czeonia_file_is_an_error_and_not_a_fixing", "[domain][cnb]") {
    CHECK_FALSE(parse_cnb_czeonia("<html>Not found</html>", "CZEONIA").has_value());
    CHECK_FALSE(parse_cnb_czeonia("Date|Volume (CZK mio)|Rate\n2026-01-02|11671|3.07\n", "CZEONIA")
                    .has_value());
    CHECK_FALSE(parse_cnb_czeonia("Date|Volume (CZK mio)|Rate\n02 Jan 2026|11671|n/a\n", "CZEONIA")
                    .has_value());
    CHECK_FALSE(
        parse_cnb_czeonia("Date|Volume (CZK mio)|Rate\n02 Jan 2026\n", "CZEONIA").has_value());
}

TEST_CASE("the_cnb_fetcher_serves_only_czeonia", "[domain][cnb]") {
    cnb_transport transport;
    cnb_fetcher fetcher(transport, "https://www.cnb.cz");

    CHECK(fetcher.universe("CZEONIA").has_value());
    CHECK_FALSE(fetcher.universe("PRIBOR").has_value());
    CHECK_FALSE(fetcher.fixings("PRIBOR", cnb_day(2026, 1, 1), cnb_day(2026, 1, 31)).has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("a_window_across_years_reads_each_yearly_file_and_keeps_its_days", "[domain][cnb]") {
    cnb_transport transport;
    transport.answers.push_back({200, "text/plain;charset=UTF-8", year_2025});
    transport.answers.push_back({200, "text/plain;charset=UTF-8", year_2026});
    cnb_fetcher fetcher(transport, "https://www.cnb.cz/");

    const auto fixings = fetcher.fixings("CZEONIA", cnb_day(2025, 12, 31), cnb_day(2026, 1, 5));

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 3);
    CHECK((*fixings)[0].date == cnb_day(2025, 12, 31));
    CHECK((*fixings)[2].date == cnb_day(2026, 1, 5));
    REQUIRE(transport.asked.size() == 2);
    CHECK(transport.asked[0] ==
          "https://www.cnb.cz/en/financial-markets/money-market/czeonia-reference-interest-rate/"
          "yearly.txt?calendarYear=2025");
    CHECK(transport.asked[1].ends_with("calendarYear=2026"));
}

TEST_CASE("a_refused_yearly_file_is_an_error", "[domain][cnb]") {
    cnb_transport transport;
    transport.answers.push_back({503, "text/html", "Service Unavailable"});
    cnb_fetcher fetcher(transport, "https://www.cnb.cz");

    CHECK_FALSE(fetcher.fixings("CZEONIA", cnb_day(2026, 1, 1), cnb_day(2026, 1, 31)).has_value());
}
