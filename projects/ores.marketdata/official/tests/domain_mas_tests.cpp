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
#include "ores.marketdata.official/domain/mas.hpp"
#include <catch2/catch_test_macros.hpp>
#include <deque>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace ores::marketdata::official::domain;
using namespace std::chrono;
namespace net = ores::http::client::net;

/// The head of the page, with the three hidden fields, shortened.
const std::string page_html =
    "<form method=\"post\" action=\"./DomesticInterestRates.aspx\" id=\"form1\">"
    "<input type=\"hidden\" name=\"__VIEWSTATE\" id=\"__VIEWSTATE\" value=\"AxCq/loG+J=\" />"
    "<input type=\"hidden\" name=\"__VIEWSTATEGENERATOR\" id=\"__VIEWSTATEGENERATOR\" "
    "value=\"A969171E\" />"
    "<input type=\"hidden\" name=\"__EVENTVALIDATION\" id=\"__EVENTVALIDATION\" "
    "value=\"oXvQO3al\" /></form>";

/// A download of SORA for October and November 2026 as MAS sent it on 2026-11-12, shortened.
const std::string sora_csv = "MAS: Financial Database - Domestic Interest Rates\n"
                             "\n"
                             "Domestic Interest Rates (Daily)\n"
                             "Oct 2026 to Nov 2026\n"
                             "\n"
                             "\n"
                             "SORA Value Date,,,SORA Publication Date,SORA\n"
                             "2026,Oct,01,02 Oct 2026,1.2965\n"
                             ",,02,05 Oct 2026,1.3724\n"
                             ",,30,02 Nov 2026,1.5526\n"
                             ",Nov,02,03 Nov 2026,1.4160\n"
                             ",,11,12 Nov 2026,1.5992\n"
                             "\n"
                             "\n"
                             "\" The SORA Publication Date is the same date as the SORA "
                             "Compounded Index Value Date.\"\n";

/// A transport that gives its stored answers in order, and records each request.
class mas_transport final : public net::transport {
public:
    std::deque<net::response> answers;
    std::vector<std::string> asked;
    std::vector<std::string> posted;

    std::expected<net::response, net::failure> get(std::string_view url,
                                                   const net::request_limits&) override {
        asked.emplace_back(url);
        return next();
    }

    std::expected<net::response, net::failure> post(std::string_view url,
                                                    std::string_view,
                                                    std::string body,
                                                    const net::request_limits&) override {
        asked.emplace_back(url);
        posted.push_back(std::move(body));
        return next();
    }

private:
    std::expected<net::response, net::failure> next() {
        if (answers.empty())
            return std::unexpected(net::failure{net::failure_kind::network, "no stored answer"});
        auto answer = std::move(answers.front());
        answers.pop_front();
        return answer;
    }
};

constexpr year_month_day mas_day(int y, int m, int d) {
    return year_month_day{year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
}

}

TEST_CASE("a_sora_download_reads_as_decimal_fixings_of_its_value_dates", "[domain][mas]") {
    const auto fixings = parse_mas_sora_csv(sora_csv, "SORA");

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 5);
    CHECK((*fixings)[0].date == mas_day(2026, 10, 1));
    CHECK((*fixings)[0].external_id == "SORA");
    CHECK((*fixings)[0].value == "0.012965");
    CHECK((*fixings)[2].date == mas_day(2026, 10, 30));
    CHECK((*fixings)[3].date == mas_day(2026, 11, 2));
    CHECK((*fixings)[4].value == "0.015992");
}

TEST_CASE("a_malformed_sora_download_is_an_error_and_not_a_fixing", "[domain][mas]") {
    CHECK_FALSE(parse_mas_sora_csv("<html>Maintenance</html>", "SORA").has_value());
    CHECK_FALSE(parse_mas_sora_csv("SORA Value Date,,,SORA Publication Date,SORA Index\n", "SORA")
                    .has_value());
    CHECK_FALSE(
        parse_mas_sora_csv(
            "SORA Value Date,,,SORA Publication Date,SORA\n,,02,05 Oct 2026,1.3724\n", "SORA")
            .has_value());
    CHECK_FALSE(parse_mas_sora_csv(
                    "SORA Value Date,,,SORA Publication Date,SORA\n2026,Oct,01,x,abc\n", "SORA")
                    .has_value());
}

TEST_CASE("the_hidden_fields_of_the_mas_page_are_read", "[domain][mas]") {
    const auto state = parse_mas_page_state(page_html);

    REQUIRE(state.has_value());
    CHECK(state->view_state == "AxCq/loG+J=");
    CHECK(state->view_state_generator == "A969171E");
    CHECK(state->event_validation == "oXvQO3al");
    CHECK_FALSE(parse_mas_page_state("<html>Maintenance</html>").has_value());
}

TEST_CASE("the_mas_fetcher_serves_only_sora", "[domain][mas]") {
    mas_transport transport;
    mas_fetcher fetcher(transport, "https://eservices.mas.gov.sg");

    CHECK(fetcher.universe("SORA").has_value());
    CHECK_FALSE(fetcher.universe("SORA Index").has_value());
    CHECK(transport.asked.empty());
}

TEST_CASE("the_mas_fetcher_posts_the_page_back_for_its_window_and_keeps_its_days",
          "[domain][mas]") {
    mas_transport transport;
    transport.answers.push_back({200, "text/html", page_html});
    transport.answers.push_back({200, "text/csv; charset=utf-8", sora_csv});
    mas_fetcher fetcher(transport, "https://eservices.mas.gov.sg/");

    const auto fixings = fetcher.fixings("SORA", mas_day(2026, 10, 2), mas_day(2026, 11, 2));

    REQUIRE(fixings.has_value());
    REQUIRE(fixings->size() == 3);
    CHECK((*fixings)[0].date == mas_day(2026, 10, 2));
    CHECK((*fixings)[2].date == mas_day(2026, 11, 2));
    REQUIRE(transport.asked.size() == 2);
    CHECK(transport.asked[0] ==
          "https://eservices.mas.gov.sg/statistics/dir/DomesticInterestRates.aspx");
    REQUIRE(transport.posted.size() == 1);
    const auto& body = transport.posted[0];
    CHECK(body.find("__VIEWSTATE=AxCq%2FloG%2BJ%3D") != std::string::npos);
    CHECK(body.find("ctl00%24ContentPlaceHolder1%24StartYearDropDownList=2026") !=
          std::string::npos);
    CHECK(body.find("ctl00%24ContentPlaceHolder1%24StartMonthDropDownList=10") !=
          std::string::npos);
    CHECK(body.find("ctl00%24ContentPlaceHolder1%24EndMonthDropDownList=11") != std::string::npos);
    CHECK(body.find("ctl00%24ContentPlaceHolder1%24ColumnsCheckBoxList%2413=on") !=
          std::string::npos);
    CHECK(body.find("ctl00%24ContentPlaceHolder1%24Button2=Download") != std::string::npos);
}

TEST_CASE("a_refused_mas_download_is_an_error", "[domain][mas]") {
    mas_transport transport;
    transport.answers.push_back({200, "text/html", page_html});
    transport.answers.push_back({500, "text/html", "Server Error"});
    mas_fetcher fetcher(transport, "https://eservices.mas.gov.sg");

    CHECK_FALSE(fetcher.fixings("SORA", mas_day(2026, 10, 1), mas_day(2026, 10, 31)).has_value());
}
