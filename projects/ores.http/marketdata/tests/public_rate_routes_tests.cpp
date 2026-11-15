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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/marketdata/public_rate_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::public_rate_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;
namespace marketdata_domain = ores::marketdata::domain;

namespace {

const auto rates_series = "00000000-0000-0000-0000-0000000000b1";
const auto rates_other_series = "00000000-0000-0000-0000-0000000000b2";

bool rates_holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

marketdata_domain::market_fixing
fixing_of(const char* series, unsigned month, unsigned day, const char* value) {
    marketdata_domain::market_fixing row;
    row.series_id = boost::uuids::string_generator()(series);
    row.fixing_date = std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{month}, std::chrono::day{day}};
    row.value = value;
    return row;
}

http_request fixings_request(const std::string& series) {
    http_request req;
    req.query_params["series"] = series;
    return req;
}

http_status fixings_status(const http_request& req) {
    return answer_at<public_rate_routes>(http_method::get,
                                         "/api/marketdata/public-rates/fixings",
                                         req,
                                         "/api/marketdata/public-rates/fixings")
        .status;
}

http_status
terms_status(const std::string& address, const std::string& pattern, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<public_rate_routes>(http_method::post, address, req, pattern).status;
}

}

TEST_CASE("public_rates_state_a_series_oldest_first", "[public-rate]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a read of fixings";

    const auto answer = public_rate_routes::fixings_json(
        {boost::uuids::string_generator()(rates_series)},
        {fixing_of(rates_series, 4, 1, "1.2345"), fixing_of(rates_series, 3, 14, "1.1")});

    CHECK(rates_holds(answer, std::string(R"(")") + rates_series + R"(":[{"date":"2026-03-14")"));
    CHECK(rates_holds(answer, R"("value":"1.1")"));
    CHECK(rates_holds(answer, R"({"date":"2026-04-01","value":"1.2345"})"));
    CHECK(answer.find(R"("2026-03-14")") < answer.find(R"("2026-04-01")"));
}

TEST_CASE("public_rates_state_every_series_asked_for", "[public-rate]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a series the store holds no fixing for";

    const auto answer =
        public_rate_routes::fixings_json({boost::uuids::string_generator()(rates_series),
                                          boost::uuids::string_generator()(rates_other_series)},
                                         {fixing_of(rates_series, 3, 14, "1.1")});

    CHECK(rates_holds(answer, R"("00000000-0000-0000-0000-0000000000b2":[])"));
    CHECK(public_rate_routes::fixings_json({}, {}) == R"({"fixings":{}})");
}

TEST_CASE("public_rates_refuse_a_read_that_names_no_series", "[public-rate]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a read of fixings without a series";

    CHECK(fixings_status(http_request{}) == http_status::bad_request);
    CHECK(fixings_status(fixings_request("")) == http_status::bad_request);
    CHECK(fixings_status(fixings_request("not-an-id")) == http_status::bad_request);
    CHECK(fixings_status(fixings_request("00000000-0000-0000-0000-0000000000b1,"
                                         "00000000-0000-0000-0000-0000000000b2,"
                                         "00000000-0000-0000-0000-0000000000b3,"
                                         "00000000-0000-0000-0000-0000000000b4,"
                                         "00000000-0000-0000-0000-0000000000b5,"
                                         "00000000-0000-0000-0000-0000000000b6,"
                                         "00000000-0000-0000-0000-0000000000b7,"
                                         "00000000-0000-0000-0000-0000000000b8,"
                                         "00000000-0000-0000-0000-0000000000b9,"
                                         "00000000-0000-0000-0000-0000000000c1,"
                                         "00000000-0000-0000-0000-0000000000c2,"
                                         "00000000-0000-0000-0000-0000000000c3,"
                                         "00000000-0000-0000-0000-0000000000c4")) ==
          http_status::bad_request);
}

TEST_CASE("public_rates_refuse_a_read_without_a_service", "[public-rate]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the fixings' refusal when the service does not answer";

    CHECK(fixings_status(fixings_request(rates_series)) == http_status::bad_gateway);
}

TEST_CASE("public_rates_refuse_a_terms_body_they_do_not_read", "[public-rate]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the terms writes' bodies";

    const std::string accept = "/api/marketdata/public-rates/bloomberg/accept-terms";
    CHECK(terms_status(accept, "/api/marketdata/public-rates/{code}/accept-terms", "") ==
          http_status::bad_request);
    CHECK(terms_status(accept,
                       "/api/marketdata/public-rates/{code}/accept-terms",
                       R"({"version":0})") == http_status::bad_request);
    CHECK(terms_status(accept,
                       "/api/marketdata/public-rates/{code}/accept-terms",
                       R"({"version":"1"})") == http_status::bad_request);
    CHECK(terms_status("/api/marketdata/public-rates/accept-terms",
                       "/api/marketdata/public-rates/accept-terms",
                       R"({"providers":[]})") == http_status::bad_request);
    CHECK(terms_status("/api/marketdata/public-rates/accept-terms",
                       "/api/marketdata/public-rates/accept-terms",
                       R"({"providers":[{"code":"","version":1}]})") == http_status::bad_request);
}

TEST_CASE("public_rates_refuse_a_terms_write_without_a_service", "[public-rate]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the terms writes' refusal when the service does not answer";

    CHECK(terms_status("/api/marketdata/public-rates/bloomberg/accept-terms",
                       "/api/marketdata/public-rates/{code}/accept-terms",
                       R"({"version":2})") == http_status::bad_gateway);
    CHECK(terms_status("/api/marketdata/public-rates/bloomberg/withdraw-terms",
                       "/api/marketdata/public-rates/{code}/withdraw-terms",
                       "") == http_status::bad_gateway);
    CHECK(terms_status("/api/marketdata/public-rates/accept-terms",
                       "/api/marketdata/public-rates/accept-terms",
                       R"({"providers":[{"code":"bloomberg","version":2}]})") ==
          http_status::bad_gateway);
}
