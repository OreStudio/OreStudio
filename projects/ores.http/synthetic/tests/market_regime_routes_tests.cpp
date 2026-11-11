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
#include "ores.http/routes/synthetic/market_regime_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::synthetic::market_regime_routes;
using ores::http::synthetic::tests::answer_at;
using namespace ores::logging;

namespace {

http_status regime_status(http_method method, const std::string& path, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<market_regime_routes>(method, path, req, path).status;
}

http_status history_status(const std::map<std::string, std::string>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    const std::string path = "/api/synthetic/market-history";
    return answer_at<market_regime_routes>(http_method::get, path, req, path).status;
}

const std::string regime_path = "/api/synthetic/market-regime";
const std::string settings_path = "/api/synthetic/market-regime/settings";

std::string settings(const std::string& decay, const std::string& tail) {
    return R"({"wakeRate":0.1,"calmRate":0.2,"jump":0.5,"decay":)" + decay +
           R"(,"speed":60,"tailLoading":)" + tail + "}";
}

}

TEST_CASE("market_regime_switch_refuses_a_mode_it_does_not_know", "[market_regime]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the regime switch's refusals";

    CHECK(regime_status(http_method::post, regime_path, "") == http_status::bad_request);
    CHECK(regime_status(http_method::post, regime_path, R"({"mode":"hot"})") ==
          http_status::bad_request);
    CHECK(regime_status(http_method::post, regime_path, R"({"mode":"calm","for":60})") ==
          http_status::bad_request);
    CHECK(regime_status(http_method::post, regime_path, R"({"mode":"active"})") ==
          http_status::bad_gateway);
    CHECK(regime_status(http_method::get, regime_path, "") == http_status::bad_gateway);
}

TEST_CASE("market_regime_settings_refuse_numbers_the_market_cannot_run_on", "[market_regime]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the settings' refusals";

    CHECK(regime_status(http_method::put, settings_path, settings("0", "1")) ==
          http_status::bad_request);
    CHECK(regime_status(http_method::put, settings_path, settings("1", "10.5")) ==
          http_status::bad_request);
    CHECK(regime_status(http_method::put, settings_path, settings("1", "-1")) ==
          http_status::bad_request);
    CHECK(regime_status(http_method::put, settings_path, R"({"wakeRate":0.1})") ==
          http_status::bad_request);
    CHECK(regime_status(http_method::put,
                        settings_path,
                        R"({"wakeRate":0.1,"calmRate":0.2,"jump":0,"decay":1,"speed":60,)"
                        R"("tailLoading":0,"seed":7})") == http_status::bad_request);
    CHECK(regime_status(http_method::put, settings_path, settings("1", "10")) ==
          http_status::bad_gateway);
}

TEST_CASE("market_history_refuses_a_query_it_does_not_know", "[market_regime]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the history read's refusals";

    CHECK(history_status({{"resolution", "minute"}}) == http_status::bad_request);
    CHECK(history_status({{"seconds", "0"}}) == http_status::bad_request);
    CHECK(history_status({{"seconds", "3601"}}) == http_status::bad_request);
    CHECK(history_status({{"seconds", ""}}) == http_status::bad_request);
    CHECK(history_status({{"seconds", "ten"}}) == http_status::bad_request);
    CHECK(history_status({{"since", "1"}}) == http_status::bad_request);
    CHECK(history_status({}) == http_status::bad_gateway);
    CHECK(history_status({{"resolution", "tenth"}, {"seconds", "2.5"}}) ==
          http_status::bad_gateway);
}
