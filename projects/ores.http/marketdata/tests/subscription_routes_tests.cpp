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
#include "ores.http/routes/marketdata/subscription_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::subscription_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;
namespace marketdata_domain = ores::marketdata::domain;

namespace {

const auto subscription_party_id = "00000000-0000-0000-0000-0000000000a1";
const auto subscription_feed_id = "00000000-0000-0000-0000-0000000000f1";

bool subscription_holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

marketdata_domain::subscription subscription_of(const char* provider) {
    marketdata_domain::subscription row;
    row.version = 3;
    row.party_id = boost::uuids::string_generator()(subscription_party_id);
    row.provider_code = provider;
    row.display = true;
    return row;
}

marketdata_domain::route route_of(const char* publisher) {
    marketdata_domain::route row;
    row.version = 2;
    row.party_id = boost::uuids::string_generator()(subscription_party_id);
    row.publisher_code = publisher;
    row.asset_class = "fx";
    row.feed_id = boost::uuids::string_generator()(subscription_feed_id);
    return row;
}

/// A request from a caller who acts in a party, which every write names.
http_request signed_in(const std::string& body) {
    http_request req;
    req.body = body;
    ores::security::jwt::jwt_claims claims;
    claims.party_id = subscription_party_id;
    req.authenticated_user = claims;
    return req;
}

http_status create_status(const std::string& body) {
    return answer_at<subscription_routes>(http_method::post,
                                          "/api/marketdata/subscriptions",
                                          signed_in(body),
                                          "/api/marketdata/subscriptions")
        .status;
}

http_status route_create_status(const std::string& body) {
    return answer_at<subscription_routes>(http_method::post,
                                          "/api/marketdata/routes",
                                          signed_in(body),
                                          "/api/marketdata/routes")
        .status;
}

http_status
removal_status(const std::string& address, const std::string& pattern, const std::string& body) {
    return answer_at<subscription_routes>(http_method::delete_, address, signed_in(body), pattern)
        .status;
}

}

TEST_CASE("subscriptions_state_a_party_its_rights_and_its_version", "[subscription]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a subscription's answer";

    const auto answer = subscription_routes::subscriptions_json({subscription_of("bloomberg")});

    CHECK(subscription_holds(answer, R"("subscriptions":[{)"));
    CHECK(subscription_holds(answer, R"("provider_code":"bloomberg")"));
    CHECK(subscription_holds(answer, R"("party_id":"00000000-0000-0000-0000-0000000000a1")"));
    CHECK(subscription_holds(answer, R"("version":3)"));
    CHECK(subscription_holds(answer, R"("display":true)"));
    CHECK(subscription_holds(answer, R"("non_display":false)"));
    CHECK(subscription_holds(answer, R"("derived":false)"));
    CHECK(subscription_routes::subscriptions_json({}) == R"({"subscriptions":[]})");
}

TEST_CASE("routes_state_the_feed_that_serves_a_publisher", "[subscription]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a route's answer";

    const auto answer = subscription_routes::routes_json({route_of("bloomberg")});

    CHECK(subscription_holds(answer, R"("routes":[{)"));
    CHECK(subscription_holds(answer, R"("publisher_code":"bloomberg")"));
    CHECK(subscription_holds(answer, R"("feed_id":"00000000-0000-0000-0000-0000000000f1")"));
    CHECK(subscription_holds(answer, R"("asset_class":"fx")"));
    CHECK(subscription_routes::routes_json({}) == R"({"routes":[]})");
}

TEST_CASE("subscriptions_refuse_a_read_without_a_service", "[subscription]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info)
        << "Testing the subscriptions' refusal when the service does not answer";

    CHECK(answer_at<subscription_routes>(http_method::get,
                                         "/api/marketdata/subscriptions",
                                         http_request{},
                                         "/api/marketdata/subscriptions")
              .status == http_status::bad_gateway);
    CHECK(answer_at<subscription_routes>(
              http_method::get, "/api/marketdata/routes", http_request{}, "/api/marketdata/routes")
              .status == http_status::bad_gateway);
}

TEST_CASE("subscriptions_refuse_a_create_without_a_right", "[subscription]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a subscription that carries no right";

    CHECK(create_status(R"({"provider_code":"bloomberg"})") == http_status::bad_request);
    CHECK(create_status(R"({"provider_code":"bloomberg","display":false,"non_display":false,)"
                        R"("derived":false})") == http_status::bad_request);
    CHECK(create_status(R"({"provider_code":"bloomberg","non_display":true})") ==
          http_status::bad_gateway);
}

TEST_CASE("subscriptions_refuse_a_create_naming_no_party", "[subscription]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create from a caller who acts in no party";

    http_request req;
    req.body = R"({"provider_code":"bloomberg","display":true})";
    CHECK(answer_at<subscription_routes>(http_method::post,
                                         "/api/marketdata/subscriptions",
                                         req,
                                         "/api/marketdata/subscriptions")
              .status == http_status::bad_request);
}

TEST_CASE("subscriptions_refuse_a_body_they_do_not_read", "[subscription]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's body";

    CHECK(create_status(R"({"provider_code":"","display":true})") == http_status::bad_request);
    CHECK(create_status(R"({"provider_code":"bloomberg","display":true,"right":true})") ==
          http_status::bad_request);
    CHECK(create_status("not a body") == http_status::bad_request);
    CHECK(route_create_status(R"({"publisher_code":"bloomberg","feed_id":"not-an-id"})") ==
          http_status::bad_request);
    CHECK(route_create_status(R"({"publisher_code":"bloomberg"})") == http_status::bad_request);
}

TEST_CASE("subscriptions_refuse_a_removal_that_states_no_reason", "[subscription]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a removal's body";

    const std::string subscription = "/api/marketdata/subscriptions/bloomberg";
    const std::string subscription_pattern = "/api/marketdata/subscriptions/{provider}";
    const std::string route = "/api/marketdata/routes/bloomberg";
    const std::string route_pattern = "/api/marketdata/routes/{publisher}";

    CHECK(removal_status(subscription, subscription_pattern, R"({"version":1})") ==
          http_status::bad_request);
    CHECK(removal_status(subscription,
                         subscription_pattern,
                         R"({"version":1,"reason_code":"system.deleted"})") ==
          http_status::bad_gateway);
    CHECK(removal_status(subscription,
                         subscription_pattern,
                         R"({"version":-1,"reason_code":"system.deleted"})") ==
          http_status::bad_request);
    CHECK(removal_status(subscription,
                         subscription_pattern,
                         R"({"version":1,"reason_code":"system.deleted","note":"why"})") ==
          http_status::bad_request);
    CHECK(removal_status(route, route_pattern, R"({"version":1})") == http_status::bad_request);
    CHECK(removal_status(route, route_pattern, R"({"version":1,"reason_code":"system.deleted"})") ==
          http_status::bad_gateway);
}
