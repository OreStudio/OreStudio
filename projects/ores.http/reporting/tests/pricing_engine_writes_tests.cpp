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
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/reporting/pricing_engine_writes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::reporting::pricing_engine_writes;
using namespace ores::logging;

namespace {

const std::string pricing_writes_address = "/api/reporting/pricing-engines";
const std::string pricing_one_engine_address =
    "/api/reporting/pricing-engines/00000000-0000-0000-0000-0000000000e1";

/// A document the form would send, with the two lists replaced as a case needs.
std::string pricing_document_body(const std::string& products, const std::string& parameters) {
    return R"({"name":"amc","description":"","variant":"amc","products":)" + products +
           R"(,"parameters":)" + parameters + R"(,"intent":{"reason_code":"seed"}})";
}

/**
 * @brief The answer a pricing engine write gives a request, on a client that is
 * not connected, so any forward fails.
 */
http_status pricing_writes_status_of(http_method method,
                                     const std::string& address,
                                     http_request req,
                                     const std::string& expected_pattern) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    pricing_engine_writes::register_write_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(method, address, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == expected_pattern);
    REQUIRE(route->requires_auth);
    boost::asio::io_context io;
    std::optional<ores::http::domain::http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return answer->status;
}

}

TEST_CASE("a_create_refuses_a_query_it_does_not_read", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's query";

    http_request req;
    req.body = pricing_document_body("[]", "[]");
    req.query_params["party"] = "acme";
    CHECK(pricing_writes_status_of(
              http_method::post, pricing_writes_address, req, pricing_writes_address) ==
          http_status::bad_request);
}

TEST_CASE("a_create_refuses_a_member_it_does_not_read", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's members";

    http_request req;
    req.body = R"({"name":"amc","products":[],"parameters":[],)"
               R"("intent":{"reason_code":"seed"},"extra":1})";
    CHECK(pricing_writes_status_of(
              http_method::post, pricing_writes_address, req, pricing_writes_address) ==
          http_status::bad_request);
}

TEST_CASE("a_create_refuses_a_product_with_no_type", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's product type";

    http_request req;
    req.body = pricing_document_body(
        R"([{"id":"00000000-0000-0000-0000-0000000000a1","version":0,"typeCode":"",)"
        R"("model":"DiscountedCashflows","engine":"DiscountingSwapEngine","position":0}])",
        "[]");
    CHECK(pricing_writes_status_of(
              http_method::post, pricing_writes_address, req, pricing_writes_address) ==
          http_status::bad_request);
}

TEST_CASE("a_create_refuses_a_parameter_whose_product_is_not_an_id", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's parameter product";

    http_request req;
    req.body = pricing_document_body(
        "[]",
        R"([{"id":"00000000-0000-0000-0000-0000000000b1","version":0,"productId":"swap",)"
        R"("scope":"engine","name":"DiscountFactor","value":"1.0","position":0}])");
    CHECK(pricing_writes_status_of(
              http_method::post, pricing_writes_address, req, pricing_writes_address) ==
          http_status::bad_request);
}

TEST_CASE("a_create_refuses_more_products_than_a_document_holds", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create's product count";

    std::string products = "[";
    for (int i = 0; i != 501; ++i) {
        if (i != 0)
            products += ',';
        products += R"({"id":"00000000-0000-0000-0000-00000000)" + std::to_string(1000 + i) +
                    R"(","version":0,"typeCode":"Swap","model":"DiscountedCashflows",)"
                    R"("engine":"DiscountingSwapEngine","position":)" +
                    std::to_string(i) + "}";
    }
    products += ']';

    http_request req;
    req.body = pricing_document_body(products, "[]");
    CHECK(pricing_writes_status_of(
              http_method::post, pricing_writes_address, req, pricing_writes_address) ==
          http_status::bad_request);
}

TEST_CASE("a_create_with_no_service_is_refused_as_upstream", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a create with no service behind it";

    http_request req;
    req.body = pricing_document_body("[]", "[]");
    CHECK(pricing_writes_status_of(
              http_method::post, pricing_writes_address, req, pricing_writes_address) ==
          http_status::bad_gateway);
}

TEST_CASE("a_change_refuses_an_id_that_is_not_an_id", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a change's id";

    http_request req;
    req.body = R"({"name":"amc","products":[],"parameters":[],)"
               R"("intent":{"reason_code":"seed"},"version":1})";
    CHECK(pricing_writes_status_of(http_method::put,
                                   "/api/reporting/pricing-engines/not-an-id",
                                   req,
                                   "/api/reporting/pricing-engines/{id}") ==
          http_status::bad_request);
}

TEST_CASE("a_change_refuses_a_body_that_states_no_version", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a change's version";

    http_request req;
    req.body = pricing_document_body("[]", "[]");
    CHECK(pricing_writes_status_of(http_method::put,
                                   pricing_one_engine_address,
                                   req,
                                   "/api/reporting/pricing-engines/{id}") ==
          http_status::bad_request);

    http_request zero;
    zero.body = R"({"name":"amc","products":[],"parameters":[],)"
                R"("intent":{"reason_code":"seed"},"version":0})";
    CHECK(pricing_writes_status_of(http_method::put,
                                   pricing_one_engine_address,
                                   zero,
                                   "/api/reporting/pricing-engines/{id}") ==
          http_status::bad_request);
}

TEST_CASE("a_change_with_no_service_is_refused_as_upstream", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a change with no service behind it";

    http_request req;
    req.body = R"({"name":"amc","products":[],"parameters":[],)"
               R"("intent":{"reason_code":"seed"},"version":1})";
    CHECK(pricing_writes_status_of(http_method::put,
                                   pricing_one_engine_address,
                                   req,
                                   "/api/reporting/pricing-engines/{id}") ==
          http_status::bad_gateway);
}

TEST_CASE("a_removal_refuses_an_id_that_is_not_an_id", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a removal's id";

    CHECK(pricing_writes_status_of(http_method::delete_,
                                   "/api/reporting/pricing-engines/not-an-id",
                                   http_request{},
                                   "/api/reporting/pricing-engines/{id}") ==
          http_status::bad_request);
}

TEST_CASE("a_removal_with_no_service_is_refused_as_upstream", "[pricing_engine_writes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a removal with no service behind it";

    CHECK(pricing_writes_status_of(http_method::delete_,
                                   pricing_one_engine_address,
                                   http_request{},
                                   "/api/reporting/pricing-engines/{id}") ==
          http_status::bad_gateway);
}
