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
#include "ores.http/routes/reporting/pricing_engine_import.hpp"
#include "ores.logging/logger.hpp"
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
using ores::http::routes::reporting::pricing_engine_import;
using namespace ores::logging;

namespace {

const std::string import_preview_address = "/api/reporting/pricing-engines/ore-import/preview";
const std::string import_address = "/api/reporting/pricing-engines/ore-import";

/**
 * @brief The answer an import route gives a request, on a client that is not
 * connected, so any forward fails.
 */
http_status import_status_of(const std::string& address, http_request req) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    pricing_engine_import::register_routes(
        table, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto route = table->match(http_method::post, address, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == address);
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

TEST_CASE("an_import_preview_refuses_a_query_it_does_not_read", "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import preview's query";

    http_request req;
    req.body = R"({"content":"<PricingEngines/>"})";
    req.query_params["party"] = "acme";
    CHECK(import_status_of(import_preview_address, req) == http_status::bad_request);
}

TEST_CASE("an_import_preview_refuses_a_body_with_no_document", "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import preview's document";

    http_request empty;
    empty.body = R"({"content":""})";
    CHECK(import_status_of(import_preview_address, empty) == http_status::bad_request);

    http_request absent;
    absent.body = R"({"configurationId":null})";
    CHECK(import_status_of(import_preview_address, absent) == http_status::bad_request);
}

TEST_CASE("an_import_preview_refuses_a_configuration_that_is_not_an_id",
          "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import preview's configuration";

    http_request req;
    req.body = R"({"content":"<PricingEngines/>","configurationId":"amc"})";
    CHECK(import_status_of(import_preview_address, req) == http_status::bad_request);
}

TEST_CASE("an_import_preview_with_no_service_is_refused_as_upstream", "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import preview with no service behind it";

    http_request req;
    req.body = R"({"content":"<PricingEngines/>"})";
    CHECK(import_status_of(import_preview_address, req) == http_status::bad_gateway);
}

TEST_CASE("an_import_refuses_a_query_it_does_not_read", "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import's query";

    http_request req;
    req.body = R"({"content":"<PricingEngines/>","intent":{"reason_code":"seed"}})";
    req.query_params["party"] = "acme";
    CHECK(import_status_of(import_address, req) == http_status::bad_request);
}

TEST_CASE("an_import_refuses_a_name_longer_than_a_configuration_may_hold",
          "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import's new name";

    http_request req;
    req.body = R"({"content":"<PricingEngines/>","newName":")" + std::string(201, 'a') +
               R"(","intent":{"reason_code":"seed"}})";
    CHECK(import_status_of(import_address, req) == http_status::bad_request);
}

TEST_CASE("an_import_refuses_a_configuration_that_is_not_an_id", "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import's configuration";

    http_request req;
    req.body = R"({"content":"<PricingEngines/>","configurationId":"amc",)"
               R"("intent":{"reason_code":"seed"}})";
    CHECK(import_status_of(import_address, req) == http_status::bad_request);
}

TEST_CASE("an_import_with_no_service_is_refused_as_upstream", "[pricing_engine_import]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the import with no service behind it";

    http_request req;
    req.body = R"({"content":"<PricingEngines/>","intent":{"reason_code":"seed"}})";
    CHECK(import_status_of(import_address, req) == http_status::bad_gateway);
}
