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
#include "ores.http/routes/reporting/run_file_routes.hpp"
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
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::reporting::run_file_routes;
using namespace ores::logging;

namespace {

/**
 * @brief The answer the unit gives a request, on a client that is not
 * connected, so any forward fails.
 */
http_response answer_at(http_method method,
                        const std::string& path,
                        http_request req,
                        const std::string& expected_pattern) {
    ores::nats::service::nats_client session;
    auto table = std::make_shared<ores::http::net::router>();
    run_file_routes::register_routes(table,
                                     std::make_shared<ores::http::openapi::endpoint_registry>(),
                                     session,
                                     ".");
    const auto route = table->match(method, path, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == expected_pattern);
    REQUIRE(route->requires_auth);
    boost::asio::io_context io;
    std::optional<http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return *answer;
}

const auto a_run = "00000000-0000-0000-0000-0000000000c1";

}

TEST_CASE("a_run_files_address_that_names_no_uuid_is_refused", "[run_file]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the run file routes' addresses";

    CHECK(answer_at(http_method::get,
                    "/api/reporting/runs/not-an-id/files",
                    http_request{},
                    "/api/reporting/runs/{id}/files")
              .status == http_status::bad_request);
    CHECK(answer_at(http_method::get,
                    "/api/reporting/runs/not-an-id/files/some-file",
                    http_request{},
                    "/api/reporting/runs/{id}/files/{file}")
              .status == http_status::bad_request);
    CHECK(answer_at(http_method::get,
                    "/api/reporting/runs/not-an-id/files/some-file/info",
                    http_request{},
                    "/api/reporting/runs/{id}/files/{file}/info")
              .status == http_status::bad_request);
}

TEST_CASE("a_run_whose_services_cannot_answer_is_refused_as_upstream", "[run_file]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a run files read with no service behind it";

    CHECK(answer_at(http_method::get,
                    "/api/reporting/runs/" + std::string(a_run) + "/files",
                    http_request{},
                    "/api/reporting/runs/{id}/files")
              .status == http_status::bad_gateway);
}

TEST_CASE("a_run_files_read_refuses_a_query_parameter_it_does_not_name", "[run_file]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the run file routes' strict query";

    http_request req;
    req.query_params["depth"] = "2";
    CHECK(answer_at(http_method::get,
                    "/api/reporting/runs/" + std::string(a_run) + "/files",
                    req,
                    "/api/reporting/runs/{id}/files")
              .status == http_status::bad_request);
}
