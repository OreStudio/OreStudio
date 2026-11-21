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
#include "ores.http/routes/workflow/operations_routes.hpp"
#include "ores.http/routes/workflow/workflow_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <utility>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::workflow::operations_routes;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using ores::telemetry::messaging::get_nats_server_samples_response;
using ores::telemetry::messaging::nats_server_sample;
using ores::telemetry::messaging::nats_stream_sample;
using namespace ores::logging;

namespace {

const std::string services_pattern = "/api/operations/services";
const std::string logs_pattern = "/api/operations/logs";
const std::string bus_pattern = "/api/operations/bus";

const std::string logs_refused =
    "The logs read takes a range, the filters the journey names, and a page.";

const std::string bus_refused = "The message bus read takes a range of 15m, 1h or 6h.";

/// The epoch, as the wire writes an instant.
const std::chrono::system_clock::time_point epoch(std::chrono::seconds(0));

bool operations_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

/// A read of the logs at one query, with the range every read states.
http_request logs_at(const std::vector<std::pair<std::string, std::string>>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return req;
}

}

TEST_CASE("the_roster_reaches_the_service_and_takes_no_query", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the services roster with no service behind it";

    CHECK(answer_at<workflow_routes>(
              http_method::get, services_pattern, http_request{}, services_pattern)
              .status == http_status::bad_gateway);

    http_request req;
    req.query_params["scope"] = "all";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, services_pattern, req, services_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(operations_holds(refused.body, "scope"));
}

TEST_CASE("the_logs_name_a_range_or_they_are_refused", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the range a logs read states";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {}, {{"range", "1y"}}, {{"range", ""}}};
    for (const auto& query : queries) {
        const auto refused = answer_at<workflow_routes>(
            http_method::get, logs_pattern, logs_at(query), logs_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(operations_holds(refused.body, logs_refused));
    }
}

TEST_CASE("the_logs_take_the_filters_the_journey_names", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the filters a logs read does not name";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {{"range", "1h"}, {"level", "fatal"}},
        {{"range", "1h"}, {"source", "client"}},
        {{"range", "1h"}, {"offset", "-1"}},
        {{"range", "1h"}, {"offset", "ten"}},
        {{"range", "1h"}, {"limit", "0"}},
        {{"range", "1h"}, {"limit", "501"}}};
    for (const auto& query : queries) {
        const auto refused = answer_at<workflow_routes>(
            http_method::get, logs_pattern, logs_at(query), logs_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(operations_holds(refused.body, logs_refused));
    }
}

TEST_CASE("a_page_of_logs_reaches_the_service", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a logs read with no service behind it";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {{"range", "15m"}},
        {{"range", "24h"}, {"limit", "500"}},
        {{"range", "6h"},
         {"level", "error"},
         {"source", "server"},
         {"component", "ores.comms"},
         {"tag", "feed"},
         {"message", "timed out"}},
        {{"range", "1h"}, {"offset", "100"}, {"limit", "50"}}};
    for (const auto& query : queries)
        CHECK(
            answer_at<workflow_routes>(http_method::get, logs_pattern, logs_at(query), logs_pattern)
                .status == http_status::bad_gateway);
}

TEST_CASE("the_bus_names_a_range_or_it_is_refused", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the range a bus read states";

    const std::vector<std::vector<std::pair<std::string, std::string>>> queries = {
        {}, {{"range", "1y"}}, {{"range", ""}}, {{"range", "24h"}}};
    for (const auto& query : queries) {
        const auto refused =
            answer_at<workflow_routes>(http_method::get, bus_pattern, logs_at(query), bus_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(operations_holds(refused.body, bus_refused));
    }
}

TEST_CASE("the_bus_takes_no_field_but_its_range", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a field a bus read does not name";

    http_request req;
    req.query_params["range"] = "1h";
    req.query_params["stream"] = "ores_dev_workflow";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, bus_pattern, req, bus_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(operations_holds(refused.body, "stream"));
}

TEST_CASE("a_bus_read_reaches_the_service", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the bus read with no service behind it";

    for (const std::string range : {"15m", "1h", "6h"})
        CHECK(answer_at<workflow_routes>(
                  http_method::get, bus_pattern, logs_at({{"range", range}}), bus_pattern)
                  .status == http_status::bad_gateway);
}

TEST_CASE("the_bus_names_the_members_the_screen_reads", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the shape of the message bus answer";

    get_nats_server_samples_response servers;
    servers.success = true;
    for (const std::uint64_t in_msgs : {7u, 3u}) {
        nats_server_sample sample;
        sample.sampled_at = epoch + std::chrono::seconds(in_msgs);
        sample.in_msgs = in_msgs;
        sample.out_msgs = in_msgs + 1;
        sample.in_bytes = 1000 + in_msgs;
        sample.out_bytes = 2000 + in_msgs;
        sample.connections = 4;
        sample.mem_bytes = 8192;
        sample.slow_consumers = 1;
        servers.samples.push_back(sample);
    }

    nats_stream_sample stream;
    stream.sampled_at = epoch;
    stream.stream_name = "ores_dev_marketdata_ticks";
    stream.messages = 12;
    stream.bytes = 4096;
    stream.consumer_count = 2;

    const auto body = operations_routes::bus_json(servers, {stream});
    CHECK(operations_holds(body, "\"sampled_at\":\"1970-01-01 00:00:07.000000Z\""));
    CHECK(operations_holds(body, "\"in_msgs\":7"));
    CHECK(operations_holds(body, "\"out_msgs\":8"));
    CHECK(operations_holds(body, "\"in_bytes\":1007"));
    CHECK(operations_holds(body, "\"out_bytes\":2007"));
    CHECK(operations_holds(body, "\"connections\":4"));
    CHECK(operations_holds(body, "\"mem_bytes\":8192"));
    CHECK(operations_holds(body, "\"slow_consumers\":1"));
    CHECK(operations_holds(body, "\"stream_name\":\"ores_dev_marketdata_ticks\""));
    CHECK(operations_holds(body, "\"messages\":12"));
    CHECK(operations_holds(body, "\"bytes\":4096"));
    CHECK(operations_holds(body, "\"consumer_count\":2"));
    CHECK_FALSE(operations_holds(body, "success"));
}

TEST_CASE("a_window_with_no_sample_names_no_instant", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the bus answer for a window with no sample";

    CHECK(operations_routes::bus_json(get_nats_server_samples_response{}, {}) ==
          R"({"sampled_at":null,"samples":[],"streams":[]})");
}

TEST_CASE("the_bus_is_kept_privately_for_ten_seconds", "[workflow_operations]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the cache policy the bus carries";

    http_request req;
    const auto first = operations_routes::bus_answer(req, "{}");
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=10");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(operations_routes::bus_answer(revalidation, "{}").status == http_status::not_modified);
}
