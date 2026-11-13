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
#include "ores.http/routes/marketdata/adapter_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::adapter_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;
namespace marketdata_domain = ores::marketdata::domain;
namespace marketdata_messaging = ores::marketdata::messaging;

namespace {

bool holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

marketdata_messaging::adapter_status status_of(const char* adapter_id) {
    marketdata_messaging::adapter_status status;
    status.adapter_id = adapter_id;
    status.state = "running";
    return status;
}

marketdata_domain::feed feed_of(const char* name, const char* session_code) {
    marketdata_domain::feed feed;
    feed.name = name;
    feed.session_code = session_code;
    return feed;
}

marketdata_domain::session session_of(const char* code, const char* environment) {
    marketdata_domain::session session;
    session.code = code;
    session.environment = environment;
    return session;
}

}

TEST_CASE("adapters_state_the_kind_of_source_their_feed_reads", "[adapter]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the kind of source an adapter names";

    marketdata_messaging::list_adapter_status_response reply;
    reply.success = true;
    reply.adapters = {status_of("bloomberg"), status_of("orphan")};
    const auto answer = adapter_routes::adapters_json(
        reply, {feed_of("bloomberg", "S1")}, {session_of("S1", "supplier")});

    CHECK(holds(answer, R"("adapter_id":"bloomberg")"));
    CHECK(holds(answer, R"("kind":"supplier")"));
    CHECK(holds(answer, R"("adapter_id":"orphan")"));
    CHECK(holds(answer, R"("kind":"")"));
    CHECK(holds(answer, R"("success":true)"));
    CHECK(holds(answer, "\"message\":\"\""));
}

TEST_CASE("adapters_state_the_service_own_words", "[adapter]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the message an adapter read forwards";

    marketdata_messaging::list_adapter_status_response reply;
    reply.success = false;
    reply.message = "The adapters are starting.";

    const auto answer = adapter_routes::adapters_json(reply, {}, {});

    CHECK(holds(answer, R"("adapters":[])"));
    CHECK(holds(answer, R"("success":false)"));
    CHECK(holds(answer, R"("message":"The adapters are starting.")"));
}

TEST_CASE("adapters_refuse_a_read_without_a_service", "[adapter]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the adapters' refusal when the service does not answer";

    CHECK(answer_at<adapter_routes>(http_method::get,
                                    "/api/marketdata/adapters",
                                    http_request{},
                                    "/api/marketdata/adapters")
              .status == http_status::bad_gateway);
}
