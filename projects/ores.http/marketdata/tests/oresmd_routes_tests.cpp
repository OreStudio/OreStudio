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
#include "ores.http/routes/marketdata/oresmd_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::marketdata::oresmd_routes;
using ores::http::marketdata::tests::answer_at;
using namespace ores::logging;

namespace {

http_status oresmd_status(const std::string& address, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<oresmd_routes>(http_method::post, address, req, address).status;
}

}

TEST_CASE("oresmd_read_the_schema_without_a_service", "[oresmd]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the schema's refusal when the service does not answer";

    http_request req;
    CHECK(
        answer_at<oresmd_routes>(http_method::get, "/api/oresmd/schema", req, "/api/oresmd/schema")
            .status == http_status::bad_gateway);
}

TEST_CASE("oresmd_read_one_uri_without_a_service", "[oresmd]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a parse and a compose when the service does not answer";

    CHECK(oresmd_status("/api/oresmd/parse", R"({"uri":"oresmd://fx/EUR"})") ==
          http_status::bad_gateway);
    CHECK(oresmd_status("/api/oresmd/compose", R"({"kind":"fx","instrument":"EUR/USD"})") ==
          http_status::bad_gateway);
}

TEST_CASE("oresmd_refuse_a_body_they_do_not_read", "[oresmd]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the codec routes' bodies";

    CHECK(oresmd_status("/api/oresmd/parse", "") == http_status::bad_request);
    CHECK(oresmd_status("/api/oresmd/parse", R"({"uri":"oresmd://fx/EUR","kind":"fx"})") ==
          http_status::bad_request);
    CHECK(oresmd_status("/api/oresmd/compose", R"({"kind":"fx"})") == http_status::bad_request);
    CHECK(oresmd_status("/api/oresmd/compose",
                        R"({"kind":"fx","instrument":"EUR/USD","colour":"red"})") ==
          http_status::bad_request);
}

TEST_CASE("oresmd_refuse_a_batch_larger_than_a_page", "[oresmd]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the limits on a batch read";

    std::string uris;
    for (int at = 0; at < 201; ++at) {
        uris += (at == 0 ? "" : ",") + std::string(R"("oresmd://fx/EUR")");
    }
    CHECK(oresmd_status("/api/oresmd/parse-many", "{\"uris\":[" + uris + "]}") ==
          http_status::bad_request);
    CHECK(oresmd_status("/api/oresmd/parse-many", R"({"uris":[]})") == http_status::ok);
}

TEST_CASE("oresmd_refuse_a_field_set_larger_than_the_codec_reads", "[oresmd]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the limits on a compose";

    std::string fields;
    for (int at = 0; at < 65; ++at) {
        fields += (at == 0 ? "" : ",") + std::string(R"({"name":"ccy","text":"USD"})");
    }
    CHECK(oresmd_status("/api/oresmd/compose",
                        R"({"kind":"fx","instrument":"EUR/USD","fields":[)" + fields + "]}") ==
          http_status::bad_request);
    CHECK(oresmd_status("/api/oresmd/compose",
                        R"({"kind":"fx","instrument":"EUR/USD","subject":")" +
                            std::string(2049, 'a') + R"("})") == http_status::bad_request);
}
