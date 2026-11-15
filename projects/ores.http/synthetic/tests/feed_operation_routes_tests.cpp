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
#include "ores.http/routes/synthetic/feed_operation_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <utility>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::synthetic::feed_operation_routes;
using ores::http::synthetic::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string feed_id = "00000000-0000-0000-0000-00000000000a";

http_response feed_answer(http_method method,
                          const std::string& path,
                          http_request req,
                          const std::string& pattern) {
    return answer_at<feed_operation_routes>(method, path, std::move(req), pattern);
}

http_response page_of(std::vector<std::pair<std::string, std::string>> query) {
    http_request req;
    for (auto& [name, value] : query)
        req.query_params[name] = value;
    return feed_answer(
        http_method::get, "/api/synthetic/folder-feeds", req, "/api/synthetic/folder-feeds");
}

http_response selected(const std::string& verb, const std::string& body) {
    http_request req;
    req.body = body;
    const auto path = "/api/synthetic/feed-selection/" + verb;
    return feed_answer(http_method::post, path, req, path);
}

}

TEST_CASE("feed_operations_refuse_a_malformed_request", "[feed_operation]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing each feed operation's refusals";

    for (const std::string verb : {"start", "stop"}) {
        CHECK(feed_answer(http_method::post,
                          "/api/synthetic/feeds/nope/" + verb,
                          http_request{},
                          "/api/synthetic/feeds/{id}/" + verb)
                  .status == http_status::bad_request);
        CHECK(feed_answer(http_method::post,
                          "/api/synthetic/folders/nope/" + verb,
                          http_request{},
                          "/api/synthetic/folders/{id}/" + verb)
                  .status == http_status::bad_request);
    }

    CHECK(page_of({{"limit", "0"}}).status == http_status::bad_request);
    CHECK(page_of({{"limit", "201"}}).status == http_status::bad_request);
    CHECK(page_of({{"offset", "1000001"}}).status == http_status::bad_request);
    CHECK(page_of({{"state", "paused"}}).status == http_status::bad_request);
    CHECK(page_of({{"shouldRun", "maybe"}}).status == http_status::bad_request);
    CHECK(page_of({{"provider", std::string(65, 'p')}}).status == http_status::bad_request);
    CHECK(page_of({{"colour", "red"}}).status == http_status::bad_request);

    CHECK(selected("start", R"({"state":"paused"})").status == http_status::bad_request);
    CHECK(selected("stop", R"({"feedIds":[")" + std::string(41, 'x') + R"("]})").status ==
          http_status::bad_request);
    CHECK(selected("start", R"({"shouldRun":"yes"})").status == http_status::bad_request);
}

TEST_CASE("feed_operations_forward_a_well_formed_request", "[feed_operation]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an accepted feed operation reaches the service";

    // The client is not connected, so an accepted request fails at the forward step.
    for (const std::string verb : {"start", "stop"}) {
        CHECK(feed_answer(http_method::post,
                          "/api/synthetic/feeds/" + feed_id + "/" + verb,
                          http_request{},
                          "/api/synthetic/feeds/{id}/" + verb)
                  .status == http_status::bad_gateway);
        CHECK(feed_answer(http_method::post,
                          "/api/synthetic/folders/" + feed_id + "/" + verb,
                          http_request{},
                          "/api/synthetic/folders/{id}/" + verb)
                  .status == http_status::bad_gateway);
    }

    CHECK(page_of({}).status == http_status::bad_gateway);
    CHECK(page_of({{"folder", " root "},
                   {"state", "started"},
                   {"shouldRun", "TRUE"},
                   {"offset", "60"},
                   {"limit", "200"}})
              .status == http_status::bad_gateway);

    CHECK(selected("start", "{}").status == http_status::bad_gateway);
    CHECK(selected("stop", R"({"feedIds":["a","b"],"excludedIds":[],"shouldRun":true})").status ==
          http_status::bad_gateway);
}
