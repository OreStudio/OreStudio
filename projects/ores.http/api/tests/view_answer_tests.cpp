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
#include "ores.http.api/net/view_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::net::entity_tag;
using ores::http::net::view_answer;
using ores::http::net::view_cache_policy;

namespace {

const std::string view_body = R"({"folders":[]})";
const view_cache_policy fresh_for_a_minute{std::chrono::seconds(60)};

http_request asking_for(const std::string& if_none_match) {
    http_request req;
    if (!if_none_match.empty())
        req.headers["if-none-match"] = if_none_match;
    return req;
}

}

TEST_CASE("view_answer_tags_a_cached_view_and_states_its_freshness", "[view_answer]") {
    const auto answer = view_answer(asking_for(""), view_body, fresh_for_a_minute);
    CHECK(answer.status == http_status::ok);
    CHECK(answer.body == view_body);
    CHECK(answer.headers.at("ETag") == entity_tag(view_body));
    CHECK(answer.headers.at("Cache-Control") == "private, max-age=60");
}

TEST_CASE("view_answer_answers_not_modified_when_the_caller_holds_the_tag", "[view_answer]") {
    const auto tag = entity_tag(view_body);
    for (const auto& header :
         {tag, "W/" + tag, "\"other\", " + tag, std::string("*"), " " + tag + " "}) {
        const auto answer = view_answer(asking_for(header), view_body, fresh_for_a_minute);
        CHECK(answer.status == http_status::not_modified);
        CHECK(answer.body.empty());
        CHECK(answer.headers.at("ETag") == tag);
        CHECK(answer.headers.at("Cache-Control") == "private, max-age=60");
    }
}

TEST_CASE("view_answer_sends_the_body_when_the_caller_holds_another_tag", "[view_answer]") {
    const auto answer = view_answer(asking_for("\"stale\""), view_body, fresh_for_a_minute);
    CHECK(answer.status == http_status::ok);
    CHECK(answer.body == view_body);
}

TEST_CASE("view_answer_does_not_store_a_view_with_no_freshness_rule", "[view_answer]") {
    const auto answer = view_answer(asking_for("*"), view_body, {});
    CHECK(answer.status == http_status::ok);
    CHECK(answer.body == view_body);
    CHECK(answer.headers.at("Cache-Control") == "no-store");
    CHECK_FALSE(answer.headers.contains("ETag"));
}

TEST_CASE("entity_tag_changes_with_the_bytes", "[view_answer]") {
    CHECK(entity_tag(view_body) == entity_tag(view_body));
    CHECK(entity_tag(view_body) != entity_tag(R"({"folders":[{}]})"));
    CHECK(entity_tag("").front() == '"');
    CHECK(entity_tag("").back() == '"');
}
