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
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http/routes/workflow/story_events.hpp"
#include "ores.http/routes/workflow/timeline.hpp"
#include "ores.http/routes/workflow/timeline_sources.hpp"
#include "ores.http/routes/workflow/workflow_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::net::record_routes::refusal;
using ores::http::routes::workflow::or_gap;
using ores::http::routes::workflow::order_story;
using ores::http::routes::workflow::story_event;
using ores::http::routes::workflow::story_link_events;
using ores::http::routes::workflow::timeline_answer;
using ores::http::routes::workflow::timeline_gap;
using ores::http::routes::workflow::timeline_json;
using ores::http::routes::workflow::timeline_routes;
using ores::http::routes::workflow::workflow_routes;
using ores::http::workflow::tests::answer_at;
using namespace ores::logging;

namespace {

const std::string timeline_pattern = "/api/timeline";

const std::string timeline_refused = "Name a timeline by its subject and its id.";

const std::string a_timeline_party_id = "77777777-7777-7777-7777-777777777777";

/// The query one timeline is read with: the router matches the address and the
/// session parses the parameters, so a test states them as the session would.
http_request timeline_query(const std::string& subject, const std::string& id) {
    http_request req;
    req.query_params["subject"] = subject;
    req.query_params["id"] = id;
    return req;
}

/// One instant, so a tie is a tie and not a clock's drift.
std::chrono::system_clock::time_point timeline_instant(int seconds) {
    return std::chrono::system_clock::time_point{} + std::chrono::seconds{seconds};
}

/// One version of one record, as the history layer answers it.
ores::http::net::history::version timeline_version(std::uint32_t number,
                                                   const std::vector<std::string>& fields) {
    ores::http::net::history::version version;
    version.version = number;
    version.modified_by = "alice";
    version.recorded_at = timeline_instant(1000);
    for (std::size_t i = 0; i < fields.size(); i += 2)
        version.fields.push_back({fields[i], fields[i + 1]});
    return version;
}

bool timeline_holds(const std::string& body, const std::string& part) {
    return body.find(part) != std::string::npos;
}

}

TEST_CASE("a_timeline_is_named_by_its_subject_and_id", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the subject and id a timeline names";

    const auto refused = answer_at<workflow_routes>(
        http_method::get, "/api/timeline", http_request{}, timeline_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(timeline_holds(refused.body, timeline_refused));
}

TEST_CASE("a_timeline_names_only_its_own_subjects", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a subject a timeline cannot read for";

    const auto refused = answer_at<workflow_routes>(
        http_method::get, "/api/timeline", timeline_query("desk", "ana"), timeline_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(timeline_holds(refused.body, "not for desk"));
}

TEST_CASE("a_timeline_refuses_a_parameter_it_does_not_take", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a parameter a timeline does not name";

    auto req = timeline_query("person", "ana");
    req.query_params["since"] = "2026-01-01";
    const auto refused =
        answer_at<workflow_routes>(http_method::get, "/api/timeline", req, timeline_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(timeline_holds(refused.body, "since"));
}

TEST_CASE("a_request_timeline_is_named_by_its_identifier", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the identifier a request's timeline names";

    const auto refused = answer_at<workflow_routes>(http_method::get,
                                                    "/api/timeline",
                                                    timeline_query("request", "not-a-request"),
                                                    timeline_pattern);
    CHECK(refused.status == http_status::bad_request);
    CHECK(timeline_holds(refused.body, timeline_refused));
}

TEST_CASE("a_timeline_answers_the_service_is_absent", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a timeline with no service behind it";

    CHECK(answer_at<workflow_routes>(
              http_method::get, "/api/timeline", timeline_query("person", "ana"), timeline_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("a_timeline_is_kept_privately_for_thirty_seconds", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the cache policy a timeline carries";

    timeline_answer answer;
    answer.subject = "party";
    answer.id = a_timeline_party_id;
    story_event raised;
    raised.entity_type = "ores.refdata.party";
    raised.entity_id = a_timeline_party_id;
    raised.kind = "raised";
    raised.at = timeline_instant(1000);
    raised.actor = "alice";
    raised.version = 1;
    answer.events.push_back(raised);
    answer.gaps.push_back({"ores.refdata.party_identifier_removed",
                           "No read lists the identifiers that were removed from this party."});
    const auto body = timeline_json(answer);

    http_request req;
    const auto first = timeline_routes::view_answer(req, body);
    CHECK(first.status == http_status::ok);
    REQUIRE(first.headers.contains("ETag"));
    CHECK(first.headers.at("Cache-Control") == "private, max-age=30");
    CHECK(first.headers.at("Vary") == "Authorization, Cookie");
    CHECK(timeline_holds(first.body, "ores.refdata.party_identifier_removed"));

    http_request revalidation;
    revalidation.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(timeline_routes::view_answer(revalidation, body).status == http_status::not_modified);

    http_request changed;
    changed.headers["if-none-match"] = first.headers.at("ETag");
    CHECK(timeline_routes::view_answer(changed, body.substr(0, 40)).status == http_status::ok);
}

TEST_CASE("a_timeline_reads_its_events_newest_first_within_one_second", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the one order every subject's stream keeps";

    const auto event = [](std::string kind, int seconds) {
        story_event made;
        made.kind = std::move(kind);
        made.at = timeline_instant(seconds);
        made.entity_id = "one";
        made.version = 1;
        return made;
    };
    std::vector<story_event> events{event("raised", 1000),
                                    event("closed", 1000),
                                    event("signed_in", 1000),
                                    event("decided", 999)};
    order_story(events);

    // The later act of one second comes first, and the earlier second last: a
    // link is closed after it is opened, and a sign-in outranks the record it
    // then writes.
    REQUIRE(events.size() == 4);
    CHECK(events[0].kind == "closed");
    CHECK(events[1].kind == "signed_in");
    CHECK(events[2].kind == "raised");
    CHECK(events[3].kind == "decided");
}

TEST_CASE("a_link_states_its_opening_its_change_and_its_closing", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the entries one link table's history makes";

    const auto versions = std::vector<ores::http::net::history::version>{
        timeline_version(1, {"ID", "acc/role", "Role", "admin"}),
        timeline_version(2,
                         {"ID",
                          "acc/role",
                          "Role",
                          "admin",
                          "Closed At",
                          "1970-01-01 00:16:40.000000Z",
                          "Closed By",
                          "bob"})};
    const auto events = story_link_events("ores.iam.account_role", versions);

    REQUIRE(events.size() == 3);
    // The opening keeps the link's id, and not the fields of its closing.
    CHECK(events[0].kind == "raised");
    CHECK(events[0].entity_id == "acc/role");
    CHECK(events[1].kind == "changed");
    CHECK(events[2].kind == "closed");
    CHECK(events[2].at == timeline_instant(1000));
    CHECK(events[2].actor == "bob");
    for (const auto& field : events[0].fields)
        CHECK(field.first != "Closed At");
}

TEST_CASE("a_source_the_caller_may_not_read_is_a_gap", "[workflow_timeline]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the rule that separates a refusal from a fault";

    std::vector<timeline_gap> gaps;
    const auto missing = or_gap(gaps, "ores.iam.role", "The roles are not shown.", []() -> int {
        ores::http::net::record_routes::refuse(http_status::forbidden, "forbidden", "No.");
    });
    CHECK(missing == 0);
    REQUIRE(gaps.size() == 1);
    CHECK(gaps[0].entity == "ores.iam.role");
    CHECK(gaps[0].reason == "The roles are not shown.");

    // A service that is down is not a permission the person lacks, so it is thrown
    // and the route answers that the service did not answer.
    std::vector<timeline_gap> no_gaps;
    CHECK_THROWS_AS(or_gap(no_gaps,
                           "ores.iam.role",
                           "The roles are not shown.",
                           []() -> int {
                               ores::http::net::record_routes::refuse(
                                   http_status::bad_gateway, "upstream-unavailable", "Down.");
                           }),
                    refusal);
    CHECK(no_gaps.empty());
}
