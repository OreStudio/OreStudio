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
#include "ores.http/routes/compute/compute_routes.hpp"
#include "ores.logging/logger.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::compute::tests::answer_at;
using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::compute::compute_routes;
using namespace ores::logging;

namespace {

const std::string page_pattern = "/api/operations/grid/app-versions/{id}";
const std::string note_pattern = "/api/operations/grid/app-versions/{id}/release-note";

const std::string a_build_id = "11111111-1111-1111-1111-111111111111";
const std::string a_build = "/api/operations/grid/app-versions/" + a_build_id;
const std::string a_note = a_build + "/release-note";

const std::string system_tenant = "ffffffff-ffff-ffff-ffff-ffffffffffff";
const std::string a_tenant = "22222222-2222-2222-2222-222222222222";

const std::string note_refused =
    "The release note takes up to 20000 characters and the version it read.";

/// A request whose caller's token is scoped to a tenant.
http_request as(const std::string& tenant) {
    http_request req;
    ores::security::jwt::jwt_claims claims;
    claims.subject = "someone";
    claims.tenant_id = tenant;
    req.authenticated_user = claims;
    return req;
}

bool page_holds(const std::string& json, const std::string& part) {
    return json.find(part) != std::string::npos;
}

/// A note write the route reads before it reaches the service.
std::string note_body(const std::string& note, const std::string& version) {
    return R"({"release_note":")" + note + R"(","version":)" + version + "}";
}

}

TEST_CASE("an_app_version_page_is_read_by_its_identifier", "[grid_app_versions]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier an app version page names";

    CHECK(answer_at<compute_routes>(http_method::get,
                                    "/api/operations/grid/app-versions/not-an-id",
                                    http_request{},
                                    page_pattern)
              .status == http_status::bad_request);
    CHECK(
        answer_at<compute_routes>(http_method::get, a_build, http_request{}, page_pattern).status ==
        http_status::bad_gateway);
}

TEST_CASE("an_app_version_page_is_written_by_the_system_tenant_alone", "[grid_app_versions]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the caller a note may be written by";

    http_request write = as(a_tenant);
    write.body = note_body("a note", "0");
    const auto refused = answer_at<compute_routes>(http_method::put, a_note, write, note_pattern);
    CHECK(refused.status == http_status::forbidden);
    CHECK(page_holds(refused.body, "Only a super administrator edits a release note."));

    http_request anonymous;
    anonymous.body = note_body("a note", "0");
    CHECK(answer_at<compute_routes>(http_method::put, a_note, anonymous, note_pattern).status ==
          http_status::forbidden);

    http_request operator_write = as(system_tenant);
    operator_write.body = note_body("a note", "0");
    CHECK(
        answer_at<compute_routes>(http_method::put, a_note, operator_write, note_pattern).status ==
        http_status::bad_gateway);
}

TEST_CASE("a_note_write_refuses_a_body_that_is_not_the_shape_it_takes", "[grid_app_versions]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the body a note write takes";

    for (const auto& body : {std::string(R"({"release_note":"a note"})"),
                             note_body("a note", "-1"),
                             note_body(std::string(20001, 'a'), "0"),
                             note_body("a note", "0") + R"(,"extra":true)"}) {
        http_request write = as(system_tenant);
        write.body = body;
        const auto refused =
            answer_at<compute_routes>(http_method::put, a_note, write, note_pattern);
        CHECK(refused.status == http_status::bad_request);
        CHECK(page_holds(refused.body, note_refused));
    }
}

TEST_CASE("a_note_write_takes_the_version_it_read_and_its_commentary", "[grid_app_versions]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the body a note write takes";

    http_request write = as(system_tenant);
    write.body = R"({"release_note":"a note","version":3,"commentary":"typo"})";
    CHECK(answer_at<compute_routes>(http_method::put, a_note, write, note_pattern).status ==
          http_status::bad_gateway);
}

TEST_CASE("an_app_version_route_refuses_a_parameter_it_does_not_take", "[grid_app_versions]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a parameter an app version route does not name";

    http_request req;
    req.query_params["version"] = "3";
    const auto page = answer_at<compute_routes>(http_method::get, a_build, req, page_pattern);
    CHECK(page.status == http_status::bad_request);
    CHECK(page_holds(page.body, "version"));
}
