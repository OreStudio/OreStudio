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
#include "ores.http/routes/refdata/convention_screen_routes.hpp"
#include "ores.logging/logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::refdata::tests::answer_at;
using ores::http::routes::refdata::convention_screen_routes;
using namespace ores::logging;

namespace {

const std::string document_pattern = "/api/conventions/{family}/documents/{id}";
const std::string terms_pattern = "/api/conventions/{family}/documents/{id}/terms";
const std::string deposit_document =
    "/api/conventions/deposit/documents/00000000-0000-0000-0000-000000000001";
const std::string deposit_terms = deposit_document + "/terms";

/// A terms write the route reads before it reaches the service.
std::string terms_body() {
    return R"({"intent":{"reason_code":"desk.change"},"version":0,)"
           R"("changes":{"index":"EUR-ESTR"}})";
}

}

TEST_CASE("a_convention_document_names_the_family_the_address_does_not_hold",
          "[convention_documents]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the family a convention document names";

    const auto answer = answer_at<convention_screen_routes>(
        http_method::get,
        "/api/conventions/average-ons/documents/00000000-0000-0000-0000-000000000001",
        http_request{},
        document_pattern);
    CHECK(answer.status == http_status::not_found);
    CHECK(answer.body.find("average-ons") != std::string::npos);
}

TEST_CASE("a_convention_document_refuses_an_identifier_and_a_party_that_are_not_ones",
          "[convention_documents]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the identifier and the party a document is asked for";

    CHECK(answer_at<convention_screen_routes>(http_method::get,
                                              "/api/conventions/deposit/documents/not-an-id",
                                              http_request{},
                                              document_pattern)
              .status == http_status::bad_request);

    http_request asked;
    asked.query_params["party"] = "acme";
    CHECK(answer_at<convention_screen_routes>(
              http_method::get, deposit_document, asked, document_pattern)
              .status == http_status::bad_request);

    http_request unknown;
    unknown.query_params["asOf"] = "2026-01-01";
    CHECK(answer_at<convention_screen_routes>(
              http_method::get, deposit_document, unknown, document_pattern)
              .status == http_status::bad_request);
}

TEST_CASE("a_convention_document_with_no_service_is_refused_as_upstream",
          "[convention_documents]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a convention document with no service behind it";

    CHECK(answer_at<convention_screen_routes>(
              http_method::get, deposit_document, http_request{}, document_pattern)
              .status == http_status::bad_gateway);
}

TEST_CASE("the_terms_of_a_composite_convention_are_changed_on_its_parts",
          "[convention_documents]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the terms write of a composite convention";

    http_request write;
    write.body = terms_body();
    const auto answer = answer_at<convention_screen_routes>(
        http_method::put,
        "/api/conventions/commodity-future/documents/00000000-0000-0000-0000-000000000001/terms",
        write,
        terms_pattern);
    CHECK(answer.status == http_status::bad_request);
    CHECK(answer.body.find("commodity-future") != std::string::npos);
}

TEST_CASE("a_terms_write_refuses_a_family_whose_terms_the_screen_does_not_draw",
          "[convention_documents]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the terms write of a family with no write";

    http_request write;
    write.body = terms_body();
    const auto answer = answer_at<convention_screen_routes>(
        http_method::put,
        "/api/conventions/average-ois/documents/00000000-0000-0000-0000-000000000001/terms",
        write,
        terms_pattern);
    CHECK(answer.status == http_status::bad_request);
    CHECK(answer.body.find("average-ois") != std::string::npos);
}

TEST_CASE("a_terms_write_refuses_a_body_it_cannot_read", "[convention_documents]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the body a terms write states";

    http_request unreadable;
    unreadable.body = "not a body";
    CHECK(answer_at<convention_screen_routes>(
              http_method::put, deposit_terms, unreadable, terms_pattern)
              .status == http_status::bad_request);

    http_request unknown_member;
    unknown_member.body = R"({"intent":{"reason_code":"desk.change"},"version":0,"write":{}})";
    CHECK(answer_at<convention_screen_routes>(
              http_method::put, deposit_terms, unknown_member, terms_pattern)
              .status == http_status::bad_request);

    http_request no_reason;
    no_reason.body = R"({"intent":{"reason_code":""},"version":0,)"
                     R"("changes":{"index":"EUR-ESTR"}})";
    CHECK(answer_at<convention_screen_routes>(
              http_method::put, deposit_terms, no_reason, terms_pattern)
              .status == http_status::bad_request);

    http_request no_changes;
    no_changes.body = R"({"intent":{"reason_code":"desk.change"},"version":0,"changes":{}})";
    CHECK(answer_at<convention_screen_routes>(
              http_method::put, deposit_terms, no_changes, terms_pattern)
              .status == http_status::bad_request);
}

TEST_CASE("a_terms_write_with_no_service_is_refused_as_upstream", "[convention_documents]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing a terms write with no service behind it";

    http_request write;
    write.body = terms_body();
    CHECK(answer_at<convention_screen_routes>(http_method::put, deposit_terms, write, terms_pattern)
              .status == http_status::bad_gateway);
}
