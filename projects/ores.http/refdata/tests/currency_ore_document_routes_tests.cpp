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
#include "ores.http/routes/refdata/currency_ore_document_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::refdata::tests::answer_at;
using ores::http::routes::refdata::currency_ore_document_routes;
using namespace ores::logging;

namespace {

const std::string import_path = "/api/refdata/currencies/ore-import";
const std::string preview_path = "/api/refdata/currencies/ore-import/preview";

http_response posted(const std::string& path, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<currency_ore_document_routes>(http_method::post, path, req, path);
}

std::string import_of(const std::string& choices, const std::string& reason) {
    return R"({"content":"<CurrencyConfig/>","choices":)" + choices + R"(,"reasonCode":")" +
           reason + R"(","commentary":""})";
}

}

TEST_CASE("currency_ore_import_refuses_a_malformed_request", "[currency_ore_document]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the ORE currency preview and import refusals";

    CHECK(posted(preview_path, R"({"content":""})").status == http_status::bad_request);
    CHECK(posted(preview_path, "{}").status == http_status::bad_request);

    const std::string take = R"([{"iso_code":"USD","action":"take"}])";
    CHECK(posted(import_path, import_of(take, "  ")).status == http_status::bad_request);
    CHECK(posted(import_path, import_of(R"([{"iso_code":"USD","action":"merge"}])", "x")).status ==
          http_status::bad_request);
    CHECK(posted(import_path, import_of(R"([{"iso_code":"  ","action":"take"}])", "x")).status ==
          http_status::bad_request);
    CHECK(posted(import_path, import_of(R"([{"iso_code":"ABCDEFGHIJK","action":"take"}])", "x"))
              .status == http_status::bad_request);
    CHECK(posted(import_path,
                 import_of(R"([{"iso_code":"USD","action":"take"},)"
                           R"({"iso_code":" USD","action":"delete"}])",
                           "x"))
              .status == http_status::bad_request);
    CHECK(posted(import_path, R"({"content":"","choices":[],"reasonCode":"x","commentary":""})")
              .status == http_status::bad_request);
}

TEST_CASE("currency_ore_document_forwards_a_well_formed_request", "[currency_ore_document]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the ORE currency routes reach the service";

    // The client is not connected, so an accepted request fails at the forward step.
    http_request none;
    CHECK(answer_at<currency_ore_document_routes>(http_method::get,
                                                  "/api/refdata/currencies/ore-document",
                                                  none,
                                                  "/api/refdata/currencies/ore-document")
              .status == http_status::bad_gateway);
    CHECK(posted(preview_path, R"({"content":"<CurrencyConfig/>"})").status ==
          http_status::bad_gateway);
    CHECK(posted(import_path,
                 import_of(R"([{"iso_code":" USD ","action":"take"},{"iso_code":"EUR",)"
                           R"("action":"skip"}])",
                           "system.new_record"))
              .status == http_status::bad_gateway);
}
