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
#include "ores.http/routes/trading/book_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::trading::book_routes;
using ores::http::trading::tests::answer_at;
using namespace ores::logging;
namespace refdata_messaging = ores::refdata::messaging;

namespace {

boost::uuids::uuid book_uuid(const char* text) {
    return boost::uuids::string_generator()(text);
}

const auto book_id = "00000000-0000-0000-0000-0000000000c1";
const auto book_party = "00000000-0000-0000-0000-0000000000d1";
const auto book_portfolio = "00000000-0000-0000-0000-0000000000c2";

refdata_messaging::book_line_outcome
book_line_of(int number, const char* operation, const char* refusal) {
    refdata_messaging::book_line_outcome line;
    line.line_no = number;
    line.operation = operation;
    line.entity_id = book_uuid(book_id);
    line.columns = {"name", "book_status"};
    line.refusal = refusal;
    line.part_codes = {"refdata.book"};
    return line;
}

bool book_holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

http_status book_write_status(const std::string& target, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<book_routes>(http_method::put, target, req, target).status;
}

http_status book_change_status(const std::string& target, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<book_routes>(http_method::post, target, req, target).status;
}

}

TEST_CASE("books_preview_shapes_each_line_and_its_parts", "[book]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a preview's answer";

    const auto preview = book_routes::preview_json(
        {book_line_of(1, "put", ""), book_line_of(2, "delete", "The book holds trades.")},
        {"refdata.book", "workflow.approval"});

    CHECK(book_holds(preview, R"("lineNo":1)"));
    CHECK(book_holds(preview, R"("operation":"put")"));
    CHECK(book_holds(preview, R"("refusal":"")"));
    CHECK(book_holds(preview, R"("columns":["name","book_status"])"));
    CHECK(book_holds(preview, R"("partCodes":["refdata.book"])"));
    CHECK(book_holds(preview, R"("refusal":"The book holds trades.")"));
    CHECK(book_holds(preview, R"("partCodes":["refdata.book","workflow.approval"])"));
}

TEST_CASE("books_raise_states_the_request_or_why_none_was_made", "[book]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a raise's answer";

    const auto raised = book_routes::raise_json(
        {book_line_of(1, "put", "")}, {"refdata.book"}, true, book_uuid(book_id), "", "");
    CHECK(book_holds(raised, R"("requestId":"00000000-0000-0000-0000-0000000000c1")"));
    CHECK(book_holds(raised, R"("refusalCode":"")"));
    CHECK(book_holds(raised, R"("refusalMessage":"")"));

    const auto refused = book_routes::raise_json({book_line_of(1, "put", "No book with this id.")},
                                                 {},
                                                 false,
                                                 std::nullopt,
                                                 "not_found",
                                                 "No book with this id.");
    CHECK(book_holds(refused, R"("requestId":null)"));
    CHECK(book_holds(refused, R"("refusalCode":"not_found")"));
    CHECK(book_holds(refused, R"("refusalMessage":"No book with this id.")"));
}

TEST_CASE("books_refuse_a_malformed_request", "[book]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the book routes' refusals";

    CHECK(answer_at<book_routes>(
              http_method::get, "/api/books/acme", http_request{}, "/api/books/{id}")
              .status == http_status::bad_request);
    CHECK(answer_at<book_routes>(http_method::get,
                                 "/api/books/00000000-0000-0000-0000-0000000000c1",
                                 http_request{},
                                 "/api/books/{id}")
              .status == http_status::bad_gateway);
    CHECK(
        answer_at<book_routes>(http_method::get,
                               "/api/books/portfolios/00000000-0000-0000-0000-0000000000c2/rights",
                               http_request{},
                               "/api/books/portfolios/{id}/rights")
            .status == http_status::bad_gateway);

    const std::string row = R"({"id":"00000000-0000-0000-0000-0000000000c1",)"
                            R"("party_id":"00000000-0000-0000-0000-0000000000d1",)"
                            R"("name":"Rates"})";
    const std::string write_of = R"({"intent":{"reason_code":"system.new_record"},)"
                                 R"("version":null,"write":)" +
                                 row + "}";
    const std::string rate_of = R"({"intent":{"reason_code":"system.new_record"},)"
                                R"("version":null,"write":)"
                                R"({"id":"00000000-0000-0000-0000-0000000000c1",)"
                                R"("party_id":"00000000-0000-0000-0000-0000000000d1",)"
                                R"("name":""}})";
    const std::string party_of = R"({"intent":{"reason_code":"system.new_record"},)"
                                 R"("version":null,"write":)"
                                 R"({"id":"00000000-0000-0000-0000-0000000000c1",)"
                                 R"("party_id":"acme","name":"Rates"}})";

    CHECK(book_write_status("/api/books/books", "") == http_status::bad_request);
    CHECK(book_write_status("/api/books/books", write_of) == http_status::bad_gateway);
    CHECK(book_write_status("/api/books/books", rate_of) == http_status::bad_request);
    CHECK(book_write_status("/api/books/books", party_of) == http_status::bad_request);
    CHECK(book_write_status("/api/books/portfolios", write_of) == http_status::bad_gateway);
    CHECK(book_write_status("/api/books/portfolios", rate_of) == http_status::bad_request);

    const std::string line = R"({"operation":"put",)"
                             R"("entity_id":"00000000-0000-0000-0000-0000000000c1",)"
                             R"("base_version":0,)"
                             R"("party_id":"00000000-0000-0000-0000-0000000000d1",)"
                             R"("name":"Rates",)"
                             R"("parent_portfolio_id":"00000000-0000-0000-0000-0000000000c2"})";
    const auto preview_of = [](const std::string& lines) {
        return R"({"lines":)" + lines + "}";
    };
    CHECK(book_change_status("/api/book-changes/preview", preview_of("[]")) ==
          http_status::bad_request);
    CHECK(book_change_status("/api/book-changes/preview", preview_of("[" + line + "]")) ==
          http_status::bad_gateway);
    CHECK(book_change_status("/api/book-changes/preview",
                             preview_of(R"([{"operation":"amend","entity_id":)"
                                        R"("00000000-0000-0000-0000-0000000000c1",)"
                                        R"("base_version":0,)"
                                        R"("party_id":"00000000-0000-0000-0000-0000000000d1",)"
                                        R"("name":"Rates"}])")) == http_status::bad_request);
    CHECK(book_change_status("/api/book-changes/preview",
                             preview_of(R"([{"operation":"put","entity_id":)"
                                        R"("00000000-0000-0000-0000-0000000000c1",)"
                                        R"("base_version":-1,)"
                                        R"("party_id":"00000000-0000-0000-0000-0000000000d1",)"
                                        R"("name":"Rates"}])")) == http_status::bad_request);

    CHECK(book_change_status("/api/book-changes", R"({"reason":"","lines":[)" + line + "]})") ==
          http_status::bad_request);
    CHECK(book_change_status("/api/book-changes",
                             R"({"reason":"The desks merge.","lines":[)" + line + "]}") ==
          http_status::bad_gateway);
}
