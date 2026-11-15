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
#include "ores.http/routes/trading/booking_routes.hpp"
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
using ores::http::routes::trading::booking_routes;
using ores::http::trading::tests::answer_at;
using namespace ores::logging;
namespace refdata_domain = ores::refdata::domain;
namespace trading_domain = ores::trading::domain;

namespace {

boost::uuids::uuid booking_id(const char* text) {
    return boost::uuids::string_generator()(text);
}

const auto booking_party_one = "00000000-0000-0000-0000-0000000000d1";
const auto booking_party_two = "00000000-0000-0000-0000-0000000000d2";

refdata_domain::book
booking_book_of(const char* id, const char* party, const char* name, const char* status) {
    refdata_domain::book b;
    b.id = booking_id(id);
    b.party_id = booking_id(party);
    b.name = name;
    b.book_status = status;
    return b;
}

trading_domain::trade_id_type booking_scheme_of(const char* code) {
    trading_domain::trade_id_type t;
    t.code = code;
    return t;
}

trading_domain::trade_identifier booking_identifier_of(const char* type) {
    trading_domain::trade_identifier i;
    i.id_type = type;
    return i;
}

trading_domain::trade_state booking_state_of(int version, const char* activity) {
    trading_domain::trade_state s;
    s.version = version;
    s.trade_activity_id = booking_id(activity);
    return s;
}

std::string booking_joined(const std::vector<std::string>& parts) {
    std::string out;
    for (const auto& part : parts)
        out += (out.empty() ? "" : ",") + part;
    return out;
}

bool booking_holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

/// The claims a signed-in booking states, so the route knows the acting party.
const char* booking_request_body = R"({"bookId":"00000000-0000-0000-0000-0000000000c1",)"
                                   R"("counterpartyId":"00000000-0000-0000-0000-0000000000c2",)"
                                   R"("tradeDate":"2026-11-12",)"
                                   R"("issueId":"00000000-0000-0000-0000-0000000000c3",)"
                                   R"("notional":"1000000.50",)"
                                   R"("identifierType":"Internal",)"
                                   R"("identifierValue":"RF-1"})";

http_request booking_signed_in(const char* body) {
    ores::security::jwt::jwt_claims claims;
    claims.party_id = booking_party_one;
    http_request req;
    req.authenticated_user = claims;
    req.body = body;
    return req;
}

http_status booking_trade_status(const http_request& req) {
    return answer_at<booking_routes>(http_method::post, "/api/trades", req, "/api/trades").status;
}

http_status booking_link_status(const std::string& trade, const std::string& body) {
    http_request req;
    req.body = body;
    return answer_at<booking_routes>(
               http_method::post, "/api/trades/" + trade + "/links", req, "/api/trades/{id}/links")
        .status;
}

}

TEST_CASE("booking_offers_the_acting_partys_open_books", "[booking]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the books the booking form offers";

    const std::vector books{
        booking_book_of(
            "00000000-0000-0000-0000-0000000000b1", booking_party_one, "Rates", "Active"),
        booking_book_of(
            "00000000-0000-0000-0000-0000000000b2", booking_party_two, "Credit", "Active"),
        booking_book_of(
            "00000000-0000-0000-0000-0000000000b3", booking_party_one, "Frozen", "Frozen")};
    const auto offered = booking_routes::party_books_json(books, booking_party_one);
    REQUIRE(offered.size() == 1);
    CHECK(booking_holds(offered.front(), R"("name":"Rates")"));
    CHECK(booking_holds(offered.front(), R"("id":"00000000-0000-0000-0000-0000000000b1")"));
    CHECK(booking_routes::party_books_json(books, "").empty());
}

TEST_CASE("booking_prefers_the_tenants_own_identifier_schemes", "[booking]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the identifier schemes the form offers";

    const std::vector<trading_domain::trade_id_type> declared{booking_scheme_of("UTI"),
                                                              booking_scheme_of("Internal"),
                                                              booking_scheme_of("Internal"),
                                                              booking_scheme_of("")};
    const std::vector<trading_domain::trade_identifier> used{booking_identifier_of("ORE")};
    const auto offered = booking_joined(booking_routes::identifier_schemes_json(declared, used));
    CHECK(booking_holds(offered, R"({"code":"Internal"},{"code":"UTI"})"));

    const auto fallback =
        booking_joined(booking_routes::identifier_schemes_json({},
                                                               {
                                                                   booking_identifier_of("ORE"),
                                                                   booking_identifier_of("UTI"),
                                                                   booking_identifier_of("ORE"),
                                                               }));
    CHECK(booking_holds(fallback, R"({"code":"ORE"},{"code":"UTI"})"));
    CHECK(booking_routes::identifier_schemes_json({}, {}).empty());
}

TEST_CASE("booking_records_a_link_against_the_newest_activity", "[booking]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the activity a link is recorded against";

    const std::vector states{booking_state_of(1, "00000000-0000-0000-0000-0000000000e1"),
                             booking_state_of(3, "00000000-0000-0000-0000-0000000000e3"),
                             booking_state_of(2, "00000000-0000-0000-0000-0000000000e2")};
    const auto newest = booking_routes::newest_activity(states);
    REQUIRE(newest.has_value());
    CHECK(boost::uuids::to_string(*newest) == "00000000-0000-0000-0000-0000000000e3");
    CHECK_FALSE(booking_routes::newest_activity({}).has_value());
}

TEST_CASE("booking_refuses_a_malformed_request", "[booking]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the booking routes' refusals";

    CHECK(answer_at<booking_routes>(
              http_method::get, "/api/trades/new", http_request{}, "/api/trades/new")
              .status == http_status::bad_gateway);
    CHECK(answer_at<booking_routes>(
              http_method::get, "/api/draft-reasons", http_request{}, "/api/draft-reasons")
              .status == http_status::bad_gateway);
    CHECK(answer_at<booking_routes>(
              http_method::get, "/api/trade-link-types", http_request{}, "/api/trade-link-types")
              .status == http_status::bad_gateway);

    const auto trade = std::string("00000000-0000-0000-0000-0000000000a1");
    CHECK(booking_link_status(
              "acme",
              R"({"linkType":"Hedge","otherTradeId":"00000000-0000-0000-0000-0000000000a2"})") ==
          http_status::bad_request);
    CHECK(booking_link_status(
              trade, R"({"linkType":"","otherTradeId":"00000000-0000-0000-0000-0000000000a2"})") ==
          http_status::bad_request);
    CHECK(booking_link_status(trade, R"({"linkType":"Hedge","otherTradeId":"acme"})") ==
          http_status::bad_request);
    CHECK(booking_link_status(trade, R"({"linkType":"Hedge"})") == http_status::bad_request);
    CHECK(booking_link_status(
              trade,
              R"({"linkType":"Hedge","otherTradeId":"00000000-0000-0000-0000-0000000000a1"})") ==
          http_status::bad_request);

    CHECK(booking_trade_status(http_request{}) == http_status::bad_request);
    CHECK(booking_trade_status(booking_signed_in(R"({"bookId":"acme"})")) ==
          http_status::bad_request);
    CHECK(booking_trade_status(booking_signed_in(
              R"({"bookId":"00000000-0000-0000-0000-0000000000c1"})")) == http_status::bad_request);
    CHECK(booking_trade_status(
              booking_signed_in(R"({"bookId":"00000000-0000-0000-0000-0000000000c1",)"
                                R"("counterpartyId":"00000000-0000-0000-0000-0000000000c2",)"
                                R"("tradeDate":"12/11/2026",)"
                                R"("issueId":"00000000-0000-0000-0000-0000000000c3",)"
                                R"("notional":"1000000.50",)"
                                R"("identifierType":"Internal","identifierValue":"RF-1"})")) ==
          http_status::bad_request);
    CHECK(booking_trade_status(
              booking_signed_in(R"({"bookId":"00000000-0000-0000-0000-0000000000c1",)"
                                R"("counterpartyId":"00000000-0000-0000-0000-0000000000c2",)"
                                R"("tradeDate":"2026-02-30",)"
                                R"("issueId":"00000000-0000-0000-0000-0000000000c3",)"
                                R"("notional":"1000000.50",)"
                                R"("identifierType":"Internal","identifierValue":"RF-1"})")) ==
          http_status::bad_request);
    CHECK(booking_trade_status(
              booking_signed_in(R"({"bookId":"00000000-0000-0000-0000-0000000000c1",)"
                                R"("counterpartyId":"00000000-0000-0000-0000-0000000000c2",)"
                                R"("tradeDate":"2026-11-12",)"
                                R"("issueId":"00000000-0000-0000-0000-0000000000c3",)"
                                R"("notional":"1,000,000",)"
                                R"("identifierType":"Internal","identifierValue":"RF-1"})")) ==
          http_status::bad_request);
    CHECK(booking_trade_status(
              booking_signed_in(R"({"bookId":"00000000-0000-0000-0000-0000000000c1",)"
                                R"("counterpartyId":"00000000-0000-0000-0000-0000000000c2",)"
                                R"("tradeDate":"2026-11-12",)"
                                R"("issueId":"00000000-0000-0000-0000-0000000000c3",)"
                                R"("notional":"1000000.50",)"
                                R"("identifierType":"Internal","identifierValue":"RF-1",)"
                                R"("draftReasonCode":"  "})")) == http_status::bad_request);
    CHECK(booking_trade_status(booking_signed_in(booking_request_body)) ==
          http_status::bad_gateway);
}
