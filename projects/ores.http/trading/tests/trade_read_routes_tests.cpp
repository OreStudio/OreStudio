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
#include "ores.http/routes/trading/trade_read_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "route_answer.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::http::routes::trading::trade_read_routes;
using ores::http::trading::tests::answer_at;
using namespace ores::logging;
namespace trading_domain = ores::trading::domain;

namespace {

boost::uuids::uuid trade_read_id(const char* text) {
    return boost::uuids::string_generator()(text);
}

const auto trade_one = trade_read_id("00000000-0000-0000-0000-0000000000a1");
const auto trade_two = trade_read_id("00000000-0000-0000-0000-0000000000a2");
const auto structure_one = trade_read_id("00000000-0000-0000-0000-0000000000b1");
const auto book_one = trade_read_id("00000000-0000-0000-0000-0000000000c1");
const auto party_one = trade_read_id("00000000-0000-0000-0000-0000000000d1");

trading_domain::trade_identifier
identifier_of(const boost::uuids::uuid& trade, const char* type, const char* value) {
    trading_domain::trade_identifier i;
    i.trade_id = trade;
    i.id_type = type;
    i.id_value = value;
    return i;
}

http_status trades_status(const std::map<std::string, std::string>& query) {
    http_request req;
    for (const auto& [name, value] : query)
        req.query_params[name] = value;
    return answer_at<trade_read_routes>(http_method::get, "/api/trades", req, "/api/trades").status;
}

bool holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

}

TEST_CASE("trade_reads_quote_the_identifier_a_desk_prefers", "[trade_read]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the identifier order";

    const auto best =
        trade_read_routes::best_identifiers({identifier_of(trade_one, "ORE", "ore-1"),
                                             identifier_of(trade_one, "UTI", "uti-1"),
                                             identifier_of(trade_one, "Internal", "int-1"),
                                             identifier_of(trade_two, "Vendor", "v-2"),
                                             identifier_of(trade_two, "ORE", "ore-2"),
                                             identifier_of(trade_two, "USI", "")});
    CHECK(best.at("00000000-0000-0000-0000-0000000000a1") == "int-1");
    CHECK(best.at("00000000-0000-0000-0000-0000000000a2") == "ore-2");
}

TEST_CASE("trade_reads_enrich_each_row_with_what_a_person_reads", "[trade_read]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the enriched rows";

    trading_domain::trade_summary trade_row;
    trade_row.id = trade_one;
    trade_row.row_kind = "trade";
    trade_row.book_id = book_one;
    trade_row.counterparty_id = party_one;
    trading_domain::trade_summary structure_row;
    structure_row.id = structure_one;
    structure_row.row_kind = "structure";
    trading_domain::structure_identifier reference;
    reference.structure_id = structure_one;
    reference.id_value = "STR-1";
    ores::refdata::domain::book book;
    book.id = book_one;
    book.name = "Rates \"London\"";
    ores::refdata::domain::counterparty counterparty;
    counterparty.id = party_one;
    counterparty.full_name = "Acme Bank";
    trading_domain::trade trade;
    trade.id = trade_one;
    trade.version = 3;
    trade.external_version = 2;

    const auto rows =
        trade_read_routes::enriched_rows({trade_row, structure_row},
                                         {identifier_of(trade_one, "Internal", "INT-1")},
                                         {reference},
                                         {book},
                                         {counterparty},
                                         {trade});
    REQUIRE(rows.size() == 2);
    CHECK(holds(rows[0], R"("identifier":"INT-1")"));
    CHECK(holds(rows[0], R"("counterparty_name":"Acme Bank")"));
    CHECK(holds(rows[0], R"("book_name":"Rates \"London\"")"));
    CHECK(holds(rows[0], R"("version":3)"));
    CHECK(holds(rows[0], R"("external_version":2)"));
    CHECK(holds(rows[0], R"("row_kind":"trade")"));
    CHECK(holds(rows[1], R"("identifier":"STR-1")"));
    CHECK(holds(rows[1], R"("version":null)"));
    CHECK(holds(rows[1], R"("book_name":"")"));
}

TEST_CASE("trade_reads_leave_a_row_naming_nothing_as_it_is", "[trade_read]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a row with nothing to enrich";

    trading_domain::trade_summary other;
    other.id = trade_one;
    other.row_kind = "other";
    const auto rows = trade_read_routes::enriched_rows({other}, {}, {}, {}, {}, {});
    REQUIRE(rows.size() == 1);
    CHECK_FALSE(holds(rows[0], R"("identifier")"));
}

TEST_CASE("trade_reads_name_a_trades_structures_and_links", "[trade_read]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the structures and links of a trade's page";

    trading_domain::structure_member member;
    member.trade_id = trade_one;
    member.structure_id = structure_one;
    member.role = "leg";
    member.sequence_number = 2;
    member.version = 1;
    trading_domain::structure_identifier reference;
    reference.structure_id = structure_one;
    reference.id_value = "STR-1";
    CHECK(trade_read_routes::structures_json({member}, {reference}) ==
          R"([{"structure_id":"00000000-0000-0000-0000-0000000000b1","reference":"STR-1",)"
          R"("role":"leg","sequence_number":2,"version":1}])");

    trading_domain::trade_link link;
    link.from_trade_id = trade_one;
    link.to_trade_id = trade_two;
    const std::map<std::string, std::string> labels{
        {"00000000-0000-0000-0000-0000000000a2", "INT-2"}};
    CHECK(holds(trade_read_routes::links_json({link}, true, labels), R"("other_label":"INT-2")"));
    CHECK(holds(trade_read_routes::links_json({link}, false, labels), R"("other_label":"")"));
}

TEST_CASE("trade_reads_refuse_a_malformed_request", "[trade_read]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the trade reads' refusals";

    CHECK(trades_status({{"limit", "0"}}) == http_status::bad_request);
    CHECK(trades_status({{"limit", "1001"}}) == http_status::bad_request);
    CHECK(trades_status({{"sort", "notional"}}) == http_status::bad_request);
    CHECK(trades_status({{"descending", "yes"}}) == http_status::bad_request);
    CHECK(trades_status({{"party", "acme"}}) == http_status::bad_request);
    CHECK(trades_status({{"search", std::string(257, 's')}}) == http_status::bad_request);
    CHECK(trades_status({}) == http_status::bad_gateway);
    CHECK(trades_status({{"book", "00000000-0000-0000-0000-0000000000c1"},
                         {"from", "2026-01-01"}}) == http_status::bad_gateway);

    const std::string page = "/api/trades/{id}";
    CHECK(answer_at<trade_read_routes>(http_method::get, "/api/trades/acme", http_request{}, page)
              .status == http_status::bad_request);
    CHECK(answer_at<trade_read_routes>(http_method::get,
                                       "/api/trades/00000000-0000-0000-0000-0000000000a1",
                                       http_request{},
                                       page)
              .status == http_status::bad_gateway);

    const std::string version = "/api/trades/{id}/versions/{version}";
    CHECK(
        answer_at<trade_read_routes>(http_method::get,
                                     "/api/trades/00000000-0000-0000-0000-0000000000a1/versions/0",
                                     http_request{},
                                     version)
            .status == http_status::bad_request);
    CHECK(
        answer_at<trade_read_routes>(http_method::get,
                                     "/api/trades/00000000-0000-0000-0000-0000000000a1/versions/2",
                                     http_request{},
                                     version)
            .status == http_status::bad_gateway);
}
