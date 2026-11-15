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
#include "ores.http/routes/trading/structure_routes.hpp"
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
using ores::http::routes::trading::structure_routes;
using ores::http::trading::tests::answer_at;
using namespace ores::logging;
namespace trading_domain = ores::trading::domain;

namespace {

boost::uuids::uuid structure_uuid(const char* text) {
    return boost::uuids::string_generator()(text);
}

const auto structure_root = "00000000-0000-0000-0000-0000000000f1";
const auto structure_child = "00000000-0000-0000-0000-0000000000f2";
const auto structure_trade = "00000000-0000-0000-0000-0000000000f3";

trading_domain::structure structure_of(const char* id, const char* parent, const char* kind) {
    trading_domain::structure s;
    s.id = structure_uuid(id);
    s.parent_structure_id = structure_uuid(parent);
    s.kind = kind;
    s.template_code = "";
    return s;
}

trading_domain::structure_member structure_leg_of(const char* trade, const char* holder) {
    trading_domain::structure_member m;
    m.trade_id = structure_uuid(trade);
    m.structure_id = structure_uuid(holder);
    m.role = "leg";
    m.sequence_number = 1;
    return m;
}

trading_domain::trade structure_trade_of(const char* id, const char* type) {
    trading_domain::trade t;
    t.id = structure_uuid(id);
    t.trade_type = type;
    t.external_version = 5;
    return t;
}

bool structure_holds(const std::string& text, const std::string& part) {
    return text.find(part) != std::string::npos;
}

http_status structures_page_status(const std::string& query) {
    http_request req;
    const auto mark = query.find('=');
    if (mark != std::string::npos)
        req.query_params[query.substr(0, mark)] = query.substr(mark + 1);
    return answer_at<structure_routes>(http_method::get, "/api/structures", req, "/api/structures")
        .status;
}

}

TEST_CASE("structures_read_a_type_code_as_a_desk_says_it", "[structure]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing how a type code reads";

    CHECK(structure_routes::humanised("Swap") == "Swap");
    CHECK(structure_routes::humanised("FxBarrierOption") == "FX Barrier Option");
    CHECK(structure_routes::humanised("CdsOption") == "CDS Option");
    CHECK(structure_routes::humanised("IrsSwap") == "IRS Swap");
    CHECK(structure_routes::humanised("ORE") == "ORE");
    CHECK(structure_routes::humanised("") == "");
}

TEST_CASE("structures_name_the_asset_class_the_desk_uses", "[structure]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the asset class names";

    CHECK(structure_routes::asset_class_named("interest_rates") == "Rates");
    CHECK(structure_routes::asset_class_named("fx") == "FX");
    CHECK(structure_routes::asset_class_named("bond") == "Bond");
    CHECK(structure_routes::asset_class_named("credit") == "credit");
    CHECK(structure_routes::asset_class_named("") == "");
}

TEST_CASE("structures_state_how_a_kind_confirms_and_prices", "[structure]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the kind verdicts";

    const auto as_one = structure_routes::verdicts_of("Swap");
    CHECK(as_one.confirms == "Confirmed as one document");
    CHECK(as_one.prices == "Priced as a whole");

    const auto leg_by_leg = structure_routes::verdicts_of("Package");
    CHECK(leg_by_leg.confirms == "Each leg confirms on its own");
    CHECK(leg_by_leg.prices == "Priced leg by leg");
}

TEST_CASE("structures_assemble_the_tree_a_structure_sits_in", "[structure]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the tree a page draws";

    const std::vector<trading_domain::structure> structures{
        structure_of(structure_child, structure_root, "Swap"),
        structure_of(structure_root, "00000000-0000-0000-0000-000000000000", "Package")};
    const std::vector<trading_domain::structure_member> members{
        structure_leg_of(structure_trade, structure_root)};
    const std::map<std::string, std::string> structure_references{{structure_root, "STR-ROOT"},
                                                                  {structure_child, "STR-CHILD"}};
    const std::map<std::string, std::string> trade_references{{structure_trade, "TRD-1"}};
    const std::map<std::string, trading_domain::trade> trades{
        {structure_trade, structure_trade_of(structure_trade, "FxBarrierOption")}};
    const std::map<std::string, std::string> type_names{{"FxBarrierOption", "FX Barrier Option"}};
    const std::map<std::string, std::string> type_asset_classes{{"FxBarrierOption", "fx"}};

    const auto tree = structure_routes::tree_json(structure_uuid(structure_child),
                                                  structures,
                                                  members,
                                                  structure_references,
                                                  trade_references,
                                                  trades,
                                                  type_names,
                                                  type_asset_classes);

    /* The page is on a child, and the tree still starts at the root. */
    CHECK(structure_holds(tree, R"("id":"00000000-0000-0000-0000-0000000000f1")"));
    CHECK(structure_holds(tree, R"("reference":"STR-ROOT")"));
    CHECK(structure_holds(tree, R"("kind":"Package")"));
    CHECK(structure_holds(tree, R"("id":"00000000-0000-0000-0000-0000000000f2")"));
    CHECK(structure_holds(tree, R"("role":"structure")"));
    CHECK(structure_holds(tree, R"("role":"leg")"));
    CHECK(structure_holds(tree, R"("reference":"TRD-1")"));
    CHECK(structure_holds(tree, R"("sequence_number":1)"));
    CHECK(structure_holds(tree, R"("product":"FX Barrier Option")"));
    CHECK(structure_holds(tree, R"("asset_class":"FX")"));
    CHECK(structure_holds(tree, R"("external_version":5)"));
}

TEST_CASE("structures_answer_nothing_for_a_structure_they_do_not_hold", "[structure]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing a tree for a structure the reads lack";

    CHECK(structure_routes::tree_json(structure_uuid(structure_root), {}, {}, {}, {}, {}, {}, {}) ==
          "null");
}

TEST_CASE("structures_refuse_a_malformed_request", "[structure]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the structure routes' refusals";

    CHECK(answer_at<structure_routes>(
              http_method::get, "/api/structure-kinds", http_request{}, "/api/structure-kinds")
              .status == http_status::bad_gateway);
    CHECK(answer_at<structure_routes>(
              http_method::get, "/api/ungrouped-trades", http_request{}, "/api/ungrouped-trades")
              .status == http_status::bad_gateway);

    CHECK(structures_page_status("limit=0") == http_status::bad_request);
    CHECK(structures_page_status("limit=201") == http_status::bad_request);
    CHECK(structures_page_status("offset=-1") == http_status::bad_request);
    CHECK(structures_page_status("sort=trade_date") == http_status::bad_request);
    CHECK(structures_page_status("search=" + std::string(257, 's')) == http_status::bad_request);
    CHECK(structures_page_status("kind=" + std::string(65, 'k')) == http_status::bad_request);
    CHECK(structures_page_status("limit=10") == http_status::bad_gateway);

    const auto grouping = [](const std::string& body, bool signed_in) {
        http_request req;
        req.body = body;
        if (signed_in) {
            ores::security::jwt::jwt_claims claims;
            claims.party_id = "00000000-0000-0000-0000-0000000000d1";
            req.authenticated_user = claims;
        }
        return answer_at<structure_routes>(
                   http_method::post, "/api/structures", req, "/api/structures")
            .status;
    };
    CHECK(grouping(R"({"kind":""})", true) == http_status::bad_request);
    CHECK(grouping(R"({"kind":"Package","tradeIds":[]})", true) == http_status::bad_request);
    CHECK(grouping(R"({"kind":"Package","tradeIds":["acme"]})", true) == http_status::bad_request);
    CHECK(grouping(R"({"kind":"Package","tradeIds":[])", true) == http_status::bad_request);
    CHECK(grouping(R"({"kind":"Package","tradeIds":["00000000-0000-0000-0000-0000000000f3",)"
                   R"("00000000-0000-0000-0000-0000000000f3"]})",
                   true) == http_status::bad_request);
    const std::string one_leg = R"({"kind":"Package","tradeIds":[)"
                                R"("00000000-0000-0000-0000-0000000000f3"]})";
    const std::string two_legs = R"({"kind":"Package","tradeIds":[)"
                                 R"("00000000-0000-0000-0000-0000000000f3",)"
                                 R"("00000000-0000-0000-0000-0000000000f4"]})";
    CHECK(grouping(one_leg, true) == http_status::bad_request);
    CHECK(grouping(two_legs, false) == http_status::bad_request);
    CHECK(grouping(two_legs, true) == http_status::bad_gateway);

    const std::string key = "/api/structures/00000000-0000-0000-0000-0000000000f1";
    CHECK(
        answer_at<structure_routes>(http_method::get, key, http_request{}, "/api/structures/{key}")
            .status == http_status::bad_gateway);
    CHECK(answer_at<structure_routes>(http_method::get,
                                      key + "/leg-candidates",
                                      http_request{},
                                      "/api/structures/{key}/leg-candidates")
              .status == http_status::bad_gateway);

    const auto unlink = [](const std::string& body) {
        http_request req;
        req.body = body;
        return answer_at<structure_routes>(
                   http_method::delete_,
                   "/api/structures/00000000-0000-0000-0000-0000000000f1/legs/"
                   "00000000-0000-0000-0000-0000000000f3",
                   req,
                   "/api/structures/{key}/legs/{tradeId}")
            .status;
    };
    CHECK(unlink(R"({"commentary":""})") == http_status::bad_request);
    CHECK(unlink(R"({"commentary":"not needed"})") == http_status::bad_gateway);

    const auto add_leg = [](const std::string& body) {
        http_request req;
        req.body = body;
        return answer_at<structure_routes>(
                   http_method::post,
                   "/api/structures/00000000-0000-0000-0000-0000000000f1/legs",
                   req,
                   "/api/structures/{key}/legs")
            .status;
    };
    CHECK(add_leg(R"({"tradeId":"acme","role":"leg","commentary":"why"})") ==
          http_status::bad_request);
    CHECK(
        add_leg(
            R"({"tradeId":"00000000-0000-0000-0000-0000000000f3","role":"","commentary":"why"})") ==
        http_status::bad_request);
    CHECK(
        add_leg(
            R"({"tradeId":"00000000-0000-0000-0000-0000000000f3","role":"leg","commentary":""})") ==
        http_status::bad_request);
    CHECK(add_leg(R"({"tradeId":"00000000-0000-0000-0000-0000000000f3","role":"leg",)"
                  R"("commentary":"why"})") == http_status::bad_gateway);
}
