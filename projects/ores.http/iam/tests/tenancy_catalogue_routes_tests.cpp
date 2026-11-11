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
#include "ores.http/routes/iam/tenancy_catalogue_routes.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::http::routes::iam::tenancy_catalogue_routes;

namespace {

std::vector<ores::dq::domain::badge_definition> catalogue_badges() {
    ores::dq::domain::badge_definition active;
    active.code = "green";
    active.name = "Active";
    active.background_colour = "#0a0";
    active.text_colour = "#fff";
    active.severity_code = "success";
    return {active};
}

}

TEST_CASE("tenancy_catalogue_routes_paint_each_status_with_its_badge", "[tenancy]") {
    ores::iam::domain::tenant_status active;
    active.status = "active";
    active.name = "Active";
    active.badge_code = "green";
    ores::iam::domain::tenant_status plain;
    plain.status = "plain";
    plain.badge_code = "";
    const auto text = tenancy_catalogue_routes::statuses_json({active, plain}, catalogue_badges());
    for (const auto* key : {"\"code\":\"active\"",
                            "\"label\":\"Active\"",
                            "\"backgroundColour\":\"#0a0\"",
                            "\"severity\":\"success\"",
                            "\"code\":\"plain\"",
                            "\"badge\":null"})
        CHECK(text.find(key) != std::string::npos);
}

TEST_CASE("tenancy_catalogue_routes_order_the_types_by_their_display_order", "[tenancy]") {
    ores::iam::domain::tenant_type later, first;
    later.type = "later";
    later.display_order = 2;
    first.type = "first";
    first.display_order = 1;
    first.badge_code = "missing";
    const auto text = tenancy_catalogue_routes::types_json({later, first}, catalogue_badges());
    CHECK(text.find("\"code\":\"first\"") < text.find("\"code\":\"later\""));
    CHECK(text.find("\"badge\":null") != std::string::npos);
}

TEST_CASE("tenancy_catalogue_routes_answer_the_legal_entities_in_the_screen_shape", "[tenancy]") {
    ores::dq::messaging::search_lei_entities_response reply;
    ores::dq::messaging::lei_entity_match match;
    match.lei = "5493001KJTIIGC8Y1R12";
    match.entity_legal_name = "Acme";
    match.country = "GB";
    match.party_count = 3;
    reply.entities.push_back(match);
    const auto text = tenancy_catalogue_routes::entities_json(reply);
    for (const auto* key : {"\"lei\":\"5493001KJTIIGC8Y1R12\"",
                            "\"legalName\":\"Acme\"",
                            "\"country\":\"GB\"",
                            "\"partyCount\":3"})
        CHECK(text.find(key) != std::string::npos);
}
