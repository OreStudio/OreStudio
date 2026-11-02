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
#include "ores.ore.service/messaging/family_row_party.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <string>

namespace {

const std::string tags("[messaging][family_row_party]");

rfl::Generic row_of(const std::string& json) {
    return rfl::json::read<rfl::Generic>(json).value();
}

}

using ores::ore::service::messaging::party_of_family_row;

TEST_CASE("a flat family row states its own party", tags) {
    CHECK(party_of_family_row(row_of(R"({"id":"p1","convention_id":"c1","party_id":"party-a"})")) ==
          "party-a");
}

TEST_CASE("a grouped family row states its party in its identity", tags) {
    // The central convention groups its identity, so a read that looks only at
    // the top level drops every central row and keeps the flat parts as orphans.
    CHECK(party_of_family_row(row_of(
              R"({"identity":{"id":"c1","party_id":"party-a","kind_code":"COMMODITY_FUTURE"},)"
              R"("payload":{"name":"XLME:OP"}})")) == "party-a");
}

TEST_CASE("a family row that names no party has none", tags) {
    CHECK(party_of_family_row(row_of(R"({"id":"x","payload":{"name":"n"}})")).empty());
    CHECK(party_of_family_row(row_of(R"({"identity":{"id":"x"}})")).empty());
}
