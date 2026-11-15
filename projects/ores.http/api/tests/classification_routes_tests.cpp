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
#include "ores.http.api/net/classification_routes.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <map>
#include <string>
#include <vector>

namespace cr = ores::http::net::classification_routes;

namespace {

/// A row of a list with a name and an order.
struct classification_named_row {
    int version = 0;
    std::string code;
    std::string name;
    std::string description;
    int display_order = 0;
    std::string modified_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

/// A row of a list with neither a name nor an order.
struct classification_plain_row {
    int version = 0;
    std::string code;
    std::string description;
    std::string modified_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

struct classification_named_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order{};
};

struct classification_plain_write {
    std::string code;
    std::string description;
};

}

TEST_CASE("classification_routes_lists_rows_by_order_then_code", "[classification_routes]") {
    std::vector<classification_named_row> rows(3);
    rows[0].code = "B";
    rows[0].display_order = 2;
    rows[1].code = "C";
    rows[1].display_order = 1;
    rows[2].code = "A";
    rows[2].display_order = 2;
    cr::sort_rows(rows);
    CHECK(rows[0].code == "C");
    CHECK(rows[1].code == "A");
    CHECK(rows[2].code == "B");

    std::vector<classification_plain_row> plain(2);
    plain[0].code = "Z";
    plain[1].code = "M";
    cr::sort_rows(plain);
    CHECK(plain[0].code == "M");
}

TEST_CASE("classification_routes_writes_a_row_with_its_label_and_the_members_it_has",
          "[classification_routes]") {
    classification_named_row named;
    named.code = "X";
    named.name = "Ex";
    named.display_order = 3;
    named.version = 4;
    const auto with_label = cr::row_json(named, {{"X", "badge"}});
    CHECK(with_label.find("\"name\":\"Ex\"") != std::string::npos);
    CHECK(with_label.find("\"displayOrder\":3") != std::string::npos);
    CHECK(with_label.find("\"version\":4") != std::string::npos);
    CHECK(with_label.find("\"labelCode\":\"badge\"") != std::string::npos);

    classification_plain_row plain;
    plain.code = "Y";
    const auto without = cr::row_json(plain, {});
    CHECK(without.find("\"name\":\"\"") != std::string::npos);
    CHECK(without.find("\"displayOrder\":null") != std::string::npos);
    CHECK(without.find("\"labelCode\":null") != std::string::npos);
}

TEST_CASE("classification_routes_builds_the_write_its_list_has", "[classification_routes]") {
    const auto named =
        cr::write_of<classification_named_write>("X", std::string("Ex"), std::nullopt, 5);
    CHECK(named.code == "X");
    CHECK(named.name == "Ex");
    CHECK(named.description.empty());
    CHECK(named.display_order == 5);

    const auto plain = cr::write_of<classification_plain_write>(
        "Y", std::string("ignored"), std::string("Why"), 9);
    CHECK(plain.code == "Y");
    CHECK(plain.description == "Why");
}

TEST_CASE("classification_routes_trims_the_reason_and_keeps_the_commentary",
          "[classification_routes]") {
    const auto intent = cr::intent_of("  why \n", std::string("because"));
    CHECK(intent.reason_code == "why");
    CHECK(intent.commentary == "because");
    CHECK(cr::intent_of("x", std::nullopt).commentary.empty());
    CHECK(cr::text_fits(std::nullopt));
    CHECK_FALSE(cr::text_fits(std::string(2001, 'a')));
}
