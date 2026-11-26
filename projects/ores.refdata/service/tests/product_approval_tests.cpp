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
#include "ores.refdata.service/service/product_approval.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::refdata::service;

namespace {

constexpr auto open_book = "11111111-1111-1111-1111-111111111111";
constexpr auto shut_book = "22222222-2222-2222-2222-222222222222";
constexpr auto narrow_book = "33333333-3333-3333-3333-333333333333";

auto uuid_of(const std::string& text) {
    return boost::uuids::string_generator{}(text);
}

}

TEST_CASE("a_book_that_permits_every_type_answer_no_set", "[product_approval]") {
    const std::vector<product_approval_row> rows{
        {.book_id = open_book, .permits_every_type = true, .product_type_code = {}}};

    const auto approvals = book_product_approvals_from(rows);

    REQUIRE(approvals.size() == 1);
    CHECK(approvals.front().book_id == uuid_of(open_book));
    CHECK_FALSE(approvals.front().allowed_product_types.has_value());
}

TEST_CASE("a_book_that_permits_nothing_answers_an_empty_set_and_not_an_absent_one",
          "[product_approval]") {
    const std::vector<product_approval_row> rows{
        {.book_id = shut_book, .permits_every_type = false, .product_type_code = {}}};

    const auto approvals = book_product_approvals_from(rows);

    REQUIRE(approvals.size() == 1);
    REQUIRE(approvals.front().allowed_product_types.has_value());
    CHECK(approvals.front().allowed_product_types->empty());
}

TEST_CASE("a_book_permits_the_types_its_rows_name", "[product_approval]") {
    const std::vector<product_approval_row> rows{
        {.book_id = narrow_book, .permits_every_type = false, .product_type_code = "Bond"},
        {.book_id = narrow_book, .permits_every_type = false, .product_type_code = "BondOption"}};

    const auto approvals = book_product_approvals_from(rows);

    REQUIRE(approvals.size() == 1);
    REQUIRE(approvals.front().allowed_product_types.has_value());
    CHECK(*approvals.front().allowed_product_types ==
          std::vector<std::string>{"Bond", "BondOption"});
}

TEST_CASE("one_answer_stands_for_each_book_in_the_order_the_books_arrived", "[product_approval]") {
    const std::vector<product_approval_row> rows{
        {.book_id = open_book, .permits_every_type = true, .product_type_code = {}},
        {.book_id = narrow_book, .permits_every_type = false, .product_type_code = "Bond"},
        {.book_id = shut_book, .permits_every_type = false, .product_type_code = {}}};

    const auto approvals = book_product_approvals_from(rows);

    REQUIRE(approvals.size() == 3);
    CHECK(approvals[0].book_id == uuid_of(open_book));
    CHECK(approvals[1].book_id == uuid_of(narrow_book));
    CHECK(approvals[2].book_id == uuid_of(shut_book));
}
