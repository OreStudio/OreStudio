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
#include "ores.refdata.service/messaging/book_proposal_mapping.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[book_proposal][mapping]");

using ores::refdata::domain::book_change;

book_change line(const std::string& operation) {
    book_change l;
    l.operation = operation;
    return l;
}

}

TEST_CASE("a_put_needs_the_write_permission", tags) {
    const auto needed = ores::refdata::messaging::required_permissions({line("put")});
    CHECK(needed == std::vector<std::string>{"refdata::books:write"});
}

TEST_CASE("a_delete_needs_the_delete_permission_and_not_the_write_permission", tags) {
    const auto needed = ores::refdata::messaging::required_permissions({line("delete")});
    CHECK(needed == std::vector<std::string>{"refdata::books:delete"});
}

TEST_CASE("a_mix_needs_each_permission_once", tags) {
    const auto needed = ores::refdata::messaging::required_permissions(
        {line("put"), line("delete"), line("put"), line("delete")});
    CHECK(needed == std::vector<std::string>{"refdata::books:write", "refdata::books:delete"});
}

TEST_CASE("an_unknown_operation_needs_the_write_permission", tags) {
    const auto needed = ores::refdata::messaging::required_permissions({line("anything")});
    CHECK(needed == std::vector<std::string>{"refdata::books:write"});
}

TEST_CASE("the_outcomes_carry_each_line_as_the_service_found_it", tags) {
    ores::refdata::service::book_preview preview;
    preview.lines.push_back({.line_no = 1,
                             .operation = "put",
                             .entity_id = {},
                             .columns = {"book_status"},
                             .refusal = ""});
    preview.lines.push_back({.line_no = 2,
                             .operation = "delete",
                             .entity_id = {},
                             .columns = {},
                             .refusal = "No book with this id to remove."});

    const auto outcomes = ores::refdata::messaging::to_outcomes(preview);

    REQUIRE(outcomes.size() == 2);
    CHECK(outcomes[0].line_no == 1);
    CHECK(outcomes[0].columns == std::vector<std::string>{"book_status"});
    CHECK(outcomes[0].refusal.empty());
    CHECK(outcomes[1].operation == "delete");
    CHECK(outcomes[1].refusal == "No book with this id to remove.");
}
