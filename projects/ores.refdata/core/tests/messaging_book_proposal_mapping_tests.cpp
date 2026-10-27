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

using ores::refdata::domain::book_change;

book_change line(const std::string& operation) {
    book_change l;
    l.operation = operation;
    return l;
}

}

TEST_CASE("a_put_needs_the_write_permission", "[book_proposal][mapping]") {
    const auto needed = ores::refdata::messaging::required_permissions({line("put")});
    CHECK(needed == std::vector<std::string>{"refdata::books:write"});
}

TEST_CASE("a_delete_needs_the_delete_permission_and_not_the_write_permission", "[book_proposal][mapping]") {
    const auto needed = ores::refdata::messaging::required_permissions({line("delete")});
    CHECK(needed == std::vector<std::string>{"refdata::books:delete"});
}

TEST_CASE("a_mix_needs_each_permission_once", "[book_proposal][mapping]") {
    const auto needed = ores::refdata::messaging::required_permissions(
        {line("put"), line("delete"), line("put"), line("delete")});
    CHECK(needed == std::vector<std::string>{"refdata::books:write", "refdata::books:delete"});
}

TEST_CASE("no_lines_cannot_be_proposed", "[book_proposal][mapping]") {
    CHECK_FALSE(ores::refdata::messaging::invalid_lines({}).empty());
}

TEST_CASE("an_unknown_operation_cannot_be_proposed_and_is_not_counted_as_a_write", "[book_proposal][mapping]") {
    CHECK_FALSE(ores::refdata::messaging::invalid_lines({line("put"), line("purge")}).empty());
    CHECK(ores::refdata::messaging::required_permissions({line("purge")}).empty());
}

TEST_CASE("puts_and_deletes_can_be_proposed", "[book_proposal][mapping]") {
    CHECK(ores::refdata::messaging::invalid_lines({line("put"), line("delete")}).empty());
}

TEST_CASE("a_recheck_with_no_refusal_completes_its_step", "[book_proposal][mapping]") {
    ores::refdata::service::book_preview preview;
    preview.lines.push_back(
        {.line_no = 1, .operation = "put", .entity_id = {}, .columns = {}, .refusal = ""});

    CHECK(ores::refdata::messaging::recheck_step_result(preview).completed);
}

TEST_CASE("a_recheck_fails_its_step_with_the_first_refused_line", "[book_proposal][mapping]") {
    ores::refdata::service::book_preview preview;
    preview.lines.push_back(
        {.line_no = 1, .operation = "put", .entity_id = {}, .columns = {}, .refusal = ""});
    preview.lines.push_back(
        {.line_no = 2, .operation = "put", .entity_id = {}, .columns = {}, .refusal = "stale"});
    preview.lines.push_back(
        {.line_no = 3, .operation = "put", .entity_id = {}, .columns = {}, .refusal = "other"});

    const auto result = ores::refdata::messaging::recheck_step_result(preview);

    CHECK_FALSE(result.completed);
    CHECK(result.error == "Line 2: stale");
}

TEST_CASE("an_applied_request_completes_its_step", "[book_proposal][mapping]") {
    ores::refdata::service::book_apply applied;
    applied.applied = true;
    CHECK(ores::refdata::messaging::apply_step_result(applied).completed);
}

TEST_CASE("a_refused_apply_fails_its_step_with_the_line_and_the_reason", "[book_proposal][mapping]") {
    ores::refdata::service::book_apply applied;
    applied.failed_line = 3;
    applied.refusal = "The book changed since it was read";

    const auto result = ores::refdata::messaging::apply_step_result(applied);

    CHECK_FALSE(result.completed);
    CHECK(result.error == "Line 3: The book changed since it was read");
}

TEST_CASE("a_transient_apply_fails_its_step_and_says_it_may_run_again", "[book_proposal][mapping]") {
    ores::refdata::service::book_apply applied;
    applied.transient = true;
    applied.refusal = "server closed the connection unexpectedly";

    const auto result = ores::refdata::messaging::apply_step_result(applied);

    CHECK_FALSE(result.completed);
    CHECK(result.error.find("may run again") != std::string::npos);
    CHECK(result.error.find("server closed") != std::string::npos);
}

TEST_CASE("the_outcomes_carry_each_line_as_the_service_found_it", "[book_proposal][mapping]") {
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
