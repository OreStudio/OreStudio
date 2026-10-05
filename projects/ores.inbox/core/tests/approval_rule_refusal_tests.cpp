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
#include "ores.inbox.core/messaging/approval_operations_handler.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

namespace {

const std::string tags("[approval]");

}

using ores::inbox::messaging::rule_refusal;
using ores::utility::domain::outcome;

TEST_CASE("a_decision_by_the_asker_reads_as_a_four_eyes_conflict", tags) {
    const auto r = rule_refusal(
        std::runtime_error("ERROR:  The person who asked cannot decide request 1111."));
    REQUIRE(r);
    CHECK(r->outcome == outcome::conflict);
    CHECK(r->code == "four_eyes");
}

TEST_CASE("a_second_approval_reads_as_already_approved", tags) {
    const auto r =
        rule_refusal(std::runtime_error("ERROR:  duplicate key value violates unique constraint "
                                        "\"approval_decisions_one_approval_per_person_idx\""));
    REQUIRE(r);
    CHECK(r->outcome == outcome::conflict);
    CHECK(r->code == "already_approved");
}

TEST_CASE("a_withdrawal_by_someone_else_reads_as_denied", tags) {
    const auto r = rule_refusal(
        std::runtime_error("ERROR:  Only the person who asked can withdraw request 1111."));
    REQUIRE(r);
    CHECK(r->outcome == outcome::denied);
}

TEST_CASE("any_other_error_is_not_a_rule_and_keeps_its_text_out_of_the_reply", tags) {
    CHECK_FALSE(rule_refusal(std::runtime_error("server closed the connection unexpectedly")));
}
