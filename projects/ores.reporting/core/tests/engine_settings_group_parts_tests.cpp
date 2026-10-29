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
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.reporting.service/service/engine_settings_proposal.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

// The lifecycle needs two seeds to agree: a group of settings declares in
// ores_reporting_engine_settings_group_settings_tbl whether it changes the
// numbers a run computes, and the approval kind names in
// ores_inbox_approval_kind_parts_tbl the parts that decide a change to it. A
// numbers-changing group with no part, or a part with no numbers-changing
// group, is a screen that offers a proposal nobody can sign: the raise refuses
// it as a kind that names no part for the group.
//
// The check reads the seeded rows through the repositories and the lifecycle
// the service itself reads them through, rather than parsing the seed files.
// A file parse would test the spelling of the SQL; this tests the data the
// service meets at run time, including whatever the insert triggers and the
// system-tenant lookups make of it.

namespace {

using namespace ores::reporting::service;

const auto kind_code = std::string(engine_settings_proposal_service::kind_code);

/** The codes of the groups that declare that they change the numbers. */
std::vector<std::string> numbers_changing_groups(engine_settings_proposal_service& service) {
    std::vector<std::string> codes;
    for (const auto& group : service.groups()) {
        if (group.changes_numbers)
            codes.push_back(group.code);
    }
    return codes;
}

}

TEST_CASE("every_numbers_changing_group_is_a_part_of_the_engine_settings_change_kind",
          "[engine_settings_proposal][seed_consistency]") {
    ores::testing::scoped_database_helper h;
    const auto& ctx = h.context();
    engine_settings_proposal_service service(ctx);

    // The seed must declare at least one, or the check below proves nothing.
    const auto changing = numbers_changing_groups(service);
    REQUIRE_FALSE(changing.empty());

    ores::inbox::service::approval_lifecycle lifecycle(ctx);
    REQUIRE(lifecycle.kind(kind_code).has_value());
    const auto parts = lifecycle.part_codes_of_kind(kind_code);
    REQUIRE_FALSE(parts.empty());

    // A numbers-changing group with no part could never be signed off.
    for (const auto& code : changing) {
        INFO("group " << code << " changes the numbers but is not a part of " << kind_code);
        CHECK(std::ranges::find(parts, code) != parts.end());
    }

    // A part with no numbers-changing group is a part nobody may answer for,
    // because no write to that group is ever a proposal that names it.
    for (const auto& code : parts) {
        INFO("part " << code << " names no group that changes the numbers");
        CHECK(std::ranges::find(changing, code) != changing.end());
    }
}
