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
#include "ores.refdata.service/messaging/counterparty_proposal_mapping.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

using ores::refdata::service::counterparty_lines;

counterparty_lines counterparties(const std::string& operation) {
    counterparty_lines lines;
    lines.counterparties.emplace_back();
    lines.counterparties.back().operation = operation;
    return lines;
}

counterparty_lines identifiers(const std::string& operation) {
    counterparty_lines lines;
    lines.identifiers.emplace_back();
    lines.identifiers.back().operation = operation;
    return lines;
}

counterparty_lines links(const std::string& operation) {
    counterparty_lines lines;
    lines.links.emplace_back();
    lines.links.back().operation = operation;
    return lines;
}

}

TEST_CASE("each_kind_of_put_needs_the_write_permission_of_that_kind", "[counterparty_proposal][mapping]") {
    using ores::refdata::messaging::required_permissions;
    CHECK(required_permissions(counterparties("put")) ==
          std::vector<std::string>{"refdata::counterparties:write"});
    CHECK(required_permissions(identifiers("put")) ==
          std::vector<std::string>{"refdata::counterparty_identifiers:write"});
    CHECK(required_permissions(links("put")) ==
          std::vector<std::string>{"refdata::party_counterparties:write"});
}

TEST_CASE("each_kind_of_delete_needs_the_delete_permission_of_that_kind", "[counterparty_proposal][mapping]") {
    using ores::refdata::messaging::required_permissions;
    CHECK(required_permissions(counterparties("delete")) ==
          std::vector<std::string>{"refdata::counterparties:delete"});
    CHECK(required_permissions(identifiers("delete")) ==
          std::vector<std::string>{"refdata::counterparty_identifiers:delete"});
    CHECK(required_permissions(links("delete")) ==
          std::vector<std::string>{"refdata::party_counterparties:delete"});
}

TEST_CASE("a_mix_of_kinds_needs_each_permission_once", "[counterparty_proposal][mapping]") {
    auto lines = counterparties("put");
    lines.counterparties.push_back(lines.counterparties.back());
    lines.identifiers = identifiers("put").identifiers;
    lines.links = links("delete").links;

    const auto needed = ores::refdata::messaging::required_permissions(lines);

    CHECK(needed == std::vector<std::string>{"refdata::counterparties:write",
                                             "refdata::counterparty_identifiers:write",
                                             "refdata::party_counterparties:delete"});
}

TEST_CASE("no_lines_cannot_be_proposed", "[counterparty_proposal][mapping]") {
    CHECK_FALSE(ores::refdata::messaging::invalid_lines(counterparty_lines{}).empty());
}

TEST_CASE("an_unknown_operation_in_any_kind_cannot_be_proposed_and_counts_as_no_write", "[counterparty_proposal][mapping]") {
    using ores::refdata::messaging::invalid_lines;
    using ores::refdata::messaging::required_permissions;
    CHECK_FALSE(invalid_lines(counterparties("purge")).empty());
    CHECK_FALSE(invalid_lines(identifiers("purge")).empty());
    CHECK_FALSE(invalid_lines(links("purge")).empty());
    CHECK(required_permissions(links("purge")).empty());
}

TEST_CASE("puts_and_deletes_of_every_kind_can_be_proposed", "[counterparty_proposal][mapping]") {
    auto lines = counterparties("put");
    lines.identifiers = identifiers("delete").identifiers;
    lines.links = links("put").links;

    CHECK(ores::refdata::messaging::invalid_lines(lines).empty());
}

TEST_CASE("a_recheck_fails_its_step_with_the_first_refused_line", "[counterparty_proposal][mapping]") {
    ores::refdata::service::counterparty_preview preview;
    preview.lines.push_back({.line_no = 1, .refusal = ""});
    preview.lines.push_back({.line_no = 2, .refusal = "stale"});
    preview.lines.push_back({.line_no = 3, .refusal = "other"});

    const auto result = ores::refdata::messaging::recheck_step_result(preview);

    CHECK_FALSE(result.completed);
    CHECK(result.error == "Line 2: stale");
}

TEST_CASE("a_recheck_with_no_refusal_completes_its_step", "[counterparty_proposal][mapping]") {
    ores::refdata::service::counterparty_preview preview;
    preview.lines.push_back({.line_no = 1});

    CHECK(ores::refdata::messaging::recheck_step_result(preview).completed);
}

TEST_CASE("an_apply_step_reports_how_the_apply_ended", "[counterparty_proposal][mapping]") {
    using ores::refdata::messaging::apply_step_result;
    ores::refdata::service::counterparty_apply applied;
    applied.applied = true;
    CHECK(apply_step_result(applied).completed);

    ores::refdata::service::counterparty_apply refused;
    refused.failed_line = 3;
    refused.refusal = "The counterparty changed since it was read";
    const auto refusal = apply_step_result(refused);
    CHECK_FALSE(refusal.completed);
    CHECK(refusal.error == "Line 3: The counterparty changed since it was read");

    ores::refdata::service::counterparty_apply transient;
    transient.transient = true;
    transient.refusal = "connection lost";
    const auto retry = apply_step_result(transient);
    CHECK_FALSE(retry.completed);
    CHECK(retry.error.find("may run again") != std::string::npos);
}
