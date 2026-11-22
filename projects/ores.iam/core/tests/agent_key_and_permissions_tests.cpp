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
#include "ores.iam.core/service/agent_key_secrets.hpp"
#include "ores.iam.core/service/agent_permissions.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[agent_grants]");

using namespace ores::iam::service;

bool has(const std::vector<std::string>& codes, const std::string& code) {
    return std::ranges::find(codes, code) != codes.end();
}

const std::vector<std::string> catalogue = {"refdata::currencies:read",
                                            "trading::trades:read",
                                            "trading::trades:write",
                                            "trading::trade_authorisation_requests:write",
                                            "iam::accounts:read",
                                            "iam::accounts:create",
                                            "iam::roles:assign",
                                            "iam::agent_grants:create",
                                            "iam::agent_grants:read",
                                            "iam::agent_keys:exchange",
                                            "iam::run_grants:exchange",
                                            "iam::storage_capabilities:mint",
                                            "iam::tenants:impersonate",
                                            "inbox::approvals:decide_operations",
                                            "inbox::approvals:set_mode",
                                            "inbox::approval_requests:read",
                                            "installation::deployment:read",
                                            "marketdata::curve_bootstrap:publish",
                                            "*",
                                            "trading::*"};

}

TEST_CASE("a_secret_matches_only_its_own_hash", tags) {
    const auto secret = generate_agent_key_secret();
    const auto hash = hash_agent_key_secret(secret);
    CHECK(agent_key_secret_matches(secret, hash));
    CHECK_FALSE(agent_key_secret_matches(generate_agent_key_secret(), hash));
    CHECK_FALSE(agent_key_secret_matches(secret, hash.substr(1)));
    CHECK_FALSE(agent_key_secret_matches(secret, ""));
}

TEST_CASE("a_secret_is_256_random_bits_in_hex", tags) {
    const auto a = generate_agent_key_secret();
    const auto b = generate_agent_key_secret();
    CHECK(a.size() == 64);
    CHECK(a != b);
    CHECK(std::ranges::all_of(
        a, [](char c) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'); }));
}

TEST_CASE("the_hash_is_not_the_secret", tags) {
    const auto secret = generate_agent_key_secret();
    CHECK(hash_agent_key_secret(secret) != secret);
}

TEST_CASE("administration_decisions_and_identities_are_withheld", tags) {
    for (const auto* code : {"installation::deployment:read",
                             "iam::accounts:create",
                             "iam::roles:assign",
                             "iam::agent_grants:create",
                             "iam::agent_grants:read",
                             "iam::agent_keys:read",
                             "iam::agent_keys:exchange",
                             "iam::run_grants:exchange",
                             "iam::storage_capabilities:mint",
                             "iam::tenants:impersonate",
                             "inbox::approvals:decide_operations",
                             "inbox::approvals:set_mode",
                             "trading::trade_authorisation_requests:write"}) {
        INFO(code);
        CHECK(is_withheld_from_agents(code));
    }
}

TEST_CASE("ordinary_reads_and_domain_writes_are_not_withheld", tags) {
    for (const auto* code : {"refdata::currencies:read",
                             "iam::accounts:read",
                             "inbox::approval_requests:read",
                             "trading::trades:write",
                             "marketdata::curve_bootstrap:publish"}) {
        INFO(code);
        CHECK_FALSE(is_withheld_from_agents(code));
    }
}

TEST_CASE("an_agent_holds_what_the_role_and_the_person_both_hold", tags) {
    const std::vector<std::string> role = {"refdata::currencies:read", "trading::trades:write"};
    const std::vector<std::string> person = {"refdata::currencies:read", "trading::trades:read"};
    const auto got = agent_permissions(catalogue, role, person);
    CHECK(got == std::vector<std::string>{"refdata::currencies:read"});
}

TEST_CASE("a_tenant_administrator_delegates_no_administration", tags) {
    const std::vector<std::string> role = {"*"};
    const std::vector<std::string> person = {"*"};
    const auto got = agent_permissions(catalogue, role, person);
    CHECK(has(got, "trading::trades:write"));
    CHECK(has(got, "refdata::currencies:read"));
    for (const auto& code : got) {
        INFO(code);
        CHECK_FALSE(is_withheld_from_agents(code));
        CHECK(code != "*");
        CHECK_FALSE(code.ends_with("::*"));
    }
    CHECK_FALSE(has(got, "installation::deployment:read"));
    CHECK_FALSE(has(got, "iam::roles:assign"));
    CHECK_FALSE(has(got, "inbox::approvals:decide_operations"));
}

TEST_CASE("a_role_with_a_wildcard_cannot_be_granted", tags) {
    const std::vector<std::string> role = {"trading::*"};
    const std::vector<std::string> person = {"*"};
    CHECK_FALSE(agent_role_refusal(role, person).empty());
}

TEST_CASE("a_role_naming_a_withheld_code_cannot_be_granted", tags) {
    const std::vector<std::string> role = {"trading::trades:read", "iam::roles:assign"};
    const std::vector<std::string> person = {"*"};
    CHECK_FALSE(agent_role_refusal(role, person).empty());
}

TEST_CASE("a_person_cannot_delegate_what_they_do_not_hold", tags) {
    const std::vector<std::string> role = {"trading::trades:write"};
    const std::vector<std::string> person = {"trading::trades:read"};
    CHECK_FALSE(agent_role_refusal(role, person).empty());
}

TEST_CASE("an_explicit_role_the_person_holds_can_be_granted", tags) {
    const std::vector<std::string> role = {"trading::trades:read", "trading::trades:write"};
    const std::vector<std::string> person = {"trading::*"};
    CHECK(agent_role_refusal(role, person).empty());
}
