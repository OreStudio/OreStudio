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
#include "ores.database/domain/context_transact.hpp"
#include "ores.database/pq/statement.hpp"
#include "ores.database/pq/store_error.hpp"
#include "ores.database/pq/transaction.hpp"
#include "ores.iam.core/messaging/bootstrap_handler.hpp"
#include "ores.testing/database_helper.hpp"
#include "test_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>

/*
 * Each case runs in one transaction that the sentinel rolls back, so the shared
 * test database keeps its bootstrap flag and gains no administrator.
 */

namespace {

using ores::database::pq::dynamic_statement;
using ores::database::pq::store_error;
using ores::database::pq::transaction;

struct initial_admin_guard_rolled_back final {};

void set_bootstrap_mode(transaction& tx, bool enabled) {
    tx.run(dynamic_statement(
        std::string("select ores_variability_system_settings_set_fn("
                    "ores_utility_system_tenant_id_fn(), 'system.bootstrap_mode', '") +
        (enabled ? "true" : "false") +
        "', 'boolean', 'Bootstrap mode, set by a test.', 'test', 'system.initial_load', "
        "'Set by a test that rolls back.')"));
}

/// The SQLSTATE a create raises, or nothing when it succeeds. The actor is the
/// principal, as the handler states it: no account exists before the first.
std::optional<std::string> create_admin(transaction& tx, const std::string& principal) {
    try {
        tx.run(dynamic_statement("select ores_iam_create_initial_admin_fn('" + principal + "', '" +
                                 principal + "@example.com', 'not-a-real-hash', '" + principal +
                                 "')"));
        return std::nullopt;
    } catch (const store_error& e) {
        return e.sqlstate();
    }
}

std::string initial_admin_guard_principal() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    return "guard" + text.substr(0, 8);
}

}

TEST_CASE("initial_admin_create_runs_once_in_bootstrap_mode", "[initial_admin_guard]") {
    ores::testing::database_helper h;
    std::optional<std::string> first;
    std::optional<std::string> second;
    CHECK_THROWS_AS(
        ores::database::transact(system_context(h),
                                 [&](transaction& tx) {
                                     set_bootstrap_mode(tx, true);
                                     first = create_admin(tx, initial_admin_guard_principal());
                                     if (!first)
                                         second = create_admin(tx, initial_admin_guard_principal());
                                     throw initial_admin_guard_rolled_back{};
                                 }),
        initial_admin_guard_rolled_back);
    INFO("first create: " << first.value_or("succeeded"));
    CHECK_FALSE(first.has_value());
    CHECK(second == std::optional<std::string>("55000"));
}

TEST_CASE("initial_admin_create_refuses_outside_bootstrap_mode", "[initial_admin_guard]") {
    ores::testing::database_helper h;
    std::optional<std::string> refused;
    CHECK_THROWS_AS(ores::database::transact(system_context(h),
                                             [&](transaction& tx) {
                                                 set_bootstrap_mode(tx, false);
                                                 refused = create_admin(
                                                     tx, initial_admin_guard_principal());
                                                 throw initial_admin_guard_rolled_back{};
                                             }),
                    initial_admin_guard_rolled_back);
    CHECK(refused == std::optional<std::string>("55000"));
}

TEST_CASE("initial_admin_create_answers_fixed_words", "[initial_admin_guard]") {
    using ores::iam::messaging::bootstrap_handler;
    CHECK(bootstrap_handler::create_refusal("55000") == "System is not in bootstrap mode");
    CHECK(bootstrap_handler::create_refusal("22023") ==
          "A username, an email address and a password are required.");
    CHECK(bootstrap_handler::create_refusal("23505") == "That username is already taken.");
    CHECK(bootstrap_handler::create_refusal("42P01") == "The administrator could not be created.");
    CHECK(bootstrap_handler::create_refusal("") == "The administrator could not be created.");
}
