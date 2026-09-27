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
#include "ores.logging/make_logger.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <memory>
#include <ostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.shell.tests");
const std::string tags("[shell_root_menu]");

std::unique_ptr<cli::Menu> menu_named(const std::string& name,
                                      const std::string& verb) {
    auto menu = std::make_unique<cli::Menu>(name);
    menu->Insert(verb, [](std::ostream&) {}, verb);
    return menu;
}

}

using namespace ores::logging;
using ores::shell::app::claim_name;
using ores::shell::app::extend_menu;
using ores::shell::app::insert_menu;
using ores::shell::app::shell_root_menu;

TEST_CASE("insert_menu_refuses_a_second_owner_of_one_name", tags) {
    auto lg(make_logger(test_suite));

    shell_root_menu root;
    insert_menu(root, menu_named("accounts", "list"));

    REQUIRE_THROWS_AS(insert_menu(root, menu_named("accounts", "get")), std::runtime_error);

    try {
        insert_menu(root, menu_named("accounts", "get"));
    } catch (const std::runtime_error& e) {
        CHECK(std::string(e.what()).find("accounts") != std::string::npos);
    }

    BOOST_LOG_SEV(lg, debug) << "A duplicate menu name is refused.";
}

TEST_CASE("insert_menu_accepts_two_different_names", tags) {
    auto lg(make_logger(test_suite));

    shell_root_menu root;
    insert_menu(root, menu_named("accounts", "list"));
    insert_menu(root, menu_named("roles", "list"));

    const auto completions = root.GetCompletions("");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"accounts"}) !=
          completions.end());
    CHECK(std::find(completions.begin(), completions.end(), std::string{"roles"}) !=
          completions.end());
}

TEST_CASE("extend_menu_adds_verbs_to_the_menu_another_unit_owns", tags) {
    auto lg(make_logger(test_suite));

    shell_root_menu root;
    insert_menu(root, menu_named("accounts", "list"));

    // The generated unit registers first and owns the menu. The hand-written
    // unit beside it contributes the verbs the model cannot express.
    extend_menu(root, "accounts", [](cli::Menu& accounts) {
        accounts.Insert("login", [](std::ostream&) {}, "Log in");
        accounts.Insert("lock", [](std::ostream&) {}, "Lock an account");
    });

    root.apply_extensions();

    const auto completions = root.GetCompletions("accounts ");
    for (const auto& verb : {std::string{"accounts list"},
                             std::string{"accounts login"},
                             std::string{"accounts lock"}})
        CHECK(std::find(completions.begin(), completions.end(), verb) != completions.end());

    BOOST_LOG_SEV(lg, debug) << "One menu answers both units' verbs.";
}

TEST_CASE("apply_extensions_refuses_an_extension_for_an_unowned_name", tags) {
    auto lg(make_logger(test_suite));

    shell_root_menu root;
    extend_menu(root, "accounts", [](cli::Menu&) {});

    REQUIRE_THROWS_AS(root.apply_extensions(), std::runtime_error);
}

TEST_CASE("claim_name_refuses_a_root_command_that_takes_an_owned_name", tags) {
    auto lg(make_logger(test_suite));

    shell_root_menu root;
    insert_menu(root, menu_named("accounts", "list"));

    REQUIRE_THROWS_AS(claim_name(root, "accounts"), std::runtime_error);
    CHECK_NOTHROW(claim_name(root, "login"));
}

TEST_CASE("a_root_that_is_not_the_shells_claims_nothing", tags) {
    auto lg(make_logger(test_suite));

    // A unit test builds its own root, and it has no other unit to collide
    // with. The generated units register against one of these.
    cli::Menu root("root");
    CHECK_NOTHROW(insert_menu(root, menu_named("accounts", "list")));
    CHECK_NOTHROW(insert_menu(root, menu_named("accounts", "list")));
    CHECK_NOTHROW(claim_name(root, "accounts"));

    const auto completions = root.GetCompletions("");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"accounts"}) !=
          completions.end());
}

TEST_CASE("extend_menu_on_a_plain_root_gives_the_verbs_their_own_menu", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root("root");
    extend_menu(root, "accounts", [](cli::Menu& accounts) {
        accounts.Insert("login", [](std::ostream&) {}, "Log in");
    });

    const auto completions = root.GetCompletions("accounts ");
    CHECK(std::find(completions.begin(), completions.end(), std::string{"accounts login"}) !=
          completions.end());
}
