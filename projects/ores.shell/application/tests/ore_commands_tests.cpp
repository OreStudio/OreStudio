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
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/commands/trading/ore_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cli/cli.h>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

using ores::nats::service::nats_client;
using ores::shell::app::commands::ore_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.application.tests");
const std::string tags("[commands][ore]");

/**
 * @brief Every case below refuses before the transport is touched.
 *
 * The package commands' happy paths need a live fleet and a provisioned report to
 * run against, so what is pinned here is the menu they hang off and the guard they
 * share: a command that reaches the transport without a session would be a bug the
 * unit suite can catch, and does not need a broker to catch it.
 */
} // anonymous namespace

TEST_CASE("ore_commands registers the package verbs", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    ore_commands::register_commands(root_menu, session);

    const auto completions = root_menu.GetCompletions("ore ");
    for (const auto& verb : {std::string{"ore build-packages"},
                             std::string{"ore fetch-packages"},
                             std::string{"ore clean-packages"}}) {
        INFO("missing completion: " << verb);
        CHECK(std::ranges::find(completions, verb) != completions.end());
    }
}

TEST_CASE("ore clean-packages refuses without a session", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    ore_commands::process_clean_packages(out, session, std::chrono::seconds(3600), false);

    CHECK(out.str().find("Not logged in.") != std::string::npos);
}

TEST_CASE("ore clean-packages refuses without a session when only reporting", tags) {
    auto lg(make_logger(test_suite));

    nats_client session;
    std::ostringstream out;

    // The dry run shares the guard: a report of what would be removed still has to ask
    // the fleet what exists, so it cannot run without a session either.
    ore_commands::process_clean_packages(out, session, std::chrono::seconds(3600), true);

    CHECK(out.str().find("Not logged in.") != std::string::npos);
}
