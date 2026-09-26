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
#include "ores.shell/app/commands/workflow/workflow_operation_commands.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>
#include <string>
#include <string_view>

using ores::nats::service::nats_client;
using ores::shell::app::commands::workflow_operation_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.workflow.tests");
const std::string tags("[commands]");

}

TEST_CASE("workflow_operation_commands_registers_the_wait_verb", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;

    workflow_operation_commands::register_commands(root_menu, session);

    // A publish that declines to block tells the operator to follow progress
    // with this verb, so a wait that is no longer registered turns a shipped
    // instruction into an unknown command.
    const auto completions = root_menu.GetCompletions("workflow ");
    CHECK(std::find(completions.begin(),
                    completions.end(),
                    std::string{"workflow wait"}) != completions.end());
}
