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
#include "ores.shell/app/commands/trading/fx_forward_instrument_commands.hpp"
#include "ores.shell/app/pagination_context.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cli/cli.h>

namespace {

const std::string_view test_suite("ores.shell.trading.tests");
const std::string tags("[commands]");

}

using ores::nats::service::nats_client;
using ores::shell::app::pagination_context;
using ores::shell::app::commands::fx_forward_instrument_commands;
using namespace ores::logging;

TEST_CASE("register_commands_registers_the_pagination_callback", tags) {
    auto lg(make_logger(test_suite));

    cli::Menu root_menu("root");
    nats_client session;
    pagination_context pagination;

    fx_forward_instrument_commands::register_commands(root_menu, session, pagination);

    BOOST_LOG_SEV(lg, debug) << "Looking up the pagination callback for fx_forward_instruments.";
    CHECK(pagination.get_list_callback("fx_forward_instruments") != nullptr);
}
