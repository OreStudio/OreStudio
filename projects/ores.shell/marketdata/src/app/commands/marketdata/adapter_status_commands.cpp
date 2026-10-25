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
#include "ores.shell/app/commands/marketdata/adapter_status_commands.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include <cli/cli.h>
#include <iomanip>
#include <memory>
#include <utility>

namespace ores::shell::app::commands {

using ores::nats::service::nats_client;
namespace messaging = ores::marketdata::messaging;

void adapter_status_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("adapters");
    menu->Insert(
        "status",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_status(std::ref(out), std::ref(session), std::move(args));
        },
        "status  — routes seen, ids demanded and accepted, ticks/min and last tick age");
    root_menu.Insert(std::move(menu));
}

void adapter_status_commands::process_status(std::ostream& out,
                                             nats_client& session,
                                             const std::vector<std::string>&) {
    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to read the adapter status." << std::endl;
        return;
    }
    messaging::list_adapter_status_request req;
    auto result = do_request(out, session, req, ores::nats::service::default_request_timeout, true);
    if (!result)
        return;
    if (!result->success) {
        fail(out) << result->message << std::endl;
        return;
    }
    if (result->adapters.empty()) {
        out << "No adapter is known to the service." << std::endl;
        return;
    }
    out << std::left << std::setw(16) << "ADAPTER" << std::setw(10) << "STATE" << std::right
        << std::setw(8) << "ROUTES" << std::setw(10) << "DEMANDED" << std::setw(10) << "ACCEPTED"
        << std::setw(10) << "REJECTED" << std::setw(10) << "TICKS/MIN" << std::setw(12)
        << "LAST TICK" << std::endl;
    for (const auto& adapter : result->adapters) {
        out << std::left << std::setw(16) << adapter.adapter_id << std::setw(10) << adapter.state
            << std::right << std::setw(8) << adapter.routes_seen << std::setw(10)
            << adapter.demanded_count << std::setw(10) << adapter.accepted.size() << std::setw(10)
            << adapter.rejected.size() << std::setw(10) << adapter.ticks_last_minute;
        if (adapter.last_tick_age_ms >= 0)
            out << std::setw(11) << std::fixed << std::setprecision(1)
                << static_cast<double>(adapter.last_tick_age_ms) / 1000.0 << "s";
        else
            out << std::setw(12) << "-";
        out << std::endl;
    }
}

}
