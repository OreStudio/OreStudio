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
#include "ores.shell/app/commands/connection_commands.hpp"

#include <ostream>
#include <functional>
#include <rfl/json.hpp>
#include <cli/cli.h>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"

namespace ores::shell::app::commands {

using namespace ores::logging;
using service::nats_session;

namespace {

inline std::string_view anon_logger_name =
    "ores.shell.app.commands.connection_commands";

auto& anon_lg() {
    static auto instance = make_logger(anon_logger_name);
    return instance;
}

void check_bootstrap_status(nats_session& session, std::ostream& out) {
    try {
        auto reply = session.request("iam.v1.auth.bootstrap-status",
            rfl::json::write(iam::messaging::bootstrap_status_request{}));
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<iam::messaging::bootstrap_status_response>(data_str);
        if (!result) return;
        if (result->is_in_bootstrap_mode) {
            BOOST_LOG_SEV(anon_lg(), warn) << "System is in bootstrap mode: "
                                           << result->message;
            out << std::endl;
            out << "WARNING: System is in BOOTSTRAP MODE" << std::endl;
            out << "  " << result->message << std::endl;
            out << std::endl;
        } else {
            BOOST_LOG_SEV(anon_lg(), debug) << "Bootstrap status: not in bootstrap mode";
        }
    } catch (...) {
        BOOST_LOG_SEV(anon_lg(), debug) << "Bootstrap status check failed";
    }
}

} // anonymous namespace

void connection_commands::
register_commands(cli::Menu& root_menu, nats_session& session) {
    root_menu.Insert("connect", [&session](std::ostream& out,
            std::string host, std::string port, std::string /*identifier*/) {
        process_connect(std::ref(out), std::ref(session),
            std::move(host), std::move(port), std::string{});
        }, "Connect to server (optional: host port identifier)");

    root_menu.Insert("disconnect", [&session](std::ostream& out) {
        process_disconnect(std::ref(out), std::ref(session));
    }, "Disconnect from server");
}

void connection_commands::
process_connect(std::ostream& out, nats_session& session,
    std::string host, std::string port, std::string /*identifier*/) {

    const std::string resolved_host = host.empty() ? "localhost" : std::move(host);
    std::uint16_t resolved_port = 4222;

    if (!port.empty()) {
        try {
            resolved_port = static_cast<std::uint16_t>(std::stoi(port));
        } catch (...) {
            out << "✗ Invalid port number: " << port << std::endl;
            return;
        }
    }

    const std::string nats_url =
        "nats://" + resolved_host + ":" + std::to_string(resolved_port);

    try {
        nats::config::nats_options opts;
        opts.url = nats_url;
        session.connect(std::move(opts));
        out << "✓ Connected to " << nats_url << std::endl;
        check_bootstrap_status(session, out);
    } catch (const std::exception& e) {
        out << "✗ Connection failed: " << e.what() << std::endl;
    }
}

void connection_commands::
process_disconnect(std::ostream& out, nats_session& session) {
    if (!session.is_connected()) {
        out << "✗ Not connected." << std::endl;
        return;
    }

    session.disconnect();
    out << "✓ Disconnected." << std::endl;
}

}
