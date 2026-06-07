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
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include <cli/cli.h>
#include <functional>
#include <ostream>
#include <rfl/json.hpp>
#include <stdexcept>

namespace ores::shell::app::commands {

using namespace ores::logging;
using ores::nats::service::nats_client;

namespace {

inline std::string_view anon_logger_name = "ores.shell.app.commands.connection_commands";

auto& anon_lg() {
    static auto instance = make_logger(anon_logger_name);
    return instance;
}

void check_bootstrap_status(nats_client& session, std::ostream& out) {
    try {
        auto reply = session.request(iam::messaging::bootstrap_status_request::nats_subject,
                                     rfl::json::write(iam::messaging::bootstrap_status_request{}));
        auto data_str =
            std::string(reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<iam::messaging::bootstrap_status_response>(data_str);
        if (!result)
            return;
        if (result->is_in_bootstrap_mode) {
            BOOST_LOG_SEV(anon_lg(), warn) << "System is in bootstrap mode: " << result->message;
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

void connection_commands::register_commands(cli::Menu& root_menu, nats_client& session,
                                            nats::config::nats_options connection_template) {
    root_menu.Insert(
        "connect",
        [&session, connection_template](std::ostream& out, std::vector<std::string> args) {
            process_connect(std::ref(out), std::ref(session), connection_template, args);
        },
        "Connect to server. Reuses the startup TLS and subject prefix; "
        "override with the flags below",
        {"[host] [port] [--tls-ca <ca.crt>] [--tls-cert <client.crt>] "
         "[--tls-key <client.key>] [--subject-prefix <prefix>]"});

    root_menu.Insert(
        "disconnect",
        [&session](std::ostream& out) { process_disconnect(std::ref(out), std::ref(session)); },
        "Disconnect from server");
}

void connection_commands::process_connect(std::ostream& out,
                                          nats_client& session,
                                          const nats::config::nats_options& connection_template,
                                          const std::vector<std::string>& args) {
    auto parsed = parse_args(args, {
        {.name = "tls-ca", .requires_value = true, .default_value = ""},
        {.name = "tls-cert", .requires_value = true, .default_value = ""},
        {.name = "tls-key", .requires_value = true, .default_value = ""},
        {.name = "subject-prefix", .requires_value = true, .default_value = ""}
    });
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() > 2) {
        fail(out) << "Usage: connect [host] [port] [--tls-ca <p>] [--tls-cert <p>] "
                     "[--tls-key <p>] [--subject-prefix <p>]" << std::endl;
        return;
    }

    const std::string host =
        !parsed->positionals.empty() ? parsed->positionals[0] : std::string("localhost");
    std::uint16_t resolved_port = 4222;
    if (parsed->positionals.size() == 2) {
        const auto& port = parsed->positionals[1];
        try {
            std::size_t pos = 0;
            const int p = std::stoi(port, &pos);
            if (pos != port.size() || p < 0 || p > 65535)
                throw std::out_of_range("invalid port");
            resolved_port = static_cast<std::uint16_t>(p);
        } catch (...) {
            fail(out) << "Invalid port number: " << port << std::endl;
            return;
        }
    }

    // Start from the connection the binary was launched with, so the
    // subject prefix and TLS context carry over, then override the URL
    // and any flag the user supplied.
    nats::config::nats_options opts = connection_template;
    opts.url = "nats://" + host + ":" + std::to_string(resolved_port);
    if (!parsed->flag("tls-ca").empty())
        opts.tls_ca_cert = parsed->flag("tls-ca");
    if (!parsed->flag("tls-cert").empty())
        opts.tls_client_cert = parsed->flag("tls-cert");
    if (!parsed->flag("tls-key").empty())
        opts.tls_client_key = parsed->flag("tls-key");
    if (!parsed->flag("subject-prefix").empty())
        opts.subject_prefix = parsed->flag("subject-prefix");

    const bool has_tls = !opts.tls_ca_cert.empty() || !opts.tls_client_cert.empty() ||
        !opts.tls_client_key.empty();

    try {
        const auto url = opts.url;
        session.connect(std::move(opts));
        out << "✓ Connected to " << url << std::endl;
        check_bootstrap_status(session, out);
    } catch (const std::exception& e) {
        fail(out) << "Connection failed: " << e.what() << std::endl;
        // The commonest cause of a failed connect against a secured
        // broker is missing TLS material; point the user at the flags.
        if (!has_tls)
            out << "  If the server requires TLS, pass: --tls-ca <ca.crt> "
                   "--tls-cert <client.crt> --tls-key <client.key> "
                   "[--subject-prefix <prefix>]" << std::endl;
    }
}

void connection_commands::process_disconnect(std::ostream& out, nats_client& session) {
    if (!session.is_connected()) {
        fail(out) << "Not connected." << std::endl;
        return;
    }

    session.disconnect();
    out << "✓ Disconnected." << std::endl;
}

}
