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
#include "ores.shell/app/application.hpp"

#include <iostream>
#include <rfl/json.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.shell/service/nats_session.hpp"
#include "ores.shell/app/repl.hpp"

namespace ores::shell::app {

using namespace ores::logging;

application::application(
    std::optional<nats::config::nats_options> connection_config,
    std::optional<config::login_options> login_config)
    : connection_config_(std::move(connection_config))
    , login_config_(std::move(login_config)) {
}

namespace {

inline std::string_view anon_logger_name = "ores.shell.app.application";

auto& anon_lg() {
    static auto instance = make_logger(anon_logger_name);
    return instance;
}

bool auto_connect(service::nats_session& session, std::ostream& out,
    const std::optional<nats::config::nats_options>& cfg) {
    nats::config::nats_options opts;
    if (cfg) opts = *cfg;
    const auto& url = opts.url;
    try {
        session.connect(opts);
        out << "✓ Connected to " << url << std::endl;
        return true;
    } catch (const std::exception& e) {
        out << "✗ Auto-connect failed: " << e.what() << std::endl;
        return false;
    }
}

void check_bootstrap_status(service::nats_session& session, std::ostream& out) {
    try {
        auto reply = session.request("iam.v1.auth.bootstrap-status",
            rfl::json::write(iam::messaging::bootstrap_status_request{}));
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<iam::messaging::bootstrap_status_response>(data_str);
        if (!result) return;
        if (result->is_in_bootstrap_mode) {
            out << "\n⚠ WARNING: System is in BOOTSTRAP MODE\n"
                << "  " << result->message << "\n"
                << "  Use 'bootstrap <principal> <password> <email>' to create admin.\n\n";
        }
    } catch (...) {
        BOOST_LOG_SEV(anon_lg(), debug) << "Bootstrap status check failed";
    }
}

bool auto_login(service::nats_session& session, std::ostream& out,
    const config::login_options& login_config) {
    try {
        iam::messaging::login_request req;
        req.principal = login_config.username;
        req.password = login_config.password;
        auto reply = session.request("iam.v1.auth.login", rfl::json::write(req));
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<iam::messaging::login_response>(data_str);
        if (!result || !result->success) {
            out << "✗ Auto-login failed: "
                << (result ? result->message : "parse error") << std::endl;
            return false;
        }
        service::nats_session::login_info info;
        info.jwt = result->token;
        info.username = result->username;
        info.tenant_id = result->tenant_id;
        info.tenant_name = result->tenant_name;
        session.set_auth(std::move(info));
        out << "✓ Logged in as: " << result->username << std::endl;
        out << "  Tenant: " << result->tenant_name
            << " (" << result->tenant_id << ")" << std::endl;
        return true;
    } catch (const std::exception& e) {
        out << "✗ Auto-login failed: " << e.what() << std::endl;
        return false;
    }
}

} // anonymous namespace

void application::run() {
    BOOST_LOG_SEV(lg(), info) << utility::version::format_startup_message(
        "ORE Studio Shell");

    try {
        service::nats_session session;

        bool connected = auto_connect(session, std::cout, connection_config_);
        if (connected) {
            check_bootstrap_status(session, std::cout);
            if (login_config_) {
                auto_login(session, std::cout, *login_config_);
            }
        }

        repl client_repl(session);
        client_repl.run();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Shell error: " << e.what();
        throw;
    }
}

}
