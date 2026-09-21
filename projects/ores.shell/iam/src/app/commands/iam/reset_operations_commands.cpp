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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_shell_operation_implementation.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.shell/app/commands/iam/reset_operations_commands.hpp"
#include "ores.iam.api/messaging/reset_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <cstddef>
#include <functional>
#include <optional>
#include <ostream>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

void reset_operations_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("reset");

    menu->Insert(
        "reset-tenant",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_reset_tenant(std::ref(out), std::ref(session), std::move(args));
        },
        "reset-tenant <tenant_code>");

    menu->Insert(
        "reset-system",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_reset_system(std::ref(out), std::ref(session), std::move(args));
        },
        "reset-system");

    root_menu.Insert(std::move(menu));
}

void reset_operations_commands::process_reset_tenant(std::ostream& out,
                                                     nats_client& session,
                                                     const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating reset-tenant request.";

    using request_type = ores::iam::messaging::reset_tenant_command;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run reset-tenant." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 1;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.tenant_code = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::reset_tenant_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::reset_tenant_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::reset_tenant_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void reset_operations_commands::process_reset_system(std::ostream& out,
                                                     nats_client& session,
                                                     const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating reset-system request.";

    using request_type = ores::iam::messaging::reset_system_command;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run reset-system." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 0;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    try {
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::reset_system_result> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::reset_system_result>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::reset_system_result>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
