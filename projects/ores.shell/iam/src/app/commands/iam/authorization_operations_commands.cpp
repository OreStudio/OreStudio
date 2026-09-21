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
#include "ores.shell/app/commands/iam/authorization_operations_commands.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <cli/cli.h>
#include <cstddef>
#include <functional>
#include <ostream>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

void authorization_operations_commands::register_commands(cli::Menu& root_menu,
                                                          nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("authorization");

    menu->Insert(
        "assign-role",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_assign_role(std::ref(out), std::ref(session), std::move(args));
        },
        "assign-role <account_id> <role_id>");

    menu->Insert(
        "assign-role-by-name",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_assign_role_by_name(std::ref(out), std::ref(session), std::move(args));
        },
        "assign-role-by-name <principal> <role_name>");

    menu->Insert(
        "revoke-role",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_revoke_role(std::ref(out), std::ref(session), std::move(args));
        },
        "revoke-role <account_id> <role_id>");

    menu->Insert(
        "revoke-role-by-name",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_revoke_role_by_name(std::ref(out), std::ref(session), std::move(args));
        },
        "revoke-role-by-name <principal> <role_name>");

    menu->Insert(
        "get-account-roles",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_account_roles(std::ref(out), std::ref(session), std::move(args));
        },
        "get-account-roles <account_id>");

    menu->Insert(
        "get-role-permissions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_role_permissions(std::ref(out), std::ref(session), std::move(args));
        },
        "get-role-permissions <role_id>");

    menu->Insert(
        "suggest-role-commands",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_suggest_role_commands(std::ref(out), std::ref(session), std::move(args));
        },
        "suggest-role-commands <username> <tenant_id> <hostname>");

    root_menu.Insert(std::move(menu));
}

void authorization_operations_commands::process_assign_role(std::ostream& out,
                                                            nats_client& session,
                                                            const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating assign-role request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run assign-role." << std::endl;
        return;
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 2;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    ores::iam::messaging::assign_role_request req;
    std::size_t next = 0;
    try {
        req.account_id = parsed->positionals[next++];
        req.role_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<ores::iam::messaging::assign_role_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_assign_role_by_name(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating assign-role-by-name request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run assign-role-by-name." << std::endl;
        return;
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 2;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    ores::iam::messaging::assign_role_by_name_request req;
    std::size_t next = 0;
    try {
        req.principal = parsed->positionals[next++];
        req.role_name = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<ores::iam::messaging::assign_role_by_name_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_revoke_role(std::ostream& out,
                                                            nats_client& session,
                                                            const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating revoke-role request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run revoke-role." << std::endl;
        return;
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 2;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    ores::iam::messaging::revoke_role_request req;
    std::size_t next = 0;
    try {
        req.account_id = parsed->positionals[next++];
        req.role_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<ores::iam::messaging::revoke_role_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_revoke_role_by_name(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating revoke-role-by-name request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run revoke-role-by-name." << std::endl;
        return;
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 2;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    ores::iam::messaging::revoke_role_by_name_request req;
    std::size_t next = 0;
    try {
        req.principal = parsed->positionals[next++];
        req.role_name = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<ores::iam::messaging::revoke_role_by_name_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_get_account_roles(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-account-roles request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run get-account-roles." << std::endl;
        return;
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

    ores::iam::messaging::get_account_roles_request req;
    std::size_t next = 0;
    try {
        req.account_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<ores::iam::messaging::get_account_roles_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_get_role_permissions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-role-permissions request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run get-role-permissions." << std::endl;
        return;
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

    ores::iam::messaging::get_role_permissions_request req;
    std::size_t next = 0;
    try {
        req.role_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<ores::iam::messaging::get_role_permissions_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_suggest_role_commands(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating suggest-role-commands request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to run suggest-role-commands." << std::endl;
        return;
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 3;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    ores::iam::messaging::suggest_role_commands_request req;
    std::size_t next = 0;
    try {
        req.username = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.hostname = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<ores::iam::messaging::suggest_role_commands_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
