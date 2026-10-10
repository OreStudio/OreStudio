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
#include "ores.shell/app/shell_root_menu.hpp"
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

namespace {

/**
 * @brief Split a comma-separated token into the elements of a list field.
 *
 * The token __none__ states the empty list, because the command line cannot
 * carry an empty argument: the tokenizer drops one, and a command whose list
 * field should be empty has no other way to say so.
 */
std::vector<std::string> split_list_token(const std::string& value) {
    if (value == "__none__")
        return {};
    std::vector<std::string> parts;
    std::string current;
    for (const char c : value) {
        if (c == ',') {
            parts.push_back(current);
            current.clear();
        } else {
            current.push_back(c);
        }
    }
    parts.push_back(current);
    return parts;
}

/**
 * @brief Read a boolean token, which the shell spells out as a word.
 */
bool parse_flag(const std::string& value, bool& out) {
    if (value.empty() || value == "false") {
        out = false;
        return true;
    }
    if (value == "true") {
        out = true;
        return true;
    }
    return false;
}

}

void authorization_operations_commands::register_commands(cli::Menu& root_menu,
                                                          nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("authorization");

    menu->Insert(
        "assign-role",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_assign_role(std::ref(out), std::ref(session), std::move(args));
        },
        "assign-role <account_id> <role_id> <change_reason_code> <change_commentary>");

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
        "get-my-roles",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_my_roles(std::ref(out), std::ref(session), std::move(args));
        },
        "get-my-roles");

    menu->Insert(
        "list-account-permissions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list_account_permissions(std::ref(out), std::ref(session), std::move(args));
        },
        "list-account-permissions <account_id> <area> <search> [--offset <v>] [--limit <v>]");

    menu->Insert(
        "list-my-permissions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list_my_permissions(std::ref(out), std::ref(session), std::move(args));
        },
        "list-my-permissions <area> <search> [--offset <v>] [--limit <v>]");

    menu->Insert(
        "list-role-permissions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list_role_permissions(std::ref(out), std::ref(session), std::move(args));
        },
        "list-role-permissions <role_id> <area> <search> [--include_unheld <v>] [--offset <v>] "
        "[--limit <v>]");

    menu->Insert(
        "list-roles-page",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list_roles_page(std::ref(out), std::ref(session), std::move(args));
        },
        "list-roles-page <role_id> <search> <area> [--include_service <v>] [--offset <v>] [--limit "
        "<v>]");

    menu->Insert(
        "get-role-permissions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_role_permissions(std::ref(out), std::ref(session), std::move(args));
        },
        "get-role-permissions <role_id>");

    menu->Insert(
        "put-role-permissions",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_put_role_permissions(std::ref(out), std::ref(session), std::move(args));
        },
        "put-role-permissions <role_id> <permission_codes> <change_reason_code> "
        "<change_commentary>");

    menu->Insert(
        "suggest-role-commands",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_suggest_role_commands(std::ref(out), std::ref(session), std::move(args));
        },
        "suggest-role-commands <username> <tenant_id> <hostname>");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

void authorization_operations_commands::process_assign_role(std::ostream& out,
                                                            nats_client& session,
                                                            const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating assign-role request.";

    using request_type = ores::iam::messaging::assign_role_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run assign-role." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.account_id = parsed->positionals[next++];
        req.role_id = parsed->positionals[next++];
        req.change_reason_code = parsed->positionals[next++];
        req.change_commentary = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::assign_role_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::assign_role_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::assign_role_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_assign_role_by_name(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating assign-role-by-name request.";

    using request_type = ores::iam::messaging::assign_role_by_name_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run assign-role-by-name." << std::endl;
            return;
        }
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

    request_type req;
    std::size_t next = 0;
    try {
        req.principal = parsed->positionals[next++];
        req.role_name = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::assign_role_by_name_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::assign_role_by_name_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::assign_role_by_name_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_revoke_role(std::ostream& out,
                                                            nats_client& session,
                                                            const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating revoke-role request.";

    using request_type = ores::iam::messaging::revoke_role_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run revoke-role." << std::endl;
            return;
        }
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

    request_type req;
    std::size_t next = 0;
    try {
        req.account_id = parsed->positionals[next++];
        req.role_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::revoke_role_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::revoke_role_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::revoke_role_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_revoke_role_by_name(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating revoke-role-by-name request.";

    using request_type = ores::iam::messaging::revoke_role_by_name_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run revoke-role-by-name." << std::endl;
            return;
        }
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

    request_type req;
    std::size_t next = 0;
    try {
        req.principal = parsed->positionals[next++];
        req.role_name = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::revoke_role_by_name_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::revoke_role_by_name_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::revoke_role_by_name_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_get_account_roles(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-account-roles request.";

    using request_type = ores::iam::messaging::get_account_roles_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-account-roles." << std::endl;
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
        req.account_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::get_account_roles_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::get_account_roles_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::get_account_roles_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_get_my_roles(std::ostream& out,
                                                             nats_client& session,
                                                             const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-my-roles request.";

    using request_type = ores::iam::messaging::get_my_roles_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-my-roles." << std::endl;
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

    std::optional<ores::iam::messaging::get_account_roles_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::get_account_roles_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::get_account_roles_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_list_account_permissions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list-account-permissions request.";

    using request_type = ores::iam::messaging::list_account_permissions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list-account-permissions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "offset", .requires_value = true, .default_value = ""},
        {.name = "limit", .requires_value = true, .default_value = ""},
    };
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

    request_type req;
    std::size_t next = 0;
    try {
        req.account_id = parsed->positionals[next++];
        req.area = parsed->positionals[next++];
        req.search = parsed->positionals[next++];
        if (const auto& raw_offset = parsed->flag("offset"); !raw_offset.empty()) {
            req.offset = ores::shell::app::from_token<int>(raw_offset, "offset");
        }
        if (const auto& raw_limit = parsed->flag("limit"); !raw_limit.empty()) {
            req.limit = ores::shell::app::from_token<int>(raw_limit, "limit");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::permission_page_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::permission_page_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::permission_page_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_list_my_permissions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list-my-permissions request.";

    using request_type = ores::iam::messaging::list_my_permissions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list-my-permissions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "offset", .requires_value = true, .default_value = ""},
        {.name = "limit", .requires_value = true, .default_value = ""},
    };
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

    request_type req;
    std::size_t next = 0;
    try {
        req.area = parsed->positionals[next++];
        req.search = parsed->positionals[next++];
        if (const auto& raw_offset = parsed->flag("offset"); !raw_offset.empty()) {
            req.offset = ores::shell::app::from_token<int>(raw_offset, "offset");
        }
        if (const auto& raw_limit = parsed->flag("limit"); !raw_limit.empty()) {
            req.limit = ores::shell::app::from_token<int>(raw_limit, "limit");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::permission_page_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::permission_page_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::permission_page_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_list_role_permissions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list-role-permissions request.";

    using request_type = ores::iam::messaging::list_role_permissions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list-role-permissions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "include_unheld", .requires_value = true, .default_value = ""},
        {.name = "offset", .requires_value = true, .default_value = ""},
        {.name = "limit", .requires_value = true, .default_value = ""},
    };
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

    request_type req;
    std::size_t next = 0;
    try {
        req.role_id = parsed->positionals[next++];
        req.area = parsed->positionals[next++];
        req.search = parsed->positionals[next++];
        if (const auto& raw_include_unheld = parsed->flag("include_unheld");
            !raw_include_unheld.empty()) {
            if (!parse_flag(raw_include_unheld, req.include_unheld)) {
                fail(out) << "include_unheld must be 'true' or 'false'." << std::endl;
                return;
            }
        }
        if (const auto& raw_offset = parsed->flag("offset"); !raw_offset.empty()) {
            req.offset = ores::shell::app::from_token<int>(raw_offset, "offset");
        }
        if (const auto& raw_limit = parsed->flag("limit"); !raw_limit.empty()) {
            req.limit = ores::shell::app::from_token<int>(raw_limit, "limit");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::permission_page_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::permission_page_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::permission_page_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_list_roles_page(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list-roles-page request.";

    using request_type = ores::iam::messaging::list_roles_page_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list-roles-page." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "include_service", .requires_value = true, .default_value = ""},
        {.name = "offset", .requires_value = true, .default_value = ""},
        {.name = "limit", .requires_value = true, .default_value = ""},
    };
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

    request_type req;
    std::size_t next = 0;
    try {
        req.role_id = parsed->positionals[next++];
        req.search = parsed->positionals[next++];
        req.area = parsed->positionals[next++];
        if (const auto& raw_include_service = parsed->flag("include_service");
            !raw_include_service.empty()) {
            if (!parse_flag(raw_include_service, req.include_service)) {
                fail(out) << "include_service must be 'true' or 'false'." << std::endl;
                return;
            }
        }
        if (const auto& raw_offset = parsed->flag("offset"); !raw_offset.empty()) {
            req.offset = ores::shell::app::from_token<int>(raw_offset, "offset");
        }
        if (const auto& raw_limit = parsed->flag("limit"); !raw_limit.empty()) {
            req.limit = ores::shell::app::from_token<int>(raw_limit, "limit");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::role_page_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::role_page_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::role_page_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_get_role_permissions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-role-permissions request.";

    using request_type = ores::iam::messaging::get_role_permissions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-role-permissions." << std::endl;
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
        req.role_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::get_role_permissions_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::get_role_permissions_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::get_role_permissions_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_put_role_permissions(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating put-role-permissions request.";

    using request_type = ores::iam::messaging::put_role_permissions_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run put-role-permissions." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 4;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.role_id = parsed->positionals[next++];
        req.permission_codes = split_list_token(parsed->positionals[next++]);
        req.change_reason_code = parsed->positionals[next++];
        req.change_commentary = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::get_role_permissions_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::get_role_permissions_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::get_role_permissions_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void authorization_operations_commands::process_suggest_role_commands(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating suggest-role-commands request.";

    using request_type = ores::iam::messaging::suggest_role_commands_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run suggest-role-commands." << std::endl;
            return;
        }
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

    request_type req;
    std::size_t next = 0;
    try {
        req.username = parsed->positionals[next++];
        req.tenant_id = parsed->positionals[next++];
        req.hostname = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::suggest_role_commands_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::suggest_role_commands_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::suggest_role_commands_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
