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
#include "ores.shell/app/commands/storage/objects_operations_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.storage.api/messaging/objects_protocol.hpp"
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

void objects_operations_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("objects");

    menu->Insert(
        "put-objects",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_put_objects(std::ref(out), std::ref(session), std::move(args));
        },
        "put-objects <bucket> <key> <content> <content_encoding>");

    menu->Insert(
        "get-objects",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_objects(std::ref(out), std::ref(session), std::move(args));
        },
        "get-objects <bucket> <key> [--include_content <v>]");

    menu->Insert(
        "delete-objects",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_delete_objects(std::ref(out), std::ref(session), std::move(args));
        },
        "delete-objects <bucket> <key>");

    menu->Insert(
        "list-objects",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list_objects(std::ref(out), std::ref(session), std::move(args));
        },
        "list-objects <bucket> <prefix> [--offset <v>] [--limit <v>]");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

void objects_operations_commands::process_put_objects(std::ostream& out,
                                                      nats_client& session,
                                                      const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating put-objects request.";

    using request_type = ores::storage::messaging::put_objects_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run put-objects." << std::endl;
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
        req.bucket = parsed->positionals[next++];
        req.key = parsed->positionals[next++];
        req.content = parsed->positionals[next++];
        req.content_encoding = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::storage::messaging::put_objects_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::storage::messaging::put_objects_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::storage::messaging::put_objects_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void objects_operations_commands::process_get_objects(std::ostream& out,
                                                      nats_client& session,
                                                      const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-objects request.";

    using request_type = ores::storage::messaging::get_objects_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-objects." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "include_content", .requires_value = true, .default_value = ""},
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
        req.bucket = parsed->positionals[next++];
        req.key = parsed->positionals[next++];
        if (const auto& raw_include_content = parsed->flag("include_content");
            !raw_include_content.empty()) {
            if (!parse_flag(raw_include_content, req.include_content)) {
                fail(out) << "include_content must be 'true' or 'false'." << std::endl;
                return;
            }
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::storage::messaging::get_objects_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::storage::messaging::get_objects_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::storage::messaging::get_objects_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void objects_operations_commands::process_delete_objects(std::ostream& out,
                                                         nats_client& session,
                                                         const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete-objects request.";

    using request_type = ores::storage::messaging::delete_objects_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run delete-objects." << std::endl;
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
        req.bucket = parsed->positionals[next++];
        req.key = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::storage::messaging::delete_objects_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::storage::messaging::delete_objects_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::storage::messaging::delete_objects_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void objects_operations_commands::process_list_objects(std::ostream& out,
                                                       nats_client& session,
                                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list-objects request.";

    using request_type = ores::storage::messaging::list_objects_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list-objects." << std::endl;
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
        req.bucket = parsed->positionals[next++];
        req.prefix = parsed->positionals[next++];
        if (const auto& raw_offset = parsed->flag("offset"); !raw_offset.empty()) {
            req.offset = ores::shell::app::from_token<std::uint32_t>(raw_offset, "offset");
        }
        if (const auto& raw_limit = parsed->flag("limit"); !raw_limit.empty()) {
            req.limit = ores::shell::app::from_token<std::uint32_t>(raw_limit, "limit");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::storage::messaging::list_objects_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::storage::messaging::list_objects_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::storage::messaging::list_objects_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
