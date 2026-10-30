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
#include "ores.shell/app/commands/inbox/notification_operations_commands.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
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
std::vector<std::string> notification_operations_split_list_token(const std::string& value) {
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
 * @brief Read one `name=value` token as the value a message names.
 *
 * The value keeps every `=` it carries: only the first one separates the name
 * from the value, so a link or a title may hold one.
 */
ores::inbox::messaging::notification_argument_value
notification_operations_named_value(const std::string& token) {
    const auto separator = token.find('=');
    if (separator == std::string::npos || separator == 0) {
        throw std::invalid_argument("Expected name=value, got '" + token + "'.");
    }
    return {.name = token.substr(0, separator), .value = token.substr(separator + 1)};
}

}

void notification_operations_commands::register_commands(cli::Menu& root_menu,
                                                         nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("notification_operations");

    menu->Insert(
        "raise-notification",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_raise_notification(std::ref(out), std::ref(session), std::move(args));
        },
        "raise-notification <kind_code> <link_route> <account_ids> [--link_id <v>] [--argument "
        "<name=value>] [--audience_permission_code <v>]");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

void notification_operations_commands::process_raise_notification(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating raise-notification request.";

    using request_type = ores::inbox::messaging::raise_notification_request;

    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run raise-notification." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "link_id", .requires_value = true, .default_value = ""},
        {.name = "argument", .requires_value = true, .default_value = "", .repeatable = true},
        {.name = "audience_permission_code", .requires_value = true, .default_value = ""},
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
        req.kind_code = parsed->positionals[next++];
        req.link_route = parsed->positionals[next++];
        req.account_ids = notification_operations_split_list_token(parsed->positionals[next++]);
        if (const auto& raw_link_id = parsed->flag("link_id"); !raw_link_id.empty())
            req.link_id = raw_link_id;
        if (const auto& repeated = parsed->repeated_flags; repeated.contains("argument")) {
            for (const auto& token : repeated.at("argument"))
                req.arguments.push_back(notification_operations_named_value(token));
        }
        if (const auto& raw_audience = parsed->flag("audience_permission_code");
            !raw_audience.empty()) {
            req.audience_permission_code = raw_audience;
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::inbox::messaging::raise_notification_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::raise_notification_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::raise_notification_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    if (result->result.outcome != ores::utility::domain::outcome::ok)
        fail(out) << result->result.code << ": " << result->result.message << std::endl;

    out << rfl::json::write(*result) << std::endl;
}

}
