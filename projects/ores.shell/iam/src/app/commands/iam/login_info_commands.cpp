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
 * Template: cpp_shell_command_impl.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.shell/app/commands/iam/login_info_commands.hpp"
#include "ores.iam.api/messaging/login_info_protocol.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/random_generator.hpp>
#include <chrono>
#include <cli/cli.h>
#include <cstddef>
#include <functional>
#include <optional>
#include <ostream>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;

namespace {

/**
 * @brief Fill one request member from one token.
 *
 * A member's own type decides how its token reads, so the caller states the
 * token and the name it answers to and nothing else. The four cases are the
 * four shapes a token has: a word, a flag, a comma-separated list and
 * everything from_token already converts.
 */
template <typename T>
void read_token(T& target, const std::string& raw, const std::string& name) {
    if constexpr (std::is_same_v<T, std::string>) {
        target = raw;
    } else if constexpr (std::is_same_v<T, bool>) {
        if (raw.empty() || raw == "false") {
            target = false;
        } else if (raw == "true") {
            target = true;
        } else {
            throw std::invalid_argument(name + " must be 'true' or 'false'");
        }
    } else if constexpr (std::is_same_v<T, std::chrono::system_clock::time_point>) {
        target = ores::platform::time::datetime::from_iso8601_utc(raw);
    } else if constexpr (std::is_same_v<T, boost::asio::ip::address>) {
        target = boost::asio::ip::make_address(raw);
    } else if constexpr (std::is_same_v<T, std::vector<std::string>>) {
        target.clear();
        std::string current;
        for (const char c : raw) {
            if (c == ',') {
                target.push_back(current);
                current.clear();
            } else {
                current.push_back(c);
            }
        }
        target.push_back(current);
    } else {
        target = ores::shell::app::from_token<T>(raw, name);
    }
}

/// Apply the page and the order a caller stated, leaving the defaults alone.
template <typename Request>
void apply_page(Request& req, const parsed_args& parsed) {
    if (const auto& raw = parsed.flag("offset"); !raw.empty()) {
        req.offset = ores::shell::app::from_token<std::uint32_t>(raw, "offset");
    }
    if (const auto& raw = parsed.flag("limit"); !raw.empty()) {
        req.limit = ores::shell::app::from_token<std::uint32_t>(raw, "limit");
    }
    if (const auto& raw = parsed.flag("order"); !raw.empty()) {
        req.order.field = raw;
    }
    req.order.descending = parsed.flag_set("desc");
}

} // namespace

void login_info_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("login_info");

    menu->Insert(
        "list",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list(std::ref(out), std::ref(session), std::move(args));
        },
        "list [--offset <n>] [--limit <n>] [--order <field>] [--desc]");

    menu->Insert(
        "get",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get(std::ref(out), std::ref(session), std::move(args));
        },
        "get <account_id>");

    menu->Insert(
        "get-many",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_many(std::ref(out), std::ref(session), std::move(args));
        },
        "get-many <account_id>");

    root_menu.Insert(std::move(menu));
}

void login_info_commands::process_list(std::ostream& out,
                                       nats_client& session,
                                       const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list request.";

    using request_type = messaging::list_login_info_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "offset", .requires_value = true, .default_value = ""},
        {.name = "limit", .requires_value = true, .default_value = ""},
        {.name = "order", .requires_value = true, .default_value = ""},
        {.name = "desc", .requires_value = false, .default_value = "false"},
    };
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        apply_page(req, *parsed);
        if (!parsed->positionals.empty()) {
            fail(out) << "Expected no arguments, got " << parsed->positionals.size() << "."
                      << std::endl;
            return;
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::list_login_info_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void login_info_commands::process_get(std::ostream& out,
                                      nats_client& session,
                                      const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get request.";

    using request_type = messaging::get_login_info_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.size() != 1) {
            fail(out) << "Expected 1 arguments, got " << parsed->positionals.size() << "."
                      << std::endl;
            return;
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::get_login_info_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void login_info_commands::process_get_many(std::ostream& out,
                                           nats_client& session,
                                           const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-many request.";

    using request_type = messaging::get_many_login_info_request;
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-many." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    request_type req;
    [[maybe_unused]] std::size_t next = 0;
    try {

        if (parsed->positionals.empty() || parsed->positionals.size() % 1 != 0) {
            fail(out) << "Expected a multiple of 1 arguments, got " << parsed->positionals.size()
                      << "." << std::endl;
            return;
        }
        for (std::size_t i = 0; i < parsed->positionals.size(); i += 1) {
            messaging::login_info_key key;
            read_token(key.account_id, parsed->positionals[i + 0], "account_id");
            req.keys.push_back(std::move(key));
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    auto result = do_auth_request<messaging::get_many_login_info_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
