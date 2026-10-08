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
#include "ores.shell/app/commands/inbox/approval_operations_operations_commands.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
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

void approval_operations_operations_commands::register_commands(cli::Menu& root_menu,
                                                                nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("approval_operations");

    menu->Insert(
        "raise-approval",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_raise_approval(std::ref(out), std::ref(session), std::move(args));
        },
        "raise-approval <kind_code> <reason>");

    menu->Insert(
        "withdraw-approval",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_withdraw_approval(std::ref(out), std::ref(session), std::move(args));
        },
        "withdraw-approval <request_id> <comment> [--version <v>]");

    menu->Insert(
        "decide-approval",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_decide_approval(std::ref(out), std::ref(session), std::move(args));
        },
        "decide-approval <request_id> <decision_code> <comment> [--version <v>]");

    menu->Insert(
        "list-approval-queue",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list_approval_queue(std::ref(out), std::ref(session), std::move(args));
        },
        "list-approval-queue [--offset <v>] [--limit <v>]");

    menu->Insert(
        "list-my-approval-requests",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_list_my_approval_requests(std::ref(out), std::ref(session), std::move(args));
        },
        "list-my-approval-requests [--offset <v>] [--limit <v>]");

    menu->Insert(
        "get-approval",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_approval(std::ref(out), std::ref(session), std::move(args));
        },
        "get-approval <request_id>");

    menu->Insert(
        "expire-overdue-approvals",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_expire_overdue_approvals(std::ref(out), std::ref(session), std::move(args));
        },
        "expire-overdue-approvals");

    menu->Insert(
        "get-approval-history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_approval_history(std::ref(out), std::ref(session), std::move(args));
        },
        "get-approval-history <request_id>");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

void approval_operations_operations_commands::process_raise_approval(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating raise-approval request.";

    using request_type = ores::inbox::messaging::raise_approval_request_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run raise-approval." << std::endl;
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
        req.kind_code = parsed->positionals[next++];
        req.reason = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::inbox::messaging::raise_approval_request_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::raise_approval_request_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::raise_approval_request_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void approval_operations_operations_commands::process_withdraw_approval(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating withdraw-approval request.";

    using request_type = ores::inbox::messaging::withdraw_approval_request_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run withdraw-approval." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "version", .requires_value = true, .default_value = ""},
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
        req.request_id = parsed->positionals[next++];
        req.comment = parsed->positionals[next++];
        if (const auto& raw_version = parsed->flag("version"); !raw_version.empty()) {
            req.version = ores::shell::app::from_token<int>(raw_version, "version");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::inbox::messaging::withdraw_approval_request_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::withdraw_approval_request_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::withdraw_approval_request_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void approval_operations_operations_commands::process_decide_approval(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating decide-approval request.";

    using request_type = ores::inbox::messaging::decide_approval_request_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run decide-approval." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "version", .requires_value = true, .default_value = ""},
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
        req.request_id = parsed->positionals[next++];
        req.decision_code = parsed->positionals[next++];
        req.comment = parsed->positionals[next++];
        if (const auto& raw_version = parsed->flag("version"); !raw_version.empty()) {
            req.version = ores::shell::app::from_token<int>(raw_version, "version");
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::inbox::messaging::decide_approval_request_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::decide_approval_request_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::decide_approval_request_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void approval_operations_operations_commands::process_list_approval_queue(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list-approval-queue request.";

    using request_type = ores::inbox::messaging::list_approval_queue_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list-approval-queue." << std::endl;
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

    constexpr std::size_t positional_count = 0;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    try {
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

    std::optional<ores::inbox::messaging::list_approval_queue_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::list_approval_queue_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::list_approval_queue_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void approval_operations_operations_commands::process_list_my_approval_requests(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating list-my-approval-requests request.";

    using request_type = ores::inbox::messaging::list_my_approval_requests_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run list-my-approval-requests." << std::endl;
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

    constexpr std::size_t positional_count = 0;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    try {
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

    std::optional<ores::inbox::messaging::list_my_approval_requests_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::list_my_approval_requests_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::list_my_approval_requests_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void approval_operations_operations_commands::process_get_approval(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-approval request.";

    using request_type = ores::inbox::messaging::get_approval_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-approval." << std::endl;
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
        req.request_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::inbox::messaging::get_approval_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::get_approval_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::get_approval_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void approval_operations_operations_commands::process_expire_overdue_approvals(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating expire-overdue-approvals request.";

    using request_type = ores::inbox::messaging::expire_overdue_approvals_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run expire-overdue-approvals." << std::endl;
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

    std::optional<ores::inbox::messaging::expire_overdue_approvals_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::expire_overdue_approvals_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::expire_overdue_approvals_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void approval_operations_operations_commands::process_get_approval_history(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-approval-history request.";

    using request_type = ores::inbox::messaging::get_approval_history_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-approval-history." << std::endl;
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
        req.request_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::inbox::messaging::get_approval_history_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::inbox::messaging::get_approval_history_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::inbox::messaging::get_approval_history_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
