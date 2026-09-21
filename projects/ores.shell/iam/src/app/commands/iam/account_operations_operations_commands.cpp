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
#include "ores.shell/app/commands/iam/account_operations_operations_commands.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
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

namespace {

/**
 * @brief Split a comma-separated token into the elements of a list field.
 */
std::vector<std::string> split_list_token(const std::string& value) {
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

} // namespace

void account_operations_operations_commands::register_commands(cli::Menu& root_menu,
                                                               nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("account_operations");

    menu->Insert(
        "save-account",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_save_account(std::ref(out), std::ref(session), std::move(args));
        },
        "save-account <principal> <password> <totp_secret> <email> <account_type>");

    menu->Insert(
        "update-account",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_update_account(std::ref(out), std::ref(session), std::move(args));
        },
        "update-account <account_id> <email> <full_name> <default_party_id> <job_title> "
        "<reports_to_account_id> <image_id> <change_reason_code> <change_commentary>");

    menu->Insert(
        "delete-account",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_delete_account(std::ref(out), std::ref(session), std::move(args));
        },
        "delete-account <account_id>");

    menu->Insert(
        "lock-account",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_lock_account(std::ref(out), std::ref(session), std::move(args));
        },
        "lock-account <account_ids>");

    menu->Insert(
        "unlock-account",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_unlock_account(std::ref(out), std::ref(session), std::move(args));
        },
        "unlock-account <account_ids>");

    menu->Insert(
        "reset-password",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_reset_password(std::ref(out), std::ref(session), std::move(args));
        },
        "reset-password <account_ids> <new_password>");

    menu->Insert(
        "update-my-email",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_update_my_email(std::ref(out), std::ref(session), std::move(args));
        },
        "update-my-email <email>");

    menu->Insert(
        "set-my-default-party",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_set_my_default_party(std::ref(out), std::ref(session), std::move(args));
        },
        "set-my-default-party <party_id>");

    menu->Insert(
        "select-party",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_select_party(std::ref(out), std::ref(session), std::move(args));
        },
        "select-party <party_id>");

    menu->Insert(
        "switch-party",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_switch_party(std::ref(out), std::ref(session), std::move(args));
        },
        "switch-party <party_id>");

    menu->Insert(
        "change-password",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_change_password(std::ref(out), std::ref(session), std::move(args));
        },
        "change-password <current_password> <new_password>");

    root_menu.Insert(std::move(menu));
}

void account_operations_operations_commands::process_save_account(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating save-account request.";

    using request_type = ores::iam::messaging::save_account_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run save-account." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 5;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.principal = parsed->positionals[next++];
        req.password = parsed->positionals[next++];
        req.totp_secret = parsed->positionals[next++];
        req.email = parsed->positionals[next++];
        req.account_type = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::save_account_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::save_account_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::save_account_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_update_account(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating update-account request.";

    using request_type = ores::iam::messaging::update_account_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run update-account." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{};
    const auto parsed = parse_args(args, specs);
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    constexpr std::size_t positional_count = 9;
    if (parsed->positionals.size() != positional_count) {
        fail(out) << "Expected " << positional_count << " arguments, got "
                  << parsed->positionals.size() << "." << std::endl;
        return;
    }

    request_type req;
    std::size_t next = 0;
    try {
        req.account_id = parsed->positionals[next++];
        req.email = parsed->positionals[next++];
        req.full_name = parsed->positionals[next++];
        req.default_party_id = parsed->positionals[next++];
        req.job_title = parsed->positionals[next++];
        req.reports_to_account_id = parsed->positionals[next++];
        req.image_id = parsed->positionals[next++];
        req.change_reason_code = parsed->positionals[next++];
        req.change_commentary = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::update_account_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::update_account_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::update_account_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_delete_account(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete-account request.";

    using request_type = ores::iam::messaging::delete_account_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run delete-account." << std::endl;
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

    std::optional<ores::iam::messaging::delete_account_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::delete_account_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::delete_account_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_lock_account(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating lock-account request.";

    using request_type = ores::iam::messaging::lock_account_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run lock-account." << std::endl;
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
        req.account_ids = split_list_token(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::lock_account_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::lock_account_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::lock_account_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_unlock_account(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating unlock-account request.";

    using request_type = ores::iam::messaging::unlock_account_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run unlock-account." << std::endl;
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
        req.account_ids = split_list_token(parsed->positionals[next++]);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::unlock_account_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::unlock_account_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::unlock_account_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_reset_password(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating reset-password request.";

    using request_type = ores::iam::messaging::reset_password_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run reset-password." << std::endl;
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
        req.account_ids = split_list_token(parsed->positionals[next++]);
        req.new_password = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::reset_password_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::reset_password_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::reset_password_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_update_my_email(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating update-my-email request.";

    using request_type = ores::iam::messaging::update_my_email_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run update-my-email." << std::endl;
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
        req.email = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::update_my_email_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::update_my_email_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::update_my_email_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_set_my_default_party(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating set-my-default-party request.";

    using request_type = ores::iam::messaging::set_my_default_party_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run set-my-default-party." << std::endl;
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
        req.party_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::set_my_default_party_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::set_my_default_party_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::set_my_default_party_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_select_party(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating select-party request.";

    using request_type = ores::iam::messaging::select_party_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run select-party." << std::endl;
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
        req.party_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::select_party_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::select_party_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::select_party_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_switch_party(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating switch-party request.";

    using request_type = ores::iam::messaging::switch_party_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run switch-party." << std::endl;
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
        req.party_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::select_party_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::select_party_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::select_party_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

void account_operations_operations_commands::process_change_password(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating change-password request.";

    using request_type = ores::iam::messaging::change_password_request_typed;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run change-password." << std::endl;
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
        req.current_password = parsed->positionals[next++];
        req.new_password = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::iam::messaging::change_password_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::iam::messaging::change_password_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::iam::messaging::change_password_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << rfl::json::write(*result) << std::endl;
}

}
