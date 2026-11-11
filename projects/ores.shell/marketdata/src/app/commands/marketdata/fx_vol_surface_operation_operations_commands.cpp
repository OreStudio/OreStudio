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
#include "ores.shell/app/commands/marketdata/fx_vol_surface_operation_operations_commands.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/command_token.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cli/cli.h>
#include <cstddef>
#include <functional>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

void fx_vol_surface_operation_operations_commands::register_commands(cli::Menu& root_menu,
                                                                     nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("fx_vol_surface_operation");

    menu->Insert(
        "calibrate-fx-vol-surface",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_calibrate_fx_vol_surface(std::ref(out), std::ref(session), std::move(args));
        },
        "calibrate-fx-vol-surface <blueprint_id> <as_of>");

    menu->Insert(
        "publish-fx-vol-surface",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_publish_fx_vol_surface(std::ref(out), std::ref(session), std::move(args));
        },
        "publish-fx-vol-surface <blueprint_id> <as_of> [--approval_request_id <v>] [--reason <v>]");

    menu->Insert(
        "get-fx-vol-surface-publish-subject",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_fx_vol_surface_publish_subject(
                std::ref(out), std::ref(session), std::move(args));
        },
        "get-fx-vol-surface-publish-subject <approval_request_id>");

    ores::shell::app::insert_menu(root_menu, std::move(menu));
}

void fx_vol_surface_operation_operations_commands::process_calibrate_fx_vol_surface(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating calibrate-fx-vol-surface request.";

    using request_type = ores::marketdata::messaging::calibrate_fx_vol_surface_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run calibrate-fx-vol-surface." << std::endl;
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
        req.blueprint_id = parsed->positionals[next++];
        req.as_of = ores::shell::app::from_token<std::chrono::system_clock::time_point>(
            parsed->positionals[next++], "as_of");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::marketdata::messaging::calibrate_fx_vol_surface_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::marketdata::messaging::calibrate_fx_vol_surface_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::marketdata::messaging::calibrate_fx_vol_surface_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << ores::utility::codec::wire_traits<std::remove_cvref_t<decltype(*result)>>::write_json(
               *result)
        << std::endl;
}

void fx_vol_surface_operation_operations_commands::process_publish_fx_vol_surface(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating publish-fx-vol-surface request.";

    using request_type = ores::marketdata::messaging::publish_fx_vol_surface_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run publish-fx-vol-surface." << std::endl;
            return;
        }
    }

    const std::vector<flag_spec> specs{
        {.name = "approval_request_id", .requires_value = true, .default_value = ""},
        {.name = "reason", .requires_value = true, .default_value = ""},
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
        req.blueprint_id = parsed->positionals[next++];
        req.as_of = ores::shell::app::from_token<std::chrono::system_clock::time_point>(
            parsed->positionals[next++], "as_of");
        if (const auto& raw_approval_request_id = parsed->flag("approval_request_id");
            !raw_approval_request_id.empty()) {
            req.approval_request_id = raw_approval_request_id;
        }
        if (const auto& raw_reason = parsed->flag("reason"); !raw_reason.empty()) {
            req.reason = raw_reason;
        }
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::marketdata::messaging::publish_fx_vol_surface_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<ores::marketdata::messaging::publish_fx_vol_surface_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result = do_request<ores::marketdata::messaging::publish_fx_vol_surface_response>(
            out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << ores::utility::codec::wire_traits<std::remove_cvref_t<decltype(*result)>>::write_json(
               *result)
        << std::endl;
}

void fx_vol_surface_operation_operations_commands::process_get_fx_vol_surface_publish_subject(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get-fx-vol-surface-publish-subject request.";

    using request_type = ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request;

    // Whether the command presents a token is the protocol's own statement, so
    // a message that establishes the session is never asked for one.
    if constexpr (request_type::requires_session) {
        if (!session.is_logged_in()) {
            fail(out) << "You must be logged in to run get-fx-vol-surface-publish-subject."
                      << std::endl;
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
        req.approval_request_id = parsed->positionals[next++];
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    std::optional<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response> result;
    if constexpr (request_type::requires_session) {
        result = do_auth_request<
            ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>(
            out, session, std::string(req.nats_subject), req);
    } else {
        result =
            do_request<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response>(
                out, session, std::string(req.nats_subject), req);
    }
    if (!result)
        return;

    out << ores::utility::codec::wire_traits<std::remove_cvref_t<decltype(*result)>>::write_json(
               *result)
        << std::endl;
}

}
