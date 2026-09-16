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
#include "ores.shell/app/commands/trading/trade_identifier_commands.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/commands/history_diff_renderer.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/domain/trade_identifier_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::trading::domain;

namespace {

std::optional<boost::uuids::uuid>
parse_uuid(std::ostream& out, const std::string& value, std::string_view what) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(value);
    } catch (const boost::bad_lexical_cast&) {
        fail(out) << "Invalid " << what << " format. Expected UUID: " << value << std::endl;
        return std::nullopt;
    }
}

}

void trade_identifier_commands::register_commands(cli::Menu& root_menu,
                                                  nats_client& session,
                                                  pagination_context& pagination) {
    auto trade_identifiers_menu = std::make_unique<cli::Menu>("trade_identifiers");

    trade_identifiers_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_trade_identifiers(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve trade identifiers from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("trade_identifiers",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_trade_identifiers(out, session, pagination);
                                      });

    trade_identifiers_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_id,
                   std::string id_value,
                   std::string id_type,
                   std::string id_scheme,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_trade_identifier(std::ref(out),
                                         std::ref(session),
                                         std::move(trade_id),
                                         std::move(id_value),
                                         std::move(id_type),
                                         std::move(id_scheme),
                                         std::move(change_reason_code),
                                         std::move(change_commentary));
        },
        "Add a trade identifier (trade_id id_value id_type [scheme|-] reason_code "
        "\"commentary\")");

    trade_identifiers_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string id) {
            process_delete_trade_identifier(std::ref(out), std::ref(session), std::move(id));
        },
        "Delete a trade identifier by id");

    trade_identifiers_menu->Insert(
        "history",
        [&session](std::ostream& out, std::vector<std::string> args) {
            process_get_trade_identifier_history(std::ref(out), std::ref(session), args);
        },
        "Show a trade identifier's version history (--diff for a unified diff, --version <n> to "
        "pick one)",
        {"id [--diff] [--version <n>]"});

    root_menu.Insert(std::move(trade_identifiers_menu));
}

void trade_identifier_commands::process_get_trade_identifiers(std::ostream& out,
                                                              nats_client& session,
                                                              pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get trade identifiers request.";

    auto& state = pagination.state_for("trade_identifiers");

    trading::messaging::get_trade_identifiers_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_trade_identifiers_response>(
        out, session, "trading.v1.trade_identifiers.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("trade_identifiers");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->identifiers.size()
                              << " trade identifiers.";
    out << result->identifiers << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->identifiers.size()
        << " of " << state.total_count << " total)" << std::endl;
}

void trade_identifier_commands::process_add_trade_identifier(std::ostream& out,
                                                             nats_client& session,
                                                             std::string trade_id,
                                                             std::string id_value,
                                                             std::string id_type,
                                                             std::string id_scheme,
                                                             std::string change_reason_code,
                                                             std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add trade identifier request for trade: " << trade_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add a trade identifier." << std::endl;
        return;
    }

    auto trade_uuid = parse_uuid(out, trade_id, "trade ID");
    if (!trade_uuid)
        return;

    const auto id = boost::uuids::random_generator()();
    const auto scheme = (id_scheme == "-") ? std::string{} : std::move(id_scheme);
    auto req = trading::messaging::save_trade_identifier_request::from(
        domain::trade_identifier{.id = id,
                                 .trade_id = *trade_uuid,
                                 .id_value = std::move(id_value),
                                 .id_type = std::move(id_type),
                                 .id_scheme = scheme,
                                 .change_reason_code = std::move(change_reason_code),
                                 .change_commentary = std::move(change_commentary),
                                 .recorded_at = std::chrono::system_clock::now()});

    auto result = do_auth_request<trading::messaging::save_trade_identifier_response>(
        out, session, "trading.v1.trade_identifiers.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added trade identifier.";
        out << "✓ Trade identifier added successfully!" << std::endl;
        out << "Identifier id: " << boost::uuids::to_string(id) << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add trade identifier: " << msg;
        fail(out) << "Failed to add trade identifier: " << msg << std::endl;
    }
}

void trade_identifier_commands::process_delete_trade_identifier(std::ostream& out,
                                                                nats_client& session,
                                                                std::string id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete trade identifier request for: " << id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete a trade identifier." << std::endl;
        return;
    }

    trading::messaging::delete_trade_identifier_request req;
    req.ids = {std::move(id)};

    auto result = do_auth_request<trading::messaging::delete_trade_identifier_response>(
        out, session, "trading.v1.trade_identifiers.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted trade identifier.";
        out << "✓ Trade identifier deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete trade identifier: " << result->message;
        fail(out) << "Failed to delete trade identifier: " << result->message << std::endl;
    }
}

void trade_identifier_commands::process_get_trade_identifier_history(
    std::ostream& out, nats_client& session, const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "diff", .requires_value = false, .default_value = "false"},
                              {.name = "version", .requires_value = true, .default_value = ""}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }
    if (parsed->positionals.size() != 1) {
        fail(out) << "Usage: trade_identifiers history id [--diff] [--version <n>]" << std::endl;
        return;
    }
    auto id = parsed->positionals.front();

    std::optional<int> version;
    if (const auto& v = parsed->flag("version"); !v.empty()) {
        const auto parsed_version = parse_uint32(v);
        if (!parsed_version) {
            fail(out) << "Invalid --version value: " << v << std::endl;
            return;
        }
        version = static_cast<int>(*parsed_version);
    }

    if (parsed->flag_set("diff")) {
        render_history_diff(out, session, "ores.trading.trade_identifier", std::move(id), version);
        return;
    }

    if (version) {
        fail(out) << "--version is only supported together with --diff." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Initiating get trade identifier history for: " << id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get trade identifier history." << std::endl;
        return;
    }

    trading::messaging::get_trade_identifier_history_request req;
    req.id = id;

    auto result = do_auth_request<trading::messaging::get_trade_identifier_history_response>(
        out, session, "trading.v1.trade_identifiers.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get trade identifier history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this trade identifier." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
