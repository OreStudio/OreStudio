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
#include "ores.shell/app/commands/trading/fx_variance_swap_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/fx_variance_swap_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/fx_variance_swap_instrument_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <cli/cli.h>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <functional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace domain = ores::trading::domain;

namespace {

constexpr std::string_view session_party_error =
    "The logged-in account has no default party; set one before adding instruments.";

boost::uuids::uuid party_uuid_for(nats_client& session) {
    const auto& party = session.auth().default_party_id;
    if (party.empty())
        throw std::runtime_error(std::string(session_party_error));
    return boost::lexical_cast<boost::uuids::uuid>(party);
}

} // namespace

void fx_variance_swap_instrument_commands::register_commands(cli::Menu& root_menu,
                             nats_client& session,
                             pagination_context& pagination) {
    auto fx_variance_swap_instruments_menu =
        std::make_unique<cli::Menu>("fx_variance_swap_instruments");

    fx_variance_swap_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_fx_variance_swap_instruments(std::ref(out), std::ref(session),
                                                     std::ref(pagination));
        },
        "Retrieve FX variance swap instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("fx_variance_swap_instruments",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_fx_variance_swap_instruments(out, session,
                                                                                   pagination);
                                      });

    fx_variance_swap_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_type_code,
                   std::string start_date,
                   std::string end_date,
                   std::string currency,
                   std::string underlying_code,
                   std::string long_short,
                   double strike,
                   double notional,
                   std::string moment_type,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_fx_variance_swap_instrument(std::ref(out),
                                                    std::ref(session),
                                                    std::move(trade_type_code),
                                                    std::move(start_date),
                                                    std::move(end_date),
                                                    std::move(currency),
                                                    std::move(underlying_code),
                                                    std::move(long_short),
                                                    strike,
                                                    notional,
                                                    std::move(moment_type),
                                                    std::move(description),
                                                    std::move(change_reason_code),
                                                    std::move(change_commentary));
        },
        "Add an FX variance swap instrument (trade_type_code start_date end_date currency "
        "underlying_code long_short strike notional moment_type [description] change_reason_code "
        "\"change_commentary\")",
        {"trade_type_code start_date end_date currency underlying_code long_short strike notional "
         "moment_type description change_reason_code change_commentary"});

    fx_variance_swap_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string instrument_id) {
            process_delete_fx_variance_swap_instrument(std::ref(out), std::ref(session),
                                   std::move(instrument_id));
        },
        "Delete an FX variance swap instrument by instrument id",
        {"instrument_id"});

    fx_variance_swap_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string instrument_id) {
            process_get_fx_variance_swap_instrument_history(std::ref(out), std::ref(session),
                                        std::move(instrument_id));
        },
        "Show an FX variance swap instrument's version history",
        {"instrument_id"});

    root_menu.Insert(std::move(fx_variance_swap_instruments_menu));
}

void fx_variance_swap_instrument_commands::process_get_fx_variance_swap_instruments(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX variance swap instruments request.";

    auto& state = pagination.state_for("fx_variance_swap_instruments");

    trading::messaging::get_fx_variance_swap_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_fx_variance_swap_instruments_response>(
        out, session, "trading.v1.fx_variance_swap_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("fx_variance_swap_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->fx_variance_swap_instruments.size()
                              << " FX variance swap instruments.";
    out << result->fx_variance_swap_instruments << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->fx_variance_swap_instruments.size() << " of " << state.total_count << " total)"
        << std::endl;
}

void fx_variance_swap_instrument_commands::process_add_fx_variance_swap_instrument(
    std::ostream& out,
    nats_client& session,
    std::string trade_type_code,
    std::string start_date,
    std::string end_date,
    std::string currency,
    std::string underlying_code,
    std::string long_short,
    double strike,
    double notional,
    std::string moment_type,
    std::string description,
    std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add FX variance swap instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an FX variance swap instrument." << std::endl;
        return;
    }

    domain::fx_variance_swap_instrument v;
    v.identity.instrument_id = boost::uuids::random_generator()();
    v.identity.trade_type_code = std::move(trade_type_code);
    try {
        v.identity.party_id = party_uuid_for(session);
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }
    const auto& tenant = session.auth().tenant_id;
    if (auto tid = utility::uuid::tenant_id::from_string(tenant); tid)
        v.identity.tenant_id = *tid;

    v.start_date = std::move(start_date);
    v.end_date = std::move(end_date);
    v.currency = std::move(currency);
    v.underlying_code = std::move(underlying_code);
    v.long_short = std::move(long_short);
    v.strike = strike;
    v.notional = notional;
    v.moment_type = std::move(moment_type);
    v.description = std::move(description);

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req = trading::messaging::save_fx_variance_swap_instrument_request::from(std::move(v));

    auto result = do_auth_request<trading::messaging::save_fx_variance_swap_instrument_response>(
        out, session, "trading.v1.fx_variance_swap_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added FX variance swap instrument.";
        out << "✓ FX variance swap instrument added successfully!" << std::endl;
        out << "Instrument id: "
            << boost::uuids::to_string(req.data.identity.instrument_id) << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add FX variance swap instrument: " << msg;
        fail(out) << "Failed to add FX variance swap instrument: " << msg << std::endl;
    }
}

void fx_variance_swap_instrument_commands::process_delete_fx_variance_swap_instrument(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete FX variance swap instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an FX variance swap instrument." << std::endl;
        return;
    }

    trading::messaging::delete_fx_variance_swap_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result = do_auth_request<trading::messaging::delete_fx_variance_swap_instrument_response>(
        out, session, "trading.v1.fx_variance_swap_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted FX variance swap instrument.";
        out << "✓ FX variance swap instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete FX variance swap instrument: "
                                  << result->message;
        fail(out) << "Failed to delete FX variance swap instrument: " << result->message << std::endl;
    }
}

void fx_variance_swap_instrument_commands::process_get_fx_variance_swap_instrument_history(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX variance swap instrument history for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get FX variance swap instrument history." << std::endl;
        return;
    }

    trading::messaging::get_fx_variance_swap_instrument_history_request req;
    req.instrument_id = std::move(instrument_id);

    auto result =
        do_auth_request<trading::messaging::get_fx_variance_swap_instrument_history_response>(
            out, session, "trading.v1.fx_variance_swap_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get FX variance swap instrument history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this FX variance swap instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
