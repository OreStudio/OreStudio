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
#include "ores.shell/app/commands/trading/equity_accumulator_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/equity_accumulator_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/equity_accumulator_instrument_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <optional>
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

std::optional<double> parse_optional_double(std::string_view value, std::string_view name) {
    if (value.empty() || value == "-")
        return std::nullopt;
    try {
        return std::stod(std::string(value));
    } catch (const std::exception&) {
        throw std::runtime_error(std::string("Invalid numeric value for ") + std::string(name) +
                                 ".");
    }
}

} // namespace

void equity_accumulator_instrument_commands::register_commands(cli::Menu& root_menu,
                                                               nats_client& session,
                                                               pagination_context& pagination) {
    auto equity_accumulator_instruments_menu =
        std::make_unique<cli::Menu>("equity_accumulator_instruments");

    equity_accumulator_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_equity_accumulator_instruments(
                std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve Equity accumulator instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback(
        "equity_accumulator_instruments", [&session, &pagination](std::ostream& out) {
            process_get_equity_accumulator_instruments(out, session, pagination);
        });

    equity_accumulator_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_type_code,
                   std::string underlying_name,
                   std::string currency,
                   double strike,
                   double fixing_amount,
                   std::string start_date,
                   std::string expiry_date,
                   std::string fixing_frequency,
                   std::string long_short,
                   std::string knock_out_level,
                   std::string target_amount,
                   std::string target_type,
                   std::string payoff_type,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_equity_accumulator_instrument(std::ref(out),
                                                      std::ref(session),
                                                      std::move(trade_type_code),
                                                      std::move(underlying_name),
                                                      std::move(currency),
                                                      strike,
                                                      fixing_amount,
                                                      std::move(start_date),
                                                      std::move(expiry_date),
                                                      std::move(fixing_frequency),
                                                      std::move(long_short),
                                                      std::move(knock_out_level),
                                                      std::move(target_amount),
                                                      std::move(target_type),
                                                      std::move(payoff_type),
                                                      std::move(description),
                                                      std::move(change_reason_code),
                                                      std::move(change_commentary));
        },
        "Add an Equity accumulator instrument (trade_type_code underlying_name currency strike "
        "fixing_amount start_date expiry_date fixing_frequency long_short knock_out_level "
        "target_amount [target_type] payoff_type [description] change_reason_code "
        "\"change_commentary\")",
        {"trade_type_code underlying_name currency strike fixing_amount start_date expiry_date "
         "fixing_frequency long_short knock_out_level target_amount target_type payoff_type "
         "description change_reason_code change_commentary"});

    equity_accumulator_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string instrument_id) {
            process_delete_equity_accumulator_instrument(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Delete an Equity accumulator instrument by instrument id",
        {"instrument_id"});

    equity_accumulator_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string instrument_id) {
            process_get_equity_accumulator_instrument_history(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Show an Equity accumulator instrument's version history",
        {"instrument_id"});

    root_menu.Insert(std::move(equity_accumulator_instruments_menu));
}

void equity_accumulator_instrument_commands::process_get_equity_accumulator_instruments(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Equity accumulator instruments request.";

    auto& state = pagination.state_for("equity_accumulator_instruments");

    trading::messaging::get_equity_accumulator_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_equity_accumulator_instruments_response>(
        out, session, "trading.v1.equity_accumulator_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("equity_accumulator_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->equity_accumulator_instruments.size()
                              << " Equity accumulator instruments.";
    out << result->equity_accumulator_instruments << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->equity_accumulator_instruments.size() << " of " << state.total_count << " total)"
        << std::endl;
}

void equity_accumulator_instrument_commands::process_add_equity_accumulator_instrument(
    std::ostream& out,
    nats_client& session,
    std::string trade_type_code,
    std::string underlying_name,
    std::string currency,
    double strike,
    double fixing_amount,
    std::string start_date,
    std::string expiry_date,
    std::string fixing_frequency,
    std::string long_short,
    std::string knock_out_level,
    std::string target_amount,
    std::string target_type,
    std::string payoff_type,
    std::string description,
    std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add Equity accumulator instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an Equity accumulator instrument." << std::endl;
        return;
    }

    domain::equity_accumulator_instrument v;
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

    v.underlying_name = std::move(underlying_name);
    v.currency = std::move(currency);
    v.strike = strike;
    v.fixing_amount = fixing_amount;
    v.start_date = std::move(start_date);
    v.expiry_date = std::move(expiry_date);
    v.fixing_frequency = std::move(fixing_frequency);
    v.long_short = std::move(long_short);
    v.target_type = (target_type == "-") ? "" : std::move(target_type);
    v.payoff_type = std::move(payoff_type);
    v.description = std::move(description);

    try {
        v.knock_out_level = parse_optional_double(knock_out_level, "knock_out_level");
        v.target_amount = parse_optional_double(target_amount, "target_amount");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req = trading::messaging::save_equity_accumulator_instrument_request::from(std::move(v));

    auto result = do_auth_request<trading::messaging::save_equity_accumulator_instrument_response>(
        out, session, "trading.v1.equity_accumulator_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added Equity accumulator instrument.";
        out << "✓ Equity accumulator instrument added successfully!" << std::endl;
        out << "Instrument id: " << boost::uuids::to_string(req.data.identity.instrument_id)
            << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add Equity accumulator instrument: " << msg;
        fail(out) << "Failed to add Equity accumulator instrument: " << msg << std::endl;
    }
}

void equity_accumulator_instrument_commands::process_delete_equity_accumulator_instrument(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete Equity accumulator instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an Equity accumulator instrument."
                  << std::endl;
        return;
    }

    trading::messaging::delete_equity_accumulator_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result =
        do_auth_request<trading::messaging::delete_equity_accumulator_instrument_response>(
            out, session, "trading.v1.equity_accumulator_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted Equity accumulator instrument.";
        out << "✓ Equity accumulator instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete Equity accumulator instrument: "
                                  << result->message;
        fail(out) << "Failed to delete Equity accumulator instrument: " << result->message
                  << std::endl;
    }
}

void equity_accumulator_instrument_commands::process_get_equity_accumulator_instrument_history(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Equity accumulator instrument history for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get Equity accumulator instrument history."
                  << std::endl;
        return;
    }

    trading::messaging::get_equity_accumulator_instrument_history_request req;
    req.instrument_id = std::move(instrument_id);

    auto result =
        do_auth_request<trading::messaging::get_equity_accumulator_instrument_history_response>(
            out, session, "trading.v1.equity_accumulator_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get Equity accumulator instrument history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this Equity accumulator instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
