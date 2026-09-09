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
#include "ores.shell/app/commands/trading/equity_barrier_option_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/equity_barrier_option_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/equity_barrier_option_instrument_protocol.hpp"
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

void equity_barrier_option_instrument_commands::register_commands(cli::Menu& root_menu,
                                                                  nats_client& session,
                                                                  pagination_context& pagination) {
    auto equity_barrier_option_instruments_menu =
        std::make_unique<cli::Menu>("equity_barrier_option_instruments");

    equity_barrier_option_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_equity_barrier_option_instruments(
                std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve Equity barrier option instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback(
        "equity_barrier_option_instruments", [&session, &pagination](std::ostream& out) {
            process_get_equity_barrier_option_instruments(out, session, pagination);
        });

    equity_barrier_option_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_type_code,
                   std::string underlying_name,
                   std::string currency,
                   double notional,
                   std::string option_type,
                   double strike,
                   std::string expiry_date,
                   std::string exercise_type,
                   std::string long_short,
                   double lower_barrier,
                   std::string lower_barrier_type,
                   std::string upper_barrier,
                   std::string upper_barrier_type,
                   std::string rebate,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_equity_barrier_option_instrument(std::ref(out),
                                                         std::ref(session),
                                                         std::move(trade_type_code),
                                                         std::move(underlying_name),
                                                         std::move(currency),
                                                         notional,
                                                         std::move(option_type),
                                                         strike,
                                                         std::move(expiry_date),
                                                         std::move(exercise_type),
                                                         std::move(long_short),
                                                         lower_barrier,
                                                         std::move(lower_barrier_type),
                                                         std::move(upper_barrier),
                                                         std::move(upper_barrier_type),
                                                         std::move(rebate),
                                                         std::move(description),
                                                         std::move(change_reason_code),
                                                         std::move(change_commentary));
        },
        "Add an Equity barrier option instrument (trade_type_code underlying_name currency "
        "notional "
        "option_type strike expiry_date exercise_type long_short lower_barrier lower_barrier_type "
        "upper_barrier [upper_barrier_type] rebate [description] change_reason_code "
        "\"change_commentary\")",
        {"trade_type_code underlying_name currency notional option_type strike expiry_date "
         "exercise_type long_short lower_barrier lower_barrier_type upper_barrier "
         "upper_barrier_type rebate description change_reason_code change_commentary"});

    equity_barrier_option_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string instrument_id) {
            process_delete_equity_barrier_option_instrument(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Delete an Equity barrier option instrument by instrument id",
        {"instrument_id"});

    equity_barrier_option_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string instrument_id) {
            process_get_equity_barrier_option_instrument_history(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Show an Equity barrier option instrument's version history",
        {"instrument_id"});

    root_menu.Insert(std::move(equity_barrier_option_instruments_menu));
}

void equity_barrier_option_instrument_commands::process_get_equity_barrier_option_instruments(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Equity barrier option instruments request.";

    auto& state = pagination.state_for("equity_barrier_option_instruments");

    trading::messaging::get_equity_barrier_option_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result =
        do_auth_request<trading::messaging::get_equity_barrier_option_instruments_response>(
            out, session, "trading.v1.equity_barrier_option_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("equity_barrier_option_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->equity_barrier_option_instruments.size()
                              << " Equity barrier option instruments.";
    out << result->equity_barrier_option_instruments << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->equity_barrier_option_instruments.size() << " of " << state.total_count
        << " total)" << std::endl;
}

void equity_barrier_option_instrument_commands::process_add_equity_barrier_option_instrument(
    std::ostream& out,
    nats_client& session,
    std::string trade_type_code,
    std::string underlying_name,
    std::string currency,
    double notional,
    std::string option_type,
    double strike,
    std::string expiry_date,
    std::string exercise_type,
    std::string long_short,
    double lower_barrier,
    std::string lower_barrier_type,
    std::string upper_barrier,
    std::string upper_barrier_type,
    std::string rebate,
    std::string description,
    std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add Equity barrier option instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an Equity barrier option instrument."
                  << std::endl;
        return;
    }

    domain::equity_barrier_option_instrument v;
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
    v.notional = notional;
    v.option_type = std::move(option_type);
    v.strike = strike;
    v.expiry_date = std::move(expiry_date);
    v.exercise_type = std::move(exercise_type);
    v.long_short = std::move(long_short);
    v.lower_barrier = lower_barrier;
    v.lower_barrier_type = std::move(lower_barrier_type);
    v.upper_barrier_type = std::move(upper_barrier_type);
    v.description = std::move(description);

    try {
        v.upper_barrier = parse_optional_double(upper_barrier, "upper_barrier");
        v.rebate = parse_optional_double(rebate, "rebate");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req =
        trading::messaging::save_equity_barrier_option_instrument_request::from(std::move(v));

    auto result =
        do_auth_request<trading::messaging::save_equity_barrier_option_instrument_response>(
            out, session, "trading.v1.equity_barrier_option_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added Equity barrier option instrument.";
        out << "✓ Equity barrier option instrument added successfully!" << std::endl;
        out << "Instrument id: " << boost::uuids::to_string(req.data.identity.instrument_id)
            << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add Equity barrier option instrument: " << msg;
        fail(out) << "Failed to add Equity barrier option instrument: " << msg << std::endl;
    }
}

void equity_barrier_option_instrument_commands::process_delete_equity_barrier_option_instrument(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete Equity barrier option instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an Equity barrier option instrument."
                  << std::endl;
        return;
    }

    trading::messaging::delete_equity_barrier_option_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result =
        do_auth_request<trading::messaging::delete_equity_barrier_option_instrument_response>(
            out, session, "trading.v1.equity_barrier_option_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted Equity barrier option instrument.";
        out << "✓ Equity barrier option instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete Equity barrier option instrument: "
                                  << result->message;
        fail(out) << "Failed to delete Equity barrier option instrument: " << result->message
                  << std::endl;
    }
}

void equity_barrier_option_instrument_commands::
    process_get_equity_barrier_option_instrument_history(std::ostream& out,
                                                         nats_client& session,
                                                         std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Equity barrier option instrument history for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get Equity barrier option instrument history."
                  << std::endl;
        return;
    }

    trading::messaging::get_equity_barrier_option_instrument_history_request req;
    req.instrument_id = std::move(instrument_id);

    auto result =
        do_auth_request<trading::messaging::get_equity_barrier_option_instrument_history_response>(
            out, session, "trading.v1.equity_barrier_option_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get Equity barrier option instrument history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this Equity barrier option instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
