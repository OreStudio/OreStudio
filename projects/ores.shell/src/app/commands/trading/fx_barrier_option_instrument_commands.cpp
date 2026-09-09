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
#include "ores.shell/app/commands/trading/fx_barrier_option_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/fx_barrier_option_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/fx_barrier_option_instrument_protocol.hpp"
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
    if (value.empty())
        return std::nullopt;
    try {
        return std::stod(std::string(value));
    } catch (const std::exception&) {
        throw std::runtime_error(std::string("Invalid numeric value for ") + std::string(name) +
                                 ".");
    }
}

} // namespace

void fx_barrier_option_instrument_commands::register_commands(cli::Menu& root_menu,
                                                              nats_client& session,
                                                              pagination_context& pagination) {
    auto fx_barrier_option_instruments_menu =
        std::make_unique<cli::Menu>("fx_barrier_option_instruments");

    fx_barrier_option_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_fx_barrier_option_instruments(
                std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve FX barrier option instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback(
        "fx_barrier_option_instruments", [&session, &pagination](std::ostream& out) {
            process_get_fx_barrier_option_instruments(out, session, pagination);
        });

    fx_barrier_option_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_type_code,
                   std::string bought_currency,
                   double bought_amount,
                   std::string sold_currency,
                   double sold_amount,
                   std::string option_type,
                   std::string expiry_date,
                   std::string settlement,
                   std::string barrier_type,
                   double lower_barrier,
                   std::string upper_barrier,
                   std::string underlying_code,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_fx_barrier_option_instrument(std::ref(out),
                                                     std::ref(session),
                                                     std::move(trade_type_code),
                                                     std::move(bought_currency),
                                                     bought_amount,
                                                     std::move(sold_currency),
                                                     sold_amount,
                                                     std::move(option_type),
                                                     std::move(expiry_date),
                                                     std::move(settlement),
                                                     std::move(barrier_type),
                                                     lower_barrier,
                                                     std::move(upper_barrier),
                                                     std::move(underlying_code),
                                                     std::move(description),
                                                     std::move(change_reason_code),
                                                     std::move(change_commentary));
        },
        "Add an FX barrier option instrument (trade_type_code bought_currency bought_amount "
        "sold_currency sold_amount option_type expiry_date [settlement] barrier_type lower_barrier "
        "upper_barrier underlying_code [description] change_reason_code \"change_commentary\")",
        {"trade_type_code bought_currency bought_amount sold_currency sold_amount option_type "
         "expiry_date settlement barrier_type lower_barrier upper_barrier underlying_code "
         "description change_reason_code change_commentary"});

    fx_barrier_option_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string instrument_id) {
            process_delete_fx_barrier_option_instrument(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Delete an FX barrier option instrument by instrument id",
        {"instrument_id"});

    fx_barrier_option_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string instrument_id) {
            process_get_fx_barrier_option_instrument_history(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Show an FX barrier option instrument's version history",
        {"instrument_id"});

    root_menu.Insert(std::move(fx_barrier_option_instruments_menu));
}

void fx_barrier_option_instrument_commands::process_get_fx_barrier_option_instruments(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX barrier option instruments request.";

    auto& state = pagination.state_for("fx_barrier_option_instruments");

    trading::messaging::get_fx_barrier_option_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_fx_barrier_option_instruments_response>(
        out, session, "trading.v1.fx_barrier_option_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("fx_barrier_option_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->fx_barrier_option_instruments.size()
                              << " FX barrier option instruments.";
    out << result->fx_barrier_option_instruments << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->fx_barrier_option_instruments.size() << " of " << state.total_count << " total)"
        << std::endl;
}

void fx_barrier_option_instrument_commands::process_add_fx_barrier_option_instrument(
    std::ostream& out,
    nats_client& session,
    std::string trade_type_code,
    std::string bought_currency,
    double bought_amount,
    std::string sold_currency,
    double sold_amount,
    std::string option_type,
    std::string expiry_date,
    std::string settlement,
    std::string barrier_type,
    double lower_barrier,
    std::string upper_barrier,
    std::string underlying_code,
    std::string description,
    std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add FX barrier option instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an FX barrier option instrument." << std::endl;
        return;
    }

    domain::fx_barrier_option_instrument v;
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

    v.bought_currency = std::move(bought_currency);
    v.bought_amount = bought_amount;
    v.sold_currency = std::move(sold_currency);
    v.sold_amount = sold_amount;
    v.option_type = std::move(option_type);
    v.expiry_date = std::move(expiry_date);
    v.settlement = std::move(settlement);
    v.barrier_type = std::move(barrier_type);
    v.lower_barrier = lower_barrier;
    v.underlying_code = std::move(underlying_code);
    v.description = std::move(description);

    try {
        v.upper_barrier = parse_optional_double(upper_barrier, "upper_barrier");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req = trading::messaging::save_fx_barrier_option_instrument_request::from(std::move(v));

    auto result = do_auth_request<trading::messaging::save_fx_barrier_option_instrument_response>(
        out, session, "trading.v1.fx_barrier_option_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added FX barrier option instrument.";
        out << "✓ FX barrier option instrument added successfully!" << std::endl;
        out << "Instrument id: " << boost::uuids::to_string(req.data.identity.instrument_id)
            << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add FX barrier option instrument: " << msg;
        fail(out) << "Failed to add FX barrier option instrument: " << msg << std::endl;
    }
}

void fx_barrier_option_instrument_commands::process_delete_fx_barrier_option_instrument(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete FX barrier option instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an FX barrier option instrument."
                  << std::endl;
        return;
    }

    trading::messaging::delete_fx_barrier_option_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result = do_auth_request<trading::messaging::delete_fx_barrier_option_instrument_response>(
        out, session, "trading.v1.fx_barrier_option_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted FX barrier option instrument.";
        out << "✓ FX barrier option instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete FX barrier option instrument: "
                                  << result->message;
        fail(out) << "Failed to delete FX barrier option instrument: " << result->message
                  << std::endl;
    }
}

void fx_barrier_option_instrument_commands::process_get_fx_barrier_option_instrument_history(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX barrier option instrument history for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get FX barrier option instrument history."
                  << std::endl;
        return;
    }

    trading::messaging::get_fx_barrier_option_instrument_history_request req;
    req.instrument_id = std::move(instrument_id);

    auto result =
        do_auth_request<trading::messaging::get_fx_barrier_option_instrument_history_response>(
            out, session, "trading.v1.fx_barrier_option_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get FX barrier option instrument history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this FX barrier option instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
