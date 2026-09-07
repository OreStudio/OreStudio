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
#include "ores.shell/app/commands/trading/fx_asian_forward_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/fx_asian_forward_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/fx_asian_forward_instrument_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <cli/cli.h>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
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

std::optional<double> parse_optional_double(std::string_view value,
                                        std::string_view name) {
    if (value.empty())
        return std::nullopt;
    try {
        return std::stod(std::string(value));
    } catch (const std::exception&) {
        throw std::runtime_error(std::string("Invalid numeric value for ") +
                                 std::string(name) + ".");
    }
}

} // namespace

void fx_asian_forward_instrument_commands::register_commands(cli::Menu& root_menu,
                             nats_client& session,
                             pagination_context& pagination) {
    auto fx_asian_forward_instruments_menu =
        std::make_unique<cli::Menu>("fx_asian_forward_instruments");

    fx_asian_forward_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_fx_asian_forward_instruments(std::ref(out), std::ref(session),
                                                     std::ref(pagination));
        },
        "Retrieve FX asian forward instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("fx_asian_forward_instruments",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_fx_asian_forward_instruments(out, session,
                                                                                   pagination);
                                      });

    fx_asian_forward_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_type_code,
                   std::string fx_index,
                   std::string reference_currency,
                   std::string reference_notional,
                   std::string settlement_currency,
                   std::string settlement_notional,
                   std::string payment_date,
                   std::string long_short,
                   std::string currency,
                   std::string fixing_amount,
                   std::string target_amount,
                   std::string strike,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_fx_asian_forward_instrument(std::ref(out),
                                                    std::ref(session),
                                                    std::move(trade_type_code),
                                                    std::move(fx_index),
                                                    std::move(reference_currency),
                                                    std::move(reference_notional),
                                                    std::move(settlement_currency),
                                                    std::move(settlement_notional),
                                                    std::move(payment_date),
                                                    std::move(long_short),
                                                    std::move(currency),
                                                    std::move(fixing_amount),
                                                    std::move(target_amount),
                                                    std::move(strike),
                                                    std::move(description),
                                                    std::move(change_reason_code),
                                                    std::move(change_commentary));
        },
        "Add an FX asian forward instrument (trade_type_code fx_index reference_currency "
        "reference_notional settlement_currency settlement_notional payment_date long_short "
        "currency fixing_amount target_amount strike [description] change_reason_code "
        "\"change_commentary\")",
        {"trade_type_code fx_index reference_currency reference_notional settlement_currency "
         "settlement_notional payment_date long_short currency fixing_amount target_amount strike "
         "description change_reason_code change_commentary"});

    fx_asian_forward_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string instrument_id) {
            process_delete_fx_asian_forward_instrument(std::ref(out), std::ref(session),
                                   std::move(instrument_id));
        },
        "Delete an FX asian forward instrument by instrument id",
        {"instrument_id"});

    fx_asian_forward_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string instrument_id) {
            process_get_fx_asian_forward_instrument_history(std::ref(out), std::ref(session),
                                        std::move(instrument_id));
        },
        "Show an FX asian forward instrument's version history",
        {"instrument_id"});

    root_menu.Insert(std::move(fx_asian_forward_instruments_menu));
}

void fx_asian_forward_instrument_commands::process_get_fx_asian_forward_instruments(
    std::ostream& out, nats_client& session, pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX asian forward instruments request.";

    auto& state = pagination.state_for("fx_asian_forward_instruments");

    trading::messaging::get_fx_asian_forward_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_fx_asian_forward_instruments_response>(
        out, session, "trading.v1.fx_asian_forward_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("fx_asian_forward_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->fx_asian_forward_instruments.size()
                              << " FX asian forward instruments.";
    out << result->fx_asian_forward_instruments << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " ("
        << result->fx_asian_forward_instruments.size() << " of " << state.total_count << " total)"
        << std::endl;
}

void fx_asian_forward_instrument_commands::process_add_fx_asian_forward_instrument(
    std::ostream& out,
    nats_client& session,
    std::string trade_type_code,
    std::string fx_index,
    std::string reference_currency,
    std::string reference_notional,
    std::string settlement_currency,
    std::string settlement_notional,
    std::string payment_date,
    std::string long_short,
    std::string currency,
    std::string fixing_amount,
    std::string target_amount,
    std::string strike,
    std::string description,
    std::string change_reason_code,
    std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add FX asian forward instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an FX asian forward instrument." << std::endl;
        return;
    }

    domain::fx_asian_forward_instrument v;
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

    v.fx_index = std::move(fx_index);
    v.reference_currency = std::move(reference_currency);
    v.settlement_currency = std::move(settlement_currency);
    v.payment_date = std::move(payment_date);
    v.long_short = std::move(long_short);
    v.currency = std::move(currency);
    v.description = std::move(description);

    try {
        v.reference_notional = parse_optional_double(reference_notional, "reference_notional");
        v.settlement_notional = parse_optional_double(settlement_notional, "settlement_notional");
        v.fixing_amount = parse_optional_double(fixing_amount, "fixing_amount");
        v.target_amount = parse_optional_double(target_amount, "target_amount");
        v.strike = parse_optional_double(strike, "strike");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req = trading::messaging::save_fx_asian_forward_instrument_request::from(std::move(v));

    auto result = do_auth_request<trading::messaging::save_fx_asian_forward_instrument_response>(
        out, session, "trading.v1.fx_asian_forward_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added FX asian forward instrument.";
        out << "✓ FX asian forward instrument added successfully!" << std::endl;
        out << "Instrument id: "
            << boost::uuids::to_string(req.data.identity.instrument_id) << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add FX asian forward instrument: " << msg;
        fail(out) << "Failed to add FX asian forward instrument: " << msg << std::endl;
    }
}

void fx_asian_forward_instrument_commands::process_delete_fx_asian_forward_instrument(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete FX asian forward instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an FX asian forward instrument." << std::endl;
        return;
    }

    trading::messaging::delete_fx_asian_forward_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result = do_auth_request<trading::messaging::delete_fx_asian_forward_instrument_response>(
        out, session, "trading.v1.fx_asian_forward_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted FX asian forward instrument.";
        out << "✓ FX asian forward instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete FX asian forward instrument: "
                                  << result->message;
        fail(out) << "Failed to delete FX asian forward instrument: " << result->message << std::endl;
    }
}

void fx_asian_forward_instrument_commands::process_get_fx_asian_forward_instrument_history(
    std::ostream& out, nats_client& session, std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get FX asian forward instrument history for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get FX asian forward instrument history." << std::endl;
        return;
    }

    trading::messaging::get_fx_asian_forward_instrument_history_request req;
    req.instrument_id = std::move(instrument_id);

    auto result =
        do_auth_request<trading::messaging::get_fx_asian_forward_instrument_history_response>(
            out, session, "trading.v1.fx_asian_forward_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get FX asian forward instrument history: "
                                  << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this FX asian forward instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
