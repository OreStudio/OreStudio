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
#include "ores.shell/app/commands/trading/bond_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/bond_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/instrument_protocol.hpp"
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

void bond_instrument_commands::register_commands(cli::Menu& root_menu,
                                                 nats_client& session,
                                                 pagination_context& pagination) {
    auto bond_instruments_menu = std::make_unique<cli::Menu>("bond_instruments");

    bond_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_bond_instruments(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve Bond instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("bond_instruments",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_bond_instruments(out, session, pagination);
                                      });

    bond_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string trade_type_code,
                   std::string security_id,
                   std::string issuer,
                   std::string currency,
                   double face_value,
                   double coupon_rate,
                   std::string coupon_frequency_code,
                   std::string day_count_code,
                   std::string issue_date,
                   std::string maturity_date,
                   std::string call_date,
                   std::string future_expiry_date,
                   std::string trs_return_type,
                   std::string trs_funding_leg_code,
                   std::string option_type,
                   std::string option_expiry_date,
                   std::string option_strike,
                   std::string ascot_option_type,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_bond_instrument(std::ref(out),
                                        std::ref(session),
                                        std::move(trade_type_code),
                                        std::move(security_id),
                                        std::move(issuer),
                                        std::move(currency),
                                        face_value,
                                        coupon_rate,
                                        std::move(coupon_frequency_code),
                                        std::move(day_count_code),
                                        std::move(issue_date),
                                        std::move(maturity_date),
                                        std::move(call_date),
                                        std::move(future_expiry_date),
                                        std::move(trs_return_type),
                                        std::move(trs_funding_leg_code),
                                        std::move(option_type),
                                        std::move(option_expiry_date),
                                        std::move(option_strike),
                                        std::move(ascot_option_type),
                                        std::move(description),
                                        std::move(change_reason_code),
                                        std::move(change_commentary));
        },
        "Add an Bond instrument (trade_type_code [security_id] issuer currency face_value "
        "coupon_rate coupon_frequency_code [day_count_code] issue_date [maturity_date] [call_date] "
        "[future_expiry_date] [trs_return_type] [trs_funding_leg_code] [option_type] "
        "[option_expiry_date] [option_strike] [ascot_option_type] description change_reason_code "
        "\"change_commentary\")",
        {"trade_type_code security_id issuer currency face_value coupon_rate coupon_frequency_code "
         "day_count_code issue_date maturity_date call_date future_expiry_date trs_return_type "
         "trs_funding_leg_code option_type option_expiry_date option_strike ascot_option_type "
         "description change_reason_code change_commentary"});

    bond_instruments_menu->Insert("delete",
                                  [&session](std::ostream& out, std::string instrument_id) {
                                      process_delete_bond_instrument(std::ref(out),
                                                                     std::ref(session),
                                                                     std::move(instrument_id));
                                  },
                                  "Delete an Bond instrument by instrument id",
                                  {"instrument_id"});

    bond_instruments_menu->Insert("history",
                                  [&session](std::ostream& out, std::string instrument_id) {
                                      process_get_bond_instrument_history(std::ref(out),
                                                                          std::ref(session),
                                                                          std::move(instrument_id));
                                  },
                                  "Show an Bond instrument's version history",
                                  {"instrument_id"});

    root_menu.Insert(std::move(bond_instruments_menu));
}

void bond_instrument_commands::process_get_bond_instruments(std::ostream& out,
                                                            nats_client& session,
                                                            pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Bond instruments request.";

    auto& state = pagination.state_for("bond_instruments");

    trading::messaging::get_bond_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_bond_instruments_response>(
        out, session, "trading.v1.bond_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("bond_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->instruments.size()
                              << " Bond instruments.";
    out << result->instruments << std::endl;

    // Display pagination info
    const auto page = (state.current_offset / pagination.page_size()) + 1;
    const auto total_pages =
        state.total_count > 0 ?
            ((state.total_count + pagination.page_size() - 1) / pagination.page_size()) :
            1;
    out << "\nPage " << page << " of " << total_pages << " (" << result->instruments.size()
        << " of " << state.total_count << " total)" << std::endl;
}

void bond_instrument_commands::process_add_bond_instrument(std::ostream& out,
                                                           nats_client& session,
                                                           std::string trade_type_code,
                                                           std::string security_id,
                                                           std::string issuer,
                                                           std::string currency,
                                                           double face_value,
                                                           double coupon_rate,
                                                           std::string coupon_frequency_code,
                                                           std::string day_count_code,
                                                           std::string issue_date,
                                                           std::string maturity_date,
                                                           std::string call_date,
                                                           std::string future_expiry_date,
                                                           std::string trs_return_type,
                                                           std::string trs_funding_leg_code,
                                                           std::string option_type,
                                                           std::string option_expiry_date,
                                                           std::string option_strike,
                                                           std::string ascot_option_type,
                                                           std::string description,
                                                           std::string change_reason_code,
                                                           std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add Bond instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an Bond instrument." << std::endl;
        return;
    }

    domain::bond_instrument v;
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

    v.terms.security_id = (security_id == "-") ? "" : std::move(security_id);
    v.terms.issuer = std::move(issuer);
    v.terms.currency = std::move(currency);
    v.terms.face_value = face_value;
    v.terms.coupon_rate = coupon_rate;
    v.terms.coupon_frequency_code = std::move(coupon_frequency_code);
    v.terms.day_count_code = std::move(day_count_code);
    v.terms.issue_date = std::move(issue_date);
    v.terms.maturity_date = std::move(maturity_date);
    v.features.call_date = (call_date == "-") ? "" : std::move(call_date);
    v.features.future_expiry_date =
        (future_expiry_date == "-") ? "" : std::move(future_expiry_date);
    v.features.trs_return_type = (trs_return_type == "-") ? "" : std::move(trs_return_type);
    v.features.trs_funding_leg_code =
        (trs_funding_leg_code == "-") ? "" : std::move(trs_funding_leg_code);
    v.option.option_type = (option_type == "-") ? "" : std::move(option_type);
    v.option.option_expiry_date = (option_expiry_date == "-") ? "" : std::move(option_expiry_date);
    v.option.ascot_option_type = (ascot_option_type == "-") ? "" : std::move(ascot_option_type);
    v.description = std::move(description);

    try {
        v.option.option_strike = parse_optional_double(option_strike, "option_strike");
    } catch (const std::exception& e) {
        fail(out) << e.what() << std::endl;
        return;
    }

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req = trading::messaging::save_bond_instrument_request{.data = std::move(v)};

    auto result = do_auth_request<trading::messaging::save_bond_instrument_response>(
        out, session, "trading.v1.bond_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added Bond instrument.";
        out << "✓ Bond instrument added successfully!" << std::endl;
        out << "Instrument id: " << boost::uuids::to_string(req.data.identity.instrument_id)
            << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add Bond instrument: " << msg;
        fail(out) << "Failed to add Bond instrument: " << msg << std::endl;
    }
}

void bond_instrument_commands::process_delete_bond_instrument(std::ostream& out,
                                                              nats_client& session,
                                                              std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete Bond instrument request for: "
                               << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an Bond instrument." << std::endl;
        return;
    }

    trading::messaging::delete_bond_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result = do_auth_request<trading::messaging::delete_bond_instrument_response>(
        out, session, "trading.v1.bond_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted Bond instrument.";
        out << "✓ Bond instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete Bond instrument: " << result->message;
        fail(out) << "Failed to delete Bond instrument: " << result->message << std::endl;
    }
}

void bond_instrument_commands::process_get_bond_instrument_history(std::ostream& out,
                                                                   nats_client& session,
                                                                   std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get Bond instrument history for: " << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get Bond instrument history." << std::endl;
        return;
    }

    trading::messaging::get_bond_instrument_history_request req;
    req.id = std::move(instrument_id);

    auto result = do_auth_request<trading::messaging::get_bond_instrument_history_response>(
        out, session, "trading.v1.bond_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get Bond instrument history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this Bond instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
