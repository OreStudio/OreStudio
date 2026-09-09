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
#include "ores.shell/app/commands/trading/rpa_instrument_commands.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.trading.api/domain/rpa_instrument_table_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
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

constexpr std::string_view session_party_error =
    "The logged-in account has no default party; set one before adding instruments.";

boost::uuids::uuid party_uuid_for(nats_client& session) {
    const auto& party = session.auth().default_party_id;
    if (party.empty())
        throw std::runtime_error(std::string(session_party_error));
    return boost::lexical_cast<boost::uuids::uuid>(party);
}

} // namespace

void rpa_instrument_commands::register_commands(cli::Menu& root_menu,
                                                nats_client& session,
                                                pagination_context& pagination) {
    auto rpa_instruments_menu = std::make_unique<cli::Menu>("rpa_instruments");

    rpa_instruments_menu->Insert(
        "get",
        [&session, &pagination](std::ostream& out) {
            process_get_rpa_instruments(std::ref(out), std::ref(session), std::ref(pagination));
        },
        "Retrieve Risk participation agreement instruments from the server (paginated)");

    // Register list callback for navigation
    pagination.register_list_callback("rpa_instruments",
                                      [&session, &pagination](std::ostream& out) {
                                          process_get_rpa_instruments(out, session, pagination);
                                      });

    rpa_instruments_menu->Insert(
        "add",
        [&session](std::ostream& out,
                   std::string start_date,
                   std::string maturity_date,
                   std::string reference_counterparty,
                   double participation_rate,
                   double protection_fee,
                   std::string description,
                   std::string change_reason_code,
                   std::string change_commentary) {
            process_add_rpa_instrument(std::ref(out),
                                       std::ref(session),
                                       std::move(start_date),
                                       std::move(maturity_date),
                                       std::move(reference_counterparty),
                                       participation_rate,
                                       protection_fee,
                                       std::move(description),
                                       std::move(change_reason_code),
                                       std::move(change_commentary));
        },
        "Add an Risk participation agreement instrument (start_date maturity_date "
        "reference_counterparty participation_rate protection_fee [description] change_reason_code "
        "\"change_commentary\")",
        {"start_date maturity_date reference_counterparty participation_rate protection_fee "
         "description change_reason_code change_commentary"});

    rpa_instruments_menu->Insert(
        "delete",
        [&session](std::ostream& out, std::string instrument_id) {
            process_delete_rpa_instrument(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Delete an Risk participation agreement instrument by instrument id",
        {"instrument_id"});

    rpa_instruments_menu->Insert(
        "history",
        [&session](std::ostream& out, std::string instrument_id) {
            process_get_rpa_instrument_history(
                std::ref(out), std::ref(session), std::move(instrument_id));
        },
        "Show an Risk participation agreement instrument's version history",
        {"instrument_id"});

    root_menu.Insert(std::move(rpa_instruments_menu));
}

void rpa_instrument_commands::process_get_rpa_instruments(std::ostream& out,
                                                          nats_client& session,
                                                          pagination_context& pagination) {
    BOOST_LOG_SEV(lg(), debug)
        << "Initiating get Risk participation agreement instruments request.";

    auto& state = pagination.state_for("rpa_instruments");

    trading::messaging::get_rpa_instruments_request req;
    req.offset = state.current_offset;
    req.limit = pagination.page_size();

    auto result = do_auth_request<trading::messaging::get_rpa_instruments_response>(
        out, session, "trading.v1.rpa_instruments.list", req);
    if (!result)
        return;

    state.total_count = result->total_available_count;
    pagination.set_last_entity("rpa_instruments");

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << result->instruments.size()
                              << " Risk participation agreement instruments.";
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

void rpa_instrument_commands::process_add_rpa_instrument(std::ostream& out,
                                                         nats_client& session,
                                                         std::string start_date,
                                                         std::string maturity_date,
                                                         std::string reference_counterparty,
                                                         double participation_rate,
                                                         double protection_fee,
                                                         std::string description,
                                                         std::string change_reason_code,
                                                         std::string change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add Risk participation agreement instrument request.";

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to add an Risk participation agreement instrument."
                  << std::endl;
        return;
    }

    domain::rpa_instrument v;
    v.identity.instrument_id = boost::uuids::random_generator()();
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
    v.maturity_date = std::move(maturity_date);
    v.reference_counterparty = std::move(reference_counterparty);
    v.participation_rate = participation_rate;
    v.protection_fee = protection_fee;
    v.description = std::move(description);

    // The trading tables require modified_by to name a real account
    // username; the logged-in account is the acting principal.
    v.audit.modified_by = session.auth().username;
    v.audit.change_reason_code = std::move(change_reason_code);
    v.audit.change_commentary = std::move(change_commentary);

    auto req = trading::messaging::save_rpa_instrument_request{.data = std::move(v)};

    auto result = do_auth_request<trading::messaging::save_rpa_instrument_response>(
        out, session, "trading.v1.rpa_instruments.save", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added Risk participation agreement instrument.";
        out << "✓ Risk participation agreement instrument added successfully!" << std::endl;
        out << "Instrument id: " << boost::uuids::to_string(req.data.identity.instrument_id)
            << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add Risk participation agreement instrument: "
                                  << msg;
        fail(out) << "Failed to add Risk participation agreement instrument: " << msg << std::endl;
    }
}

void rpa_instrument_commands::process_delete_rpa_instrument(std::ostream& out,
                                                            nats_client& session,
                                                            std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Initiating delete Risk participation agreement instrument request for: "
        << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to delete an Risk participation agreement instrument."
                  << std::endl;
        return;
    }

    trading::messaging::delete_rpa_instrument_request req;
    req.ids = {std::move(instrument_id)};

    auto result = do_auth_request<trading::messaging::delete_rpa_instrument_response>(
        out, session, "trading.v1.rpa_instruments.delete", req);
    if (!result)
        return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info)
            << "Successfully deleted Risk participation agreement instrument.";
        out << "✓ Risk participation agreement instrument deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete Risk participation agreement instrument: "
                                  << result->message;
        fail(out) << "Failed to delete Risk participation agreement instrument: " << result->message
                  << std::endl;
    }
}

void rpa_instrument_commands::process_get_rpa_instrument_history(std::ostream& out,
                                                                 nats_client& session,
                                                                 std::string instrument_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Initiating get Risk participation agreement instrument history for: " << instrument_id;

    if (!session.is_logged_in()) {
        fail(out) << "You must be logged in to get Risk participation agreement instrument history."
                  << std::endl;
        return;
    }

    trading::messaging::get_rpa_instrument_history_request req;
    req.id = std::move(instrument_id);

    auto result = do_auth_request<trading::messaging::get_rpa_instrument_history_response>(
        out, session, "trading.v1.rpa_instruments.history", req);
    if (!result)
        return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn)
            << "Failed to get Risk participation agreement instrument history: " << result->message;
        fail(out) << result->message << std::endl;
        return;
    }

    if (result->history.empty()) {
        out << "No history found for this Risk participation agreement instrument." << std::endl;
        return;
    }

    out << result->history << std::endl;
}

}
