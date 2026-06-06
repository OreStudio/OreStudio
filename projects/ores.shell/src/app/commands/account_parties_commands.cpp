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
#include "ores.shell/app/commands/account_parties_commands.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.iam.api/domain/account_party_table_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <ostream>
#include <rfl/json.hpp>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

template <typename Response>
std::optional<Response> do_auth_request(std::ostream& out,
                                        nats_client& session,
                                        const std::string& subject,
                                        const std::string& body) {
    try {
        auto reply = session.authenticated_request(subject, body);
        auto result = rfl::json::read<Response>(
                ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what()
                      << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

std::optional<boost::uuids::uuid> parse_uuid(std::ostream& out,
                                             const std::string& value,
                                             std::string_view what) {
    try {
        return boost::lexical_cast<boost::uuids::uuid>(value);
    } catch (const boost::bad_lexical_cast&) {
        fail(out) << "Invalid " << what << " format. Expected UUID: " << value
                  << std::endl;
        return std::nullopt;
    }
}

}

void account_parties_commands::register_commands(cli::Menu& root_menu,
                                                 nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("account-parties");

    menu->Insert(
        "list",
        [&session](std::ostream& out) {
            process_list(std::ref(out), std::ref(session));
        },
        "List account-to-party associations");

    menu->Insert(
        "add",
        [&session](std::ostream& out, std::string account_id, std::string party_id) {
            process_add(std::ref(out), std::ref(session), account_id, party_id);
        },
        "Associate an account with a party",
        {"account_id", "party_id"});

    root_menu.Insert(std::move(menu));
}

void account_parties_commands::process_list(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Listing account-party associations.";

    iam::messaging::get_account_parties_request req;
    auto result = do_auth_request<iam::messaging::get_account_parties_response>(
        out, session, std::string(req.nats_subject), rfl::json::write(req));
    if (!result)
        return;

    out << result->account_parties << std::endl;
    out << result->account_parties.size() << " of " << result->total_available_count
        << " associations shown." << std::endl;
}

void account_parties_commands::process_add(std::ostream& out,
                                           nats_client& session,
                                           const std::string& account_id,
                                           const std::string& party_id) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    auto account_uuid = parse_uuid(out, account_id, "account ID");
    if (!account_uuid)
        return;
    auto party_uuid = parse_uuid(out, party_id, "party ID");
    if (!party_uuid)
        return;

    BOOST_LOG_SEV(lg(), debug) << "Associating account " << account_id << " with party "
                               << party_id;

    // Look the party up: validates it exists and sources the tenant
    // id, as the tenant provisioning wizard does.
    refdata::messaging::get_parties_request parties_req;
    parties_req.limit = 1000;
    auto parties = do_auth_request<refdata::messaging::get_parties_response>(
        out, session, std::string(parties_req.nats_subject),
        rfl::json::write(parties_req));
    if (!parties)
        return;

    const auto party = std::find_if(parties->parties.begin(), parties->parties.end(),
                                    [&](const auto& p) { return p.id == *party_uuid; });
    if (party == parties->parties.end()) {
        fail(out) << "Party not found: " << party_id << std::endl;
        return;
    }

    iam::domain::account_party association;
    association.tenant_id = party->tenant_id.to_string();
    association.account_id = *account_uuid;
    association.party_id = *party_uuid;
    association.modified_by = session.auth().username;
    association.performed_by = session.auth().username;
    association.change_reason_code =
        std::string(dq::domain::change_reason_constants::codes::new_record);
    association.change_commentary = "Associated via shell";

    iam::messaging::save_account_party_request req;
    req.account_parties.push_back(std::move(association));

    auto result = do_auth_request<iam::messaging::save_account_party_response>(
        out, session, std::string(req.nats_subject), rfl::json::write(req));
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Failed to associate account with party: " << result->message
                  << std::endl;
        return;
    }
    out << "✓ Account " << account_id << " associated with party '" << party->full_name
        << "'." << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Account " << account_id << " associated with party "
                              << party_id;
}

}
