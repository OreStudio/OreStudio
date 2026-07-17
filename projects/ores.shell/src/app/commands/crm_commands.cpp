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
#include "ores.shell/app/commands/crm_commands.hpp"
#include "ores.marketdata.api/messaging/crm_protocol.hpp"
#include "ores.marketdata.client/crm_client.hpp"
#include "ores.marketdata.client/presentation/crm_rate_display_service.hpp"
#include "ores.marketdata.client/presentation/crm_rate_table.hpp"
#include "ores.marketdata.client/presentation/crm_rate_table_io.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.refdata.api/messaging/currency_pair_convention_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <cli/cli.h>
#include <ostream>
#include <rfl/json.hpp>
#include <unordered_map>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;
namespace marketdata_client = ores::marketdata::client;
namespace marketdata_msg = ores::marketdata::messaging;
namespace refdata_msg = ores::refdata::messaging;

namespace {

template <typename Response>
std::optional<Response>
do_auth_request(std::ostream& out,
                nats_client& session,
                const std::string& subject,
                const std::string& body,
                std::chrono::milliseconds timeout = std::chrono::seconds(30)) {
    try {
        auto reply = session.authenticated_request(subject, body, timeout);
        auto result = rfl::json::read<Response>(ores::nats::as_string_view(reply.data));
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what() << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

}

void crm_commands::register_commands(cli::Menu& root_menu, nats_client& session) {
    auto menu = std::make_unique<cli::Menu>("crm");

    menu->Insert("rates",
                 [&session](std::ostream& out, std::vector<std::string> args) {
                     process_rates(std::ref(out), std::ref(session), args);
                 },
                 "List CRM rates for a party",
                 {"[<crm-name>] [--party <id-or-full-name>] [--inverted] [--matrix]"});

    root_menu.Insert(std::move(menu));
}

namespace {

/// Resolves --party by UUID or exact full name -- same convention as
/// `provision party <party>` -- so recipes can spell "BARCLAYS PLC"
/// instead of a UUID that differs per environment.
std::optional<std::string>
resolve_party_id(std::ostream& out, nats_client& session, const std::string& party_ref) {
    refdata_msg::get_parties_request req;
    req.limit = 1000;
    auto parties = do_auth_request<refdata_msg::get_parties_response>(
        out, session, std::string(req.nats_subject), rfl::json::write(req));
    if (!parties)
        return std::nullopt;

    std::optional<boost::uuids::uuid> ref_uuid;
    try {
        ref_uuid = boost::lexical_cast<boost::uuids::uuid>(party_ref);
    } catch (const boost::bad_lexical_cast&) {
    }

    for (const auto& party : parties->parties) {
        if ((ref_uuid && party.id == *ref_uuid) || (!ref_uuid && party.full_name == party_ref))
            return boost::uuids::to_string(party.id);
    }
    fail(out) << "Party not found (by UUID or exact full name): " << party_ref << std::endl;
    return std::nullopt;
}

}

void crm_commands::process_rates(std::ostream& out,
                                 nats_client& session,
                                 const std::vector<std::string>& args) {
    auto parsed = parse_args(args,
                             {{.name = "party", .requires_value = true, .default_value = ""},
                              {.name = "inverted", .requires_value = false},
                              {.name = "matrix", .requires_value = false}});
    if (!parsed) {
        fail(out) << parsed.error() << std::endl;
        return;
    }

    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    const std::string crm_name = parsed->positionals.empty() ? "" : parsed->positionals.front();
    const bool inverted = parsed->flag_set("inverted");
    const bool matrix = parsed->flag_set("matrix");
    if (matrix && crm_name.empty()) {
        fail(out) << "--matrix requires a named CRM (mixing CRMs in a grid is ambiguous)."
                  << std::endl;
        return;
    }

    // Every command runs in the context of the logged-in account's own
    // party by default (mirroring ClientManager::currentPartyId() on
    // the Qt side) -- --party only exists to override that for an
    // account (e.g. a tenant admin) that manages more than one party.
    const auto& party_ref = parsed->flag("party");
    std::optional<std::string> party_id;
    if (party_ref.empty()) {
        if (session.auth().default_party_id.empty()) {
            fail(out) << "No default party on this account and no --party given." << std::endl;
            return;
        }
        party_id = session.auth().default_party_id;
    } else {
        party_id = resolve_party_id(out, session, party_ref);
        if (!party_id)
            return;
    }

    BOOST_LOG_SEV(lg(), info) << "Listing CRM rates for party " << *party_id
                              << " (crm: " << (crm_name.empty() ? "all" : crm_name)
                              << ", inverted: " << std::boolalpha << inverted << ")";

    marketdata_msg::get_crm_rates_request rates_req;
    rates_req.party_id = *party_id;
    rates_req.crm_name = crm_name;
    rates_req.inverted = inverted;
    auto rates_result = do_auth_request<marketdata_msg::get_crm_rates_response>(
        out, session, std::string(rates_req.nats_subject), rfl::json::write(rates_req));
    if (!rates_result)
        return;
    if (!rates_result->success) {
        fail(out) << "Failed to fetch CRM rates: " << rates_result->message << std::endl;
        return;
    }

    // A convention may only be captured in one pair-code direction (see
    // crm_rate_display_service); fetching every convention up front and
    // letting it try both directions per cell is simpler than a
    // per-cell round trip, and this list rarely runs into the
    // thousands.
    refdata_msg::get_currency_pair_conventions_request conventions_req;
    conventions_req.limit = 10000;
    auto conventions_result = do_auth_request<refdata_msg::get_currency_pair_conventions_response>(
        out, session, std::string(conventions_req.nats_subject), rfl::json::write(conventions_req));
    if (!conventions_result)
        return;

    std::unordered_map<std::string, ores::refdata::domain::currency_pair_convention> conventions;
    for (const auto& c : conventions_result->conventions)
        conventions.emplace(c.pair_code, c);

    marketdata_client::crm_client::rates_result fetched;
    fetched.success = rates_result->success;
    fetched.rates = rates_result->rates;

    marketdata_client::presentation::crm_rate_display_service display_service(
        [&fetched](const std::string&, const std::string&, bool) { return fetched; },
        [&conventions](const std::string&, const std::string& key)
            -> std::optional<ores::refdata::domain::currency_pair_convention> {
            const auto it = conventions.find(key);
            if (it == conventions.end())
                return std::nullopt;
            return it->second;
        });

    const auto display_result =
        display_service.rates(session.auth().tenant_id, *party_id, crm_name, inverted);
    if (!display_result.success) {
        fail(out) << "Failed to format CRM rates: " << display_result.error << std::endl;
        return;
    }

    if (matrix)
        out << std::endl
            << marketdata_client::presentation::convert_to_matrix_table(display_result.rows)
            << std::endl;
    else
        out << display_result.rows;
    out << display_result.rows.size() << " rate(s) shown." << std::endl;
}

}
