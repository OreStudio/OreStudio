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
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/workflow/timeline_sources.hpp"
#include "ores.refdata.api/messaging/business_unit_protocol.hpp"
#include "ores.refdata.api/messaging/party_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <exception>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace refdata_messaging = ores::refdata::messaging;

using ores::http::domain::http_request;
using ores::nats::service::nats_client;

/// The tables a party's stream draws from, named as the models name them.
constexpr std::string_view party_entity = "ores.refdata.party";
constexpr std::string_view identifier_entity = "ores.refdata.party_identifier";
constexpr std::string_view party_contact_entity = "ores.refdata.party_contact_information";
constexpr std::string_view business_unit_entity = "ores.refdata.business_unit";
constexpr std::string_view country_entity = "ores.refdata.party_country";
constexpr std::string_view currency_entity = "ores.refdata.party_currency";
constexpr std::string_view counterparty_entity = "ores.refdata.party_counterparty";

using identifiers_page = refdata_messaging::list_by_party_id_party_identifiers_response;
using contacts_page = refdata_messaging::list_by_party_id_party_contact_informations_response;
using business_units_page = refdata_messaging::list_by_party_id_business_units_response;

/// One policy's version of the party, or nothing when the tenant holds none.
std::optional<ores::refdata::domain::party>
party_of(const http_request& req, nats_client& session, const boost::uuids::uuid& party_id) {
    refdata_messaging::list_parties_request msg;
    msg.limit = 1;
    msg.filter =
        refdata_messaging::parties_filter{std::vector<boost::uuids::uuid>{party_id}, std::nullopt};
    const auto reply = forwarding::forward(req, session, msg);
    if (reply.result.outcome == ores::utility::domain::outcome::missing)
        return std::nullopt;
    rr::require_ok(reply.result);
    if (reply.parties.empty())
        return std::nullopt;
    return reply.parties.front();
}

/// The versions of one party that belong to the party asked for.
std::vector<ores::http::net::history::version>
versions_of_row(const std::vector<ores::http::net::history::version>& versions,
                const std::string& id) {
    std::vector<ores::http::net::history::version> own;
    for (const auto& version : versions) {
        for (const auto& field : version.fields) {
            if (field.name != "ID")
                continue;
            if (field.value == id)
                own.push_back(version);
            break;
        }
    }
    return own;
}

/**
 * The versions of every row of one kind this party holds today.
 *
 * A part is listed by the party and its history is read by the row's own id: the
 * key a model declares for these rows is shared by the rows of other parties,
 * because every party holds a legal contact and two may hold the same identifier
 * value. The id is one row.
 */
std::vector<story_event> part_events(const http_request& req,
                                     nats_client& session,
                                     std::vector<timeline_gap>& gaps,
                                     std::string_view entity_type,
                                     const std::string& name,
                                     const std::vector<std::string>& ids) {
    std::vector<story_event> events;
    for (const auto& id : ids) {
        const auto versions = versions_or_gap(gaps,
                                              entity_type,
                                              "The versions of one " + name +
                                                  " could not be read, so they are not shown.",
                                              req,
                                              session,
                                              entity_type,
                                              id);
        for (auto& event : story_record_events(entity_type, id, versions))
            events.push_back(std::move(event));
    }
    return events;
}

/// The opening and closing of one kind of link a party keeps. A link has two
/// keys, so it is read by the party and answers every link the party ever held.
std::vector<story_event> link_events(const http_request& req,
                                     nats_client& session,
                                     std::vector<timeline_gap>& gaps,
                                     std::string_view entity_type,
                                     const std::string& what,
                                     const std::string& party_id) {
    const auto versions = versions_or_gap(
        gaps,
        entity_type,
        "The " + what + " could not be read, so opening or closing one is not shown.",
        req,
        session,
        entity_type,
        party_id);
    return story_link_events(entity_type, versions);
}

}

timeline_answer
party_timeline(const http_request& req, nats_client& session, const std::string& party_id) {
    timeline_answer answer;
    answer.subject = "party";
    answer.id = party_id;

    boost::uuids::uuid id;
    try {
        id = boost::uuids::string_generator()(party_id);
    } catch (const std::exception&) {
        rr::invalid("A party is named by its identifier.");
    }

    const auto party = party_of(req, session, id);
    if (!party)
        return answer;

    const auto party_versions = versions_or_gap(
        answer.gaps,
        party_entity,
        "The party's own versions are not shown: the service did not answer the read.",
        req,
        session,
        party_entity,
        party->short_code);

    const auto identifiers =
        or_gap(answer.gaps,
               identifier_entity,
               "The identifiers of this party could not be read, so their versions are not shown.",
               [&]() {
                   refdata_messaging::list_by_party_id_party_identifiers_request msg;
                   msg.party_id = id;
                   std::vector<std::string> rows;
                   for (const auto& row : forwarding::forward_all(
                            req, session, msg, &identifiers_page::party_identifiers))
                       rows.push_back(rr::as_text(row.id));
                   return rows;
               });

    const auto contacts =
        or_gap(answer.gaps,
               party_contact_entity,
               "The contacts of this party could not be read, so their versions are not shown.",
               [&]() {
                   refdata_messaging::list_by_party_id_party_contact_informations_request msg;
                   msg.party_id = id;
                   std::vector<std::string> rows;
                   for (const auto& row : forwarding::forward_all(
                            req, session, msg, &contacts_page::party_contact_informations))
                       rows.push_back(rr::as_text(row.id));
                   return rows;
               });

    const auto business_units = or_gap(
        answer.gaps,
        business_unit_entity,
        "The business units of this party could not be read, so their versions are not shown.",
        [&]() {
            refdata_messaging::list_by_party_id_business_units_request msg;
            msg.party_id = id;
            std::vector<std::string> rows;
            for (const auto& row :
                 forwarding::forward_all(req, session, msg, &business_units_page::business_units))
                rows.push_back(rr::as_text(row.id));
            return rows;
        });

    const auto countries =
        link_events(req, session, answer.gaps, country_entity, "countries it works in", party_id);
    const auto currencies =
        link_events(req, session, answer.gaps, currency_entity, "currencies it works in", party_id);
    const auto named_counterparties = link_events(req,
                                                  session,
                                                  answer.gaps,
                                                  counterparty_entity,
                                                  "counterparties it may trade with",
                                                  party_id);
    const auto counterparties =
        with_names(named_counterparties,
                   "Counterparty",
                   counterparty_names(
                       req, session, ids_in(named_counterparties, "Counterparty"), answer.gaps));

    // A part that was removed is closed, and no read lists a closed row, so its
    // versions cannot be found from the party. The stream says so for every kind
    // rather than showing a story that looks complete.
    answer.gaps.push_back({std::string(identifier_entity) + "_removed",
                           "No read lists the identifiers that were removed from this party."});
    answer.gaps.push_back({std::string(party_contact_entity) + "_removed",
                           "No read lists the contacts that were removed from this party."});
    answer.gaps.push_back({std::string(business_unit_entity) + "_removed",
                           "No read lists the business units that were removed from this party."});

    for (auto& event :
         story_record_events(party_entity, party_id, versions_of_row(party_versions, party_id)))
        answer.events.push_back(std::move(event));
    for (auto& event :
         part_events(req, session, answer.gaps, identifier_entity, "identifier", identifiers))
        answer.events.push_back(std::move(event));
    for (auto& event :
         part_events(req, session, answer.gaps, party_contact_entity, "contact", contacts))
        answer.events.push_back(std::move(event));
    for (auto& event : part_events(
             req, session, answer.gaps, business_unit_entity, "business unit", business_units))
        answer.events.push_back(std::move(event));
    for (auto& event : countries)
        answer.events.push_back(std::move(event));
    for (auto& event : currencies)
        answer.events.push_back(std::move(event));
    for (auto& event : counterparties)
        answer.events.push_back(std::move(event));
    order_story(answer.events);
    return answer;
}

}
