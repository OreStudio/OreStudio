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
#include "ores.http/routes/refdata/counterparty_screen_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/domain/counterparty_business_centre_codec.hpp"
#include "ores.refdata.api/domain/counterparty_codec.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information_codec.hpp"
#include "ores.refdata.api/domain/counterparty_identifier_codec.hpp"
#include "ores.refdata.api/domain/csa_codec.hpp"
#include "ores.refdata.api/domain/csa_eligible_currency_codec.hpp"
#include "ores.refdata.api/domain/netting_agreement_codec.hpp"
#include "ores.refdata.api/domain/netting_set_codec.hpp"
#include "ores.refdata.api/domain/netting_set_identifier_codec.hpp"
#include "ores.refdata.api/domain/party_counterparty_codec.hpp"
#include "ores.refdata.api/messaging/counterparty_business_centre_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_contact_information_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <set>
#include <type_traits>
#include <utility>

namespace ores::http::routes::refdata {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::refdata::messaging;
namespace refdata_domain = ores::refdata::domain;
namespace rr = ores::http::net::record_routes;
namespace utility_domain = ores::utility::domain;

namespace {

/// The most counterparties one children read may name.
constexpr std::size_t counterparty_screen_most_ids = 200;

/// The bodies the screen sends, beside the wire_fields each is read with.
namespace counterparty_screen {

/// The composite write as the screen sends it: each list of children may be left out.
struct counterparty_composite_body final {
    rr::stated_intent intent;
    refdata_domain::counterparty counterparty;
    std::optional<std::vector<refdata_domain::counterparty_identifier>> identifiers;
    std::optional<std::vector<refdata_domain::counterparty_contact_information>> contacts;
    std::optional<std::vector<refdata_domain::netting_agreement>> agreements;
    std::optional<std::vector<refdata_domain::netting_set>> netting_sets;
    std::optional<std::vector<refdata_domain::netting_set_identifier>> netting_set_identifiers;
    std::optional<std::vector<refdata_domain::csa>> csas;
    std::optional<std::vector<refdata_domain::csa_eligible_currency>> eligible_currencies;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_composite_body>) {
    using ores::utility::codec::field;
    using body = counterparty_composite_body;
    return ::ores::utility::codec::field_list{
        field{"intent", &body::intent},
        field{"counterparty", &body::counterparty},
        field{"identifiers", &body::identifiers},
        field{"contacts", &body::contacts},
        field{"agreements", &body::agreements},
        field{"netting_sets", &body::netting_sets},
        field{"netting_set_identifiers", &body::netting_set_identifiers},
        field{"csas", &body::csas},
        field{"eligible_currencies", &body::eligible_currencies}};
}

/// Which party a counterparty is to be visible to.
struct counterparty_visibility_body final {
    std::string partyId;
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_visibility_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{
        field{"partyId", &counterparty_visibility_body::partyId},
        field{"intent", &counterparty_visibility_body::intent}};
}

/// The business centres a counterparty is to hold.
struct counterparty_centres_body final {
    std::vector<std::string> codes;
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_centres_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"codes", &counterparty_centres_body::codes},
                                              field{"intent", &counterparty_centres_body::intent}};
}

}

boost::uuids::uuid uuid_of(const std::string& text) {
    return boost::uuids::string_generator()(text);
}

/// The counterparty the address names, or a refusal when it names no identifier.
std::string counterparty_id_of(const http_request& req) {
    auto id = req.get_path_param("id");
    if (!rr::is_uuid(id))
        rr::invalid("A counterparty is named by its identifier.");
    return id;
}

/// The rows of a children read that belong to one counterparty.
template <typename Row>
std::string rows_of(const std::vector<Row>& rows, const std::string& id) {
    std::string out = "[";
    for (const auto& row : rows) {
        if (boost::uuids::to_string(row.counterparty_id) != id)
            continue;
        out += (out.size() == 1 ? "" : ",") + json::write(row);
    }
    return out + "]";
}

std::string result_json(const utility_domain::result& result) {
    return "{\"result\":" + json::write(result) + "}";
}

}

counterparty_screen_routes::centre_changes
counterparty_screen_routes::changes_of(const std::vector<std::string>& held,
                                       const std::vector<std::string>& wanted) {
    const std::set<std::string> holding(held.begin(), held.end());
    const std::set<std::string> wanting(wanted.begin(), wanted.end());
    centre_changes out;
    std::set<std::string> seen;
    for (const auto& code : wanted)
        if (!holding.contains(code) && seen.insert(code).second)
            out.links.push_back(code);
    seen.clear();
    for (const auto& code : held)
        if (!wanting.contains(code) && seen.insert(code).second)
            out.unlinks.push_back(code);
    return out;
}

std::optional<std::vector<std::string>>
counterparty_screen_routes::read_ids(const std::string& text) {
    std::vector<std::string> ids;
    std::size_t start = 0;
    while (start <= text.size()) {
        const auto end = std::min(text.find(',', start), text.size());
        auto id = text.substr(start, end - start);
        if (!id.empty()) {
            if (!rr::is_uuid(id))
                return std::nullopt;
            // The service's rows carry the lower case form, so the named id is matched in it.
            auto canonical = boost::uuids::to_string(uuid_of(id));
            if (std::ranges::find(ids, canonical) == ids.end())
                ids.push_back(std::move(canonical));
        }
        start = end + 1;
    }
    if (ids.empty() || ids.size() > counterparty_screen_most_ids)
        return std::nullopt;
    return ids;
}

std::string counterparty_screen_routes::children_json(
    const std::vector<std::string>& ids,
    const std::vector<refdata_domain::counterparty_identifier>& identifiers,
    const std::vector<refdata_domain::counterparty_contact_information>& contacts,
    const std::vector<refdata_domain::counterparty_business_centre>& centres) {
    std::string out = "{\"children\":[";
    for (std::size_t i = 0; i < ids.size(); ++i) {
        const auto& id = ids[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"counterpartyId\":" + json::write(id);
        out += ",\"identifiers\":" + rows_of(identifiers, id);
        out += ",\"contacts\":" + rows_of(contacts, id);
        out += ",\"centres\":" + rows_of(centres, id) + "}";
    }
    return out + "]}";
}

void counterparty_screen_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using ores::http::forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"counterparties"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/counterparties/{id}")
            .summary("Read one counterparty with the identifiers it is known by")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::list_counterparties_request>([&] {
                    const auto id = counterparty_id_of(req);
                    messaging::list_counterparties_request found_msg;
                    found_msg.filter = messaging::counterparties_filter{};
                    found_msg.filter->id_one_of = std::vector<boost::uuids::uuid>{uuid_of(id)};
                    const auto found = ores::http::forwarding::forward_all(
                        req,
                        session,
                        found_msg,
                        &messaging::list_counterparties_response::counterparties);
                    if (found.empty())
                        rr::refuse(
                            http_status::not_found, "not-found", "No counterparty has that id.");
                    messaging::list_counterparty_identifiers_request identifiers_msg;
                    identifiers_msg.filter = messaging::counterparty_identifiers_filter{};
                    identifiers_msg.filter->counterparty_id_one_of =
                        std::vector<boost::uuids::uuid>{uuid_of(id)};
                    const auto identifiers = ores::http::forwarding::forward_all(
                        req,
                        session,
                        identifiers_msg,
                        &messaging::list_counterparty_identifiers_response::
                            counterparty_identifiers);
                    return http_response::json("{\"counterparty\":" + json::write(found.front()) +
                                               ",\"identifiers\":" + json::write(identifiers) +
                                               "}");
                });
            }));

    add(router->get("/api/counterparties/children")
            .summary(
                "Read the identifiers, contacts and business centres of a page of counterparties")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                const auto ids = read_ids(req.get_query_param("ids"));
                if (!ids)
                    co_return rr::refused(
                        http_status::bad_request,
                        "invalid-request",
                        "Name 1 to 200 counterparty identifiers, comma separated.");
                co_return guarded<messaging::list_counterparty_identifiers_request>([&] {
                    std::vector<boost::uuids::uuid> named;
                    for (const auto& id : *ids)
                        named.push_back(uuid_of(id));
                    messaging::list_counterparty_identifiers_request identifiers_msg;
                    identifiers_msg.filter = messaging::counterparty_identifiers_filter{};
                    identifiers_msg.filter->counterparty_id_one_of = named;
                    messaging::list_counterparty_contact_informations_request contacts_msg;
                    contacts_msg.filter = messaging::counterparty_contact_informations_filter{};
                    contacts_msg.filter->counterparty_id_one_of = named;
                    messaging::list_counterparty_business_centres_request centres_msg;
                    centres_msg.filter = messaging::counterparty_business_centres_filter{};
                    centres_msg.filter->counterparty_id_one_of = named;
                    return http_response::json(children_json(
                        *ids,
                        ores::http::forwarding::forward_all(
                            req,
                            session,
                            identifiers_msg,
                            &messaging::list_counterparty_identifiers_response::
                                counterparty_identifiers),
                        ores::http::forwarding::forward_all(
                            req,
                            session,
                            contacts_msg,
                            &messaging::list_counterparty_contact_informations_response::
                                counterparty_contact_informations),
                        ores::http::forwarding::forward_all(
                            req,
                            session,
                            centres_msg,
                            &messaging::list_counterparty_business_centres_response::
                                counterparty_business_centres)));
                });
            }));

    add(router->put("/api/counterparties/composite")
            .summary("Write a counterparty with its children in one change")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                const auto body =
                    rr::read_body<counterparty_screen::counterparty_composite_body>(req.body);
                const auto intent = body ? rr::intent_of(body->intent) : std::nullopt;
                if (!body || !intent || body->counterparty.id.is_nil())
                    co_return rr::refused(
                        http_status::bad_request,
                        "invalid-request",
                        "A composite write needs the counterparty with its id, and a reason.");
                co_return guarded<messaging::put_counterparty_composite_request>([&] {
                    messaging::put_counterparty_composite_request msg;
                    msg.intent = *intent;
                    msg.counterparty = body->counterparty;
                    msg.identifiers = body->identifiers.value_or(decltype(msg.identifiers){});
                    msg.contacts = body->contacts.value_or(decltype(msg.contacts){});
                    msg.agreements = body->agreements.value_or(decltype(msg.agreements){});
                    msg.netting_sets = body->netting_sets.value_or(decltype(msg.netting_sets){});
                    msg.netting_set_identifiers = body->netting_set_identifiers.value_or(
                        decltype(msg.netting_set_identifiers){});
                    msg.csas = body->csas.value_or(decltype(msg.csas){});
                    msg.eligible_currencies =
                        body->eligible_currencies.value_or(decltype(msg.eligible_currencies){});
                    return http_response::json(
                        json::write(ores::http::forwarding::forward(req, session, msg)));
                });
            }));

    add(router->get("/api/counterparties/{id}/visibility")
            .summary("Read the parties a counterparty is directly visible to")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::list_by_counterparty_id_party_counterparties_request>(
                    [&] {
                        messaging::list_by_counterparty_id_party_counterparties_request msg;
                        msg.counterparty_id = uuid_of(counterparty_id_of(req));
                        msg.scope = utility_domain::scope::direct;
                        const auto rows = ores::http::forwarding::forward_all(
                            req,
                            session,
                            msg,
                            &messaging::list_by_counterparty_id_party_counterparties_response::
                                party_counterparties);
                        return http_response::json("{\"partyCounterparties\":" + json::write(rows) +
                                                   "}");
                    });
            }));

    add(router->put("/api/counterparties/{id}/visibility")
            .summary("Make a counterparty visible to a party")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::put_party_counterparty_request>([&] {
                    const auto id = counterparty_id_of(req);
                    const auto body =
                        rr::read_body<counterparty_screen::counterparty_visibility_body>(req.body);
                    const auto intent = body ? rr::intent_of(body->intent) : std::nullopt;
                    if (!body || !intent || !rr::is_uuid(body->partyId))
                        rr::invalid(
                            "A visibility write names the party by its identifier, and a reason.");
                    messaging::put_party_counterparty_request msg;
                    msg.change.write.party_id = uuid_of(body->partyId);
                    msg.change.write.counterparty_id = uuid_of(id);
                    msg.change.precondition.kind = utility_domain::precondition_kind::any;
                    msg.intent = *intent;
                    return http_response::json(
                        result_json(ores::http::forwarding::forward(req, session, msg).result));
                });
            }));

    add(router->put("/api/counterparties/{id}/business-centres")
            .summary("Set the business centres a counterparty holds")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::put_many_counterparty_business_centres_request>([&] {
                    const auto id = counterparty_id_of(req);
                    const auto body =
                        rr::read_body<counterparty_screen::counterparty_centres_body>(req.body);
                    const auto intent = body ? rr::intent_of(body->intent) : std::nullopt;
                    const std::string sentence =
                        "A business centre write names up to 1000 codes of 1 to 100 characters, "
                        "and a reason.";
                    if (!body || !intent || body->codes.size() > 1000)
                        rr::invalid(sentence);
                    std::vector<std::string> wanted;
                    for (const auto& raw : body->codes) {
                        auto code = rr::trimmed(raw);
                        if (code.empty() || rr::characters_of(code) > 100)
                            rr::invalid(sentence);
                        wanted.push_back(std::move(code));
                    }

                    messaging::list_counterparty_business_centres_request held_msg;
                    held_msg.filter = messaging::counterparty_business_centres_filter{};
                    held_msg.filter->counterparty_id = uuid_of(id);
                    std::vector<std::string> held;
                    for (const auto& row : ores::http::forwarding::forward_all(
                             req,
                             session,
                             held_msg,
                             &messaging::list_counterparty_business_centres_response::
                                 counterparty_business_centres))
                        held.push_back(row.business_centre_code);

                    /*
                     * Each batch lands whole or not at all. The links land
                     * first, so when the closing batch fails the counterparty
                     * holds both sets, never neither.
                     */
                    const auto changes = changes_of(held, wanted);
                    if (!changes.links.empty()) {
                        messaging::put_many_counterparty_business_centres_request msg;
                        for (const auto& code : changes.links) {
                            messaging::counterparty_business_centre_change change;
                            change.write.counterparty_id = uuid_of(id);
                            change.write.business_centre_code = code;
                            change.precondition.kind =
                                utility_domain::precondition_kind::must_not_exist;
                            msg.changes.push_back(std::move(change));
                        }
                        msg.intent = *intent;
                        const auto result =
                            ores::http::forwarding::forward(req, session, msg).result;
                        if (result.outcome != utility_domain::outcome::ok)
                            return http_response::json(result_json(result));
                    }
                    if (!changes.unlinks.empty()) {
                        messaging::delete_many_counterparty_business_centres_request msg;
                        for (const auto& code : changes.unlinks) {
                            messaging::counterparty_business_centre_removal removal;
                            removal.key.counterparty_id = uuid_of(id);
                            removal.key.business_centre_code = code;
                            removal.precondition.kind = utility_domain::precondition_kind::any;
                            msg.removals.push_back(std::move(removal));
                        }
                        msg.intent = *intent;
                        const auto result =
                            ores::http::forwarding::forward(req, session, msg).result;
                        if (result.outcome != utility_domain::outcome::ok)
                            return http_response::json(result_json(result));
                    }
                    return http_response::json(result_json(utility_domain::result{}));
                });
            }));
}

}
