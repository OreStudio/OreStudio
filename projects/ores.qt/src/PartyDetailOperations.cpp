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
#include "ores.qt/PartyDetailOperations.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.refdata/domain/party.hpp"
#include "ores.refdata/domain/party_identifier.hpp"
#include "ores.refdata/domain/party_contact_information.hpp"
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.refdata/messaging/party_identifier_protocol.hpp"
#include "ores.refdata/messaging/party_contact_information_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

std::string party_detail_operations::entity_type_name() const {
    return "Party";
}

bool party_detail_operations::has_party_category() const {
    return true;
}

operation_result party_detail_operations::save_entity(
    ClientManager* cm, const entity_data& data) const {

    refdata::domain::party p;
    p.version = data.version;
    p.tenant_id = data.tenant_id;
    p.id = data.id;
    p.full_name = data.full_name;
    p.short_code = data.short_code;
    p.transliterated_name = data.transliterated_name;
    p.party_category = data.party_category.value_or("Operational");
    p.party_type = data.party_type;
    p.parent_party_id = data.parent_id;
    p.business_center_code = data.business_center_code;
    p.status = data.status;
    p.modified_by = data.modified_by;
    p.performed_by = data.performed_by;
    p.change_reason_code = data.change_reason_code;
    p.change_commentary = data.change_commentary;

    refdata::messaging::save_party_request request;
    request.party = p;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::save_party_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::save_party_response::
        deserialize(*payload_result);
    if (!response)
        return {false, "Invalid server response"};

    return {response->success, response->message};
}

operation_result party_detail_operations::delete_entity(
    ClientManager* cm, const boost::uuids::uuid& id) const {

    refdata::messaging::delete_party_request request;
    request.ids = {id};
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::delete_party_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::delete_party_response::
        deserialize(*payload_result);
    if (!response || response->results.empty())
        return {false, "Invalid server response"};

    return {response->results[0].success, response->results[0].message};
}

load_all_entities_result party_detail_operations::load_all_entities(
    ClientManager* cm) const {

    refdata::messaging::get_parties_request request;
    request.offset = 0;
    request.limit = 1000;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::get_parties_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result) return {{}, false};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result) return {{}, false};

    auto response = refdata::messaging::get_parties_response::
        deserialize(*payload_result);
    if (!response) return {{}, false};

    std::vector<parent_entity_entry> entries;
    entries.reserve(response->parties.size());
    for (const auto& p : response->parties) {
        entries.push_back({
            p.id, p.short_code, p.full_name, p.status,
            p.parent_party_id
        });
    }

    return {std::move(entries), true};
}

load_identifiers_result party_detail_operations::load_identifiers(
    ClientManager* cm, const boost::uuids::uuid& /*entity_id*/) const {

    refdata::messaging::get_party_identifiers_request request;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::get_party_identifiers_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result) return {{}, false};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result) return {{}, false};

    auto response = refdata::messaging::get_party_identifiers_response::
        deserialize(*payload_result);
    if (!response) return {{}, false};

    std::vector<identifier_entry> entries;
    entries.reserve(response->party_identifiers.size());
    for (const auto& ident : response->party_identifiers) {
        entries.push_back({
            ident.id, ident.party_id,
            ident.id_scheme, ident.id_value, ident.description,
            ident.modified_by, ident.performed_by
        });
    }

    return {std::move(entries), true};
}

operation_result party_detail_operations::save_identifier(
    ClientManager* cm, const identifier_entry& entry) const {

    refdata::domain::party_identifier ident;
    ident.id = entry.id;
    ident.party_id = entry.owner_id;
    ident.id_scheme = entry.id_scheme;
    ident.id_value = entry.id_value;
    ident.description = entry.description;
    ident.modified_by = entry.modified_by;
    ident.performed_by = entry.performed_by;

    refdata::messaging::save_party_identifier_request request;
    request.party_identifier = ident;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::save_party_identifier_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::save_party_identifier_response::
        deserialize(*payload_result);
    if (!response)
        return {false, "Invalid server response"};

    return {response->success, response->message};
}

operation_result party_detail_operations::delete_identifier(
    ClientManager* cm, const boost::uuids::uuid& id) const {

    refdata::messaging::delete_party_identifier_request request;
    request.ids = {id};
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::delete_party_identifier_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::delete_party_identifier_response::
        deserialize(*payload_result);
    if (!response || response->results.empty())
        return {false, "Invalid server response"};

    return {response->results[0].success, response->results[0].message};
}

load_contacts_result party_detail_operations::load_contacts(
    ClientManager* cm, const boost::uuids::uuid& /*entity_id*/) const {

    refdata::messaging::get_party_contact_informations_request request;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::get_party_contact_informations_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result) return {{}, false};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result) return {{}, false};

    auto response = refdata::messaging::get_party_contact_informations_response::
        deserialize(*payload_result);
    if (!response) return {{}, false};

    std::vector<contact_entry> entries;
    entries.reserve(response->party_contact_informations.size());
    for (const auto& c : response->party_contact_informations) {
        entries.push_back({
            c.id, c.party_id, c.contact_type,
            c.street_line_1, c.street_line_2, c.city, c.state,
            c.country_code, c.postal_code, c.phone, c.email, c.web_page,
            c.modified_by, c.performed_by
        });
    }

    return {std::move(entries), true};
}

operation_result party_detail_operations::save_contact(
    ClientManager* cm, const contact_entry& entry) const {

    refdata::domain::party_contact_information contact;
    contact.id = entry.id;
    contact.party_id = entry.owner_id;
    contact.contact_type = entry.contact_type;
    contact.street_line_1 = entry.street_line_1;
    contact.street_line_2 = entry.street_line_2;
    contact.city = entry.city;
    contact.state = entry.state;
    contact.country_code = entry.country_code;
    contact.postal_code = entry.postal_code;
    contact.phone = entry.phone;
    contact.email = entry.email;
    contact.web_page = entry.web_page;
    contact.modified_by = entry.modified_by;
    contact.performed_by = entry.performed_by;

    refdata::messaging::save_party_contact_information_request request;
    request.party_contact_information = contact;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::save_party_contact_information_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::save_party_contact_information_response::
        deserialize(*payload_result);
    if (!response)
        return {false, "Invalid server response"};

    return {response->success, response->message};
}

operation_result party_detail_operations::delete_contact(
    ClientManager* cm, const boost::uuids::uuid& id) const {

    refdata::messaging::delete_party_contact_information_request request;
    request.ids = {id};
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::delete_party_contact_information_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::delete_party_contact_information_response::
        deserialize(*payload_result);
    if (!response || response->results.empty())
        return {false, "Invalid server response"};

    return {response->results[0].success, response->results[0].message};
}

}
