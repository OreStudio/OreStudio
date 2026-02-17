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
#include "ores.qt/CounterpartyDetailOperations.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.refdata/domain/counterparty_identifier.hpp"
#include "ores.refdata/domain/counterparty_contact_information.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.refdata/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata/messaging/counterparty_contact_information_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

std::string counterparty_detail_operations::entity_type_name() const {
    return "Counterparty";
}

bool counterparty_detail_operations::has_party_category() const {
    return false;
}

operation_result counterparty_detail_operations::save_entity(
    ClientManager* cm, const entity_data& data) const {

    refdata::domain::counterparty cpty;
    cpty.version = data.version;
    cpty.tenant_id = data.tenant_id;
    cpty.id = data.id;
    cpty.full_name = data.full_name;
    cpty.short_code = data.short_code;
    cpty.transliterated_name = data.transliterated_name;
    cpty.party_type = data.party_type;
    cpty.parent_counterparty_id = data.parent_id;
    cpty.business_center_code = data.business_center_code;
    cpty.status = data.status;
    cpty.modified_by = data.modified_by;
    cpty.performed_by = data.performed_by;
    cpty.change_reason_code = data.change_reason_code;
    cpty.change_commentary = data.change_commentary;

    refdata::messaging::save_counterparty_request request;
    request.counterparty = cpty;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::save_counterparty_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::save_counterparty_response::
        deserialize(*payload_result);
    if (!response)
        return {false, "Invalid server response"};

    return {response->success, response->message};
}

operation_result counterparty_detail_operations::delete_entity(
    ClientManager* cm, const boost::uuids::uuid& id) const {

    refdata::messaging::delete_counterparty_request request;
    request.ids = {id};
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::delete_counterparty_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::delete_counterparty_response::
        deserialize(*payload_result);
    if (!response || response->results.empty())
        return {false, "Invalid server response"};

    return {response->results[0].success, response->results[0].message};
}

load_all_entities_result counterparty_detail_operations::load_all_entities(
    ClientManager* cm) const {

    refdata::messaging::get_counterparties_request request;
    request.offset = 0;
    request.limit = 1000;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::get_counterparties_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result) return {{}, false};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result) return {{}, false};

    auto response = refdata::messaging::get_counterparties_response::
        deserialize(*payload_result);
    if (!response) return {{}, false};

    std::vector<parent_entity_entry> entries;
    entries.reserve(response->counterparties.size());
    for (const auto& cpty : response->counterparties) {
        entries.push_back({
            cpty.id, cpty.short_code, cpty.full_name, cpty.status,
            cpty.parent_counterparty_id
        });
    }

    return {std::move(entries), true};
}

load_identifiers_result counterparty_detail_operations::load_identifiers(
    ClientManager* cm, const boost::uuids::uuid& entity_id) const {

    refdata::messaging::get_counterparty_identifiers_request request;
    request.counterparty_id = entity_id;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::get_counterparty_identifiers_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result) return {{}, false};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result) return {{}, false};

    auto response = refdata::messaging::get_counterparty_identifiers_response::
        deserialize(*payload_result);
    if (!response) return {{}, false};

    std::vector<identifier_entry> entries;
    entries.reserve(response->counterparty_identifiers.size());
    for (const auto& ident : response->counterparty_identifiers) {
        entries.push_back({
            ident.id, ident.counterparty_id,
            ident.id_scheme, ident.id_value, ident.description,
            ident.modified_by, ident.performed_by
        });
    }

    return {std::move(entries), true};
}

operation_result counterparty_detail_operations::save_identifier(
    ClientManager* cm, const identifier_entry& entry) const {

    refdata::domain::counterparty_identifier ident;
    ident.id = entry.id;
    ident.counterparty_id = entry.owner_id;
    ident.id_scheme = entry.id_scheme;
    ident.id_value = entry.id_value;
    ident.description = entry.description;
    ident.modified_by = entry.modified_by;
    ident.performed_by = entry.performed_by;

    refdata::messaging::save_counterparty_identifier_request request;
    request.counterparty_identifier = ident;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::save_counterparty_identifier_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::save_counterparty_identifier_response::
        deserialize(*payload_result);
    if (!response)
        return {false, "Invalid server response"};

    return {response->success, response->message};
}

operation_result counterparty_detail_operations::delete_identifier(
    ClientManager* cm, const boost::uuids::uuid& id) const {

    refdata::messaging::delete_counterparty_identifier_request request;
    request.ids = {id};
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::delete_counterparty_identifier_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::delete_counterparty_identifier_response::
        deserialize(*payload_result);
    if (!response || response->results.empty())
        return {false, "Invalid server response"};

    return {response->results[0].success, response->results[0].message};
}

load_contacts_result counterparty_detail_operations::load_contacts(
    ClientManager* cm, const boost::uuids::uuid& entity_id) const {

    refdata::messaging::get_counterparty_contact_informations_request request;
    request.counterparty_id = entity_id;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::get_counterparty_contact_informations_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result) return {{}, false};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result) return {{}, false};

    auto response = refdata::messaging::get_counterparty_contact_informations_response::
        deserialize(*payload_result);
    if (!response) return {{}, false};

    std::vector<contact_entry> entries;
    entries.reserve(response->counterparty_contact_informations.size());
    for (const auto& c : response->counterparty_contact_informations) {
        entries.push_back({
            c.id, c.counterparty_id, c.contact_type,
            c.street_line_1, c.street_line_2, c.city, c.state,
            c.country_code, c.postal_code, c.phone, c.email, c.web_page,
            c.modified_by, c.performed_by
        });
    }

    return {std::move(entries), true};
}

operation_result counterparty_detail_operations::save_contact(
    ClientManager* cm, const contact_entry& entry) const {

    refdata::domain::counterparty_contact_information contact;
    contact.id = entry.id;
    contact.counterparty_id = entry.owner_id;
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

    refdata::messaging::save_counterparty_contact_information_request request;
    request.counterparty_contact_information = contact;
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::save_counterparty_contact_information_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::save_counterparty_contact_information_response::
        deserialize(*payload_result);
    if (!response)
        return {false, "Invalid server response"};

    return {response->success, response->message};
}

operation_result counterparty_detail_operations::delete_contact(
    ClientManager* cm, const boost::uuids::uuid& id) const {

    refdata::messaging::delete_counterparty_contact_information_request request;
    request.ids = {id};
    auto payload = request.serialize();

    comms::messaging::frame request_frame(
        comms::messaging::message_type::delete_counterparty_contact_information_request,
        0, std::move(payload));

    auto response_result = cm->sendRequest(std::move(request_frame));
    if (!response_result)
        return {false, "Failed to communicate with server"};

    auto payload_result = response_result->decompressed_payload();
    if (!payload_result)
        return {false, "Failed to decompress response"};

    auto response = refdata::messaging::delete_counterparty_contact_information_response::
        deserialize(*payload_result);
    if (!response || response->results.empty())
        return {false, "Invalid server response"};

    return {response->results[0].success, response->results[0].message};
}

}
