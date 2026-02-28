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
#include "ores.refdata/messaging/party_contact_information_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::refdata::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Party Contact Information helpers
// ============================================================================

void write_party_contact_information(std::vector<std::byte>& buffer,
    const domain::party_contact_information& pci) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(pci.version));
    writer::write_uuid(buffer, pci.id);
    writer::write_uuid(buffer, pci.party_id);
    writer::write_string(buffer, pci.contact_type);
    writer::write_string(buffer, pci.street_line_1);
    writer::write_string(buffer, pci.street_line_2);
    writer::write_string(buffer, pci.city);
    writer::write_string(buffer, pci.state);
    writer::write_string(buffer, pci.country_code);
    writer::write_string(buffer, pci.postal_code);
    writer::write_string(buffer, pci.phone);
    writer::write_string(buffer, pci.email);
    writer::write_string(buffer, pci.web_page);
    writer::write_string(buffer, pci.change_reason_code);
    writer::write_string(buffer, pci.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(pci.recorded_at));
}

std::expected<domain::party_contact_information, error_code>
read_party_contact_information(std::span<const std::byte>& data) {
    domain::party_contact_information pci;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    pci.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    pci.id = *id_result;

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());
    pci.party_id = *party_id_result;

    auto contact_type_result = reader::read_string(data);
    if (!contact_type_result) return std::unexpected(contact_type_result.error());
    pci.contact_type = *contact_type_result;

    auto street_line_1_result = reader::read_string(data);
    if (!street_line_1_result) return std::unexpected(street_line_1_result.error());
    pci.street_line_1 = *street_line_1_result;

    auto street_line_2_result = reader::read_string(data);
    if (!street_line_2_result) return std::unexpected(street_line_2_result.error());
    pci.street_line_2 = *street_line_2_result;

    auto city_result = reader::read_string(data);
    if (!city_result) return std::unexpected(city_result.error());
    pci.city = *city_result;

    auto state_result = reader::read_string(data);
    if (!state_result) return std::unexpected(state_result.error());
    pci.state = *state_result;

    auto country_code_result = reader::read_string(data);
    if (!country_code_result) return std::unexpected(country_code_result.error());
    pci.country_code = *country_code_result;

    auto postal_code_result = reader::read_string(data);
    if (!postal_code_result) return std::unexpected(postal_code_result.error());
    pci.postal_code = *postal_code_result;

    auto phone_result = reader::read_string(data);
    if (!phone_result) return std::unexpected(phone_result.error());
    pci.phone = *phone_result;

    auto email_result = reader::read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    pci.email = *email_result;

    auto web_page_result = reader::read_string(data);
    if (!web_page_result) return std::unexpected(web_page_result.error());
    pci.web_page = *web_page_result;



    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    pci.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    pci.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        pci.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return pci;
}

} // anonymous namespace

// ============================================================================
// Party Contact Information Messages Implementation
// ============================================================================

std::vector<std::byte> get_party_contact_informations_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, party_id);
    return buffer;
}

std::expected<get_party_contact_informations_request, error_code>
get_party_contact_informations_request::deserialize(std::span<const std::byte> data) {
    get_party_contact_informations_request request;

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());
    request.party_id = *party_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_party_contact_informations_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_contact_informations_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(party_contact_informations.size()));
    for (const auto& pci : party_contact_informations) {
        write_party_contact_information(buffer, pci);
    }
    return buffer;
}

std::expected<get_party_contact_informations_response, error_code>
get_party_contact_informations_response::deserialize(std::span<const std::byte> data) {
    get_party_contact_informations_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.party_contact_informations.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_party_contact_information(data);
        if (!result) return std::unexpected(result.error());
        response.party_contact_informations.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_party_contact_informations_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_party_contact_information_request
save_party_contact_information_request::from(domain::party_contact_information party_contact_information) {
    return save_party_contact_information_request{std::vector<domain::party_contact_information>{std::move(party_contact_information)}};
}

save_party_contact_information_request
save_party_contact_information_request::from(std::vector<domain::party_contact_information> party_contact_informations) {
    return save_party_contact_information_request{std::move(party_contact_informations)};
}

std::vector<std::byte> save_party_contact_information_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(party_contact_informations.size()));
    for (const auto& e : party_contact_informations)
        write_party_contact_information(buffer, e);
    return buffer;
}

std::expected<save_party_contact_information_request, error_code>
save_party_contact_information_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());

    save_party_contact_information_request request;
    request.party_contact_informations.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_party_contact_information(data);
        if (!e) return std::unexpected(e.error());
        request.party_contact_informations.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_party_contact_information_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_party_contact_information_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_party_contact_information_response, error_code>
save_party_contact_information_response::deserialize(std::span<const std::byte> data) {
    save_party_contact_information_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_party_contact_information_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_party_contact_information_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_party_contact_information_request, error_code>
delete_party_contact_information_request::deserialize(std::span<const std::byte> data) {
    delete_party_contact_information_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_party_contact_information_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_party_contact_information_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_party_contact_information_response, error_code>
delete_party_contact_information_response::deserialize(std::span<const std::byte> data) {
    delete_party_contact_information_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_party_contact_information_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_contact_information_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_party_contact_information_history_request, error_code>
get_party_contact_information_history_request::deserialize(std::span<const std::byte> data) {
    get_party_contact_information_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_party_contact_information_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_contact_information_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_party_contact_information(buffer, v);
    }
    return buffer;
}

std::expected<get_party_contact_information_history_response, error_code>
get_party_contact_information_history_response::deserialize(std::span<const std::byte> data) {
    get_party_contact_information_history_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.versions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_party_contact_information(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_party_contact_information_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
