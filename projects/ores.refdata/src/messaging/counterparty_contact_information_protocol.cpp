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
#include "ores.refdata/messaging/counterparty_contact_information_protocol.hpp"

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
// Counterparty Contact Information helpers
// ============================================================================

void write_counterparty_contact_information(std::vector<std::byte>& buffer,
    const domain::counterparty_contact_information& cci) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(cci.version));
    writer::write_uuid(buffer, cci.id);
    writer::write_uuid(buffer, cci.counterparty_id);
    writer::write_string(buffer, cci.contact_type);
    writer::write_string(buffer, cci.street_line_1);
    writer::write_string(buffer, cci.street_line_2);
    writer::write_string(buffer, cci.city);
    writer::write_string(buffer, cci.state);
    writer::write_string(buffer, cci.country_code);
    writer::write_string(buffer, cci.postal_code);
    writer::write_string(buffer, cci.phone);
    writer::write_string(buffer, cci.email);
    writer::write_string(buffer, cci.web_page);
    writer::write_string(buffer, cci.change_reason_code);
    writer::write_string(buffer, cci.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(cci.recorded_at));
}

std::expected<domain::counterparty_contact_information, error_code>
read_counterparty_contact_information(std::span<const std::byte>& data) {
    domain::counterparty_contact_information cci;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    cci.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    cci.id = *id_result;

    auto counterparty_id_result = reader::read_uuid(data);
    if (!counterparty_id_result) return std::unexpected(counterparty_id_result.error());
    cci.counterparty_id = *counterparty_id_result;

    auto contact_type_result = reader::read_string(data);
    if (!contact_type_result) return std::unexpected(contact_type_result.error());
    cci.contact_type = *contact_type_result;

    auto street_line_1_result = reader::read_string(data);
    if (!street_line_1_result) return std::unexpected(street_line_1_result.error());
    cci.street_line_1 = *street_line_1_result;

    auto street_line_2_result = reader::read_string(data);
    if (!street_line_2_result) return std::unexpected(street_line_2_result.error());
    cci.street_line_2 = *street_line_2_result;

    auto city_result = reader::read_string(data);
    if (!city_result) return std::unexpected(city_result.error());
    cci.city = *city_result;

    auto state_result = reader::read_string(data);
    if (!state_result) return std::unexpected(state_result.error());
    cci.state = *state_result;

    auto country_code_result = reader::read_string(data);
    if (!country_code_result) return std::unexpected(country_code_result.error());
    cci.country_code = *country_code_result;

    auto postal_code_result = reader::read_string(data);
    if (!postal_code_result) return std::unexpected(postal_code_result.error());
    cci.postal_code = *postal_code_result;

    auto phone_result = reader::read_string(data);
    if (!phone_result) return std::unexpected(phone_result.error());
    cci.phone = *phone_result;

    auto email_result = reader::read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    cci.email = *email_result;

    auto web_page_result = reader::read_string(data);
    if (!web_page_result) return std::unexpected(web_page_result.error());
    cci.web_page = *web_page_result;



    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    cci.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    cci.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        cci.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return cci;
}

} // anonymous namespace

// ============================================================================
// Counterparty Contact Information Messages Implementation
// ============================================================================

std::vector<std::byte> get_counterparty_contact_informations_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, counterparty_id);
    return buffer;
}

std::expected<get_counterparty_contact_informations_request, error_code>
get_counterparty_contact_informations_request::deserialize(std::span<const std::byte> data) {
    get_counterparty_contact_informations_request request;

    auto cpty_id_result = reader::read_uuid(data);
    if (!cpty_id_result) return std::unexpected(cpty_id_result.error());
    request.counterparty_id = *cpty_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_counterparty_contact_informations_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_counterparty_contact_informations_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(counterparty_contact_informations.size()));
    for (const auto& cci : counterparty_contact_informations) {
        write_counterparty_contact_information(buffer, cci);
    }
    return buffer;
}

std::expected<get_counterparty_contact_informations_response, error_code>
get_counterparty_contact_informations_response::deserialize(std::span<const std::byte> data) {
    get_counterparty_contact_informations_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.counterparty_contact_informations.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_counterparty_contact_information(data);
        if (!result) return std::unexpected(result.error());
        response.counterparty_contact_informations.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_counterparty_contact_informations_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_counterparty_contact_information_request
save_counterparty_contact_information_request::from(domain::counterparty_contact_information counterparty_contact_information) {
    return save_counterparty_contact_information_request{std::vector<domain::counterparty_contact_information>{std::move(counterparty_contact_information)}};
}

save_counterparty_contact_information_request
save_counterparty_contact_information_request::from(std::vector<domain::counterparty_contact_information> counterparty_contact_informations) {
    return save_counterparty_contact_information_request{std::move(counterparty_contact_informations)};
}

std::vector<std::byte> save_counterparty_contact_information_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(counterparty_contact_informations.size()));
    for (const auto& e : counterparty_contact_informations)
        write_counterparty_contact_information(buffer, e);
    return buffer;
}

std::expected<save_counterparty_contact_information_request, error_code>
save_counterparty_contact_information_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());

    save_counterparty_contact_information_request request;
    request.counterparty_contact_informations.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_counterparty_contact_information(data);
        if (!e) return std::unexpected(e.error());
        request.counterparty_contact_informations.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_counterparty_contact_information_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_counterparty_contact_information_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_counterparty_contact_information_response, error_code>
save_counterparty_contact_information_response::deserialize(std::span<const std::byte> data) {
    save_counterparty_contact_information_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_counterparty_contact_information_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_counterparty_contact_information_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_counterparty_contact_information_request, error_code>
delete_counterparty_contact_information_request::deserialize(std::span<const std::byte> data) {
    delete_counterparty_contact_information_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_counterparty_contact_information_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_counterparty_contact_information_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_counterparty_contact_information_response, error_code>
delete_counterparty_contact_information_response::deserialize(std::span<const std::byte> data) {
    delete_counterparty_contact_information_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_counterparty_contact_information_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_counterparty_contact_information_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_counterparty_contact_information_history_request, error_code>
get_counterparty_contact_information_history_request::deserialize(std::span<const std::byte> data) {
    get_counterparty_contact_information_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_counterparty_contact_information_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_counterparty_contact_information_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_counterparty_contact_information(buffer, v);
    }
    return buffer;
}

std::expected<get_counterparty_contact_information_history_response, error_code>
get_counterparty_contact_information_history_response::deserialize(std::span<const std::byte> data) {
    get_counterparty_contact_information_history_response response;

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
        auto result = read_counterparty_contact_information(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_counterparty_contact_information_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
