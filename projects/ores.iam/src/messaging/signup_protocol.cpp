/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam/messaging/signup_protocol.hpp"

#include <expected>
#include <boost/uuid/uuid_io.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

namespace ores::iam::messaging {

using namespace ores::comms::messaging;

std::vector<std::byte> signup_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, username);
    writer::write_string(buffer, email);
    writer::write_string(buffer, password);
    return buffer;
}

std::expected<signup_request, comms::messaging::error_code>
signup_request::deserialize(std::span<const std::byte> data) {
    signup_request request;

    auto username_result = reader::read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    request.username = *username_result;

    auto email_result = reader::read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    request.email = *email_result;

    auto password_result = reader::read_string(data);
    if (!password_result) return std::unexpected(password_result.error());
    request.password = *password_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const signup_request& v) {
    // Don't log password for security
    s << "signup_request{username=" << v.username << ", email=" << v.email << "}";
    return s;
}

std::vector<std::byte> signup_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    writer::write_uuid(buffer, account_id);
    writer::write_string(buffer, username);
    return buffer;
}

std::expected<signup_response, comms::messaging::error_code>
signup_response::deserialize(std::span<const std::byte> data) {
    signup_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    response.account_id = *account_id_result;

    auto username_result = reader::read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    response.username = *username_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const signup_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
