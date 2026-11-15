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
#include "ores.http.api/domain/http_response.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <tuple>

namespace ores::http::domain {

namespace {

struct error_message final {
    std::string error;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<error_message>) {
    return ::ores::utility::codec::field_list{
        ores::utility::codec::field{"error", &error_message::error}};
}
static_assert(ores::utility::codec::names_every_member<error_message>());

struct api_error_body final {
    std::string code;
    std::string message;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<api_error_body>) {
    return ::ores::utility::codec::field_list{
        ores::utility::codec::field{"code", &api_error_body::code},
        ores::utility::codec::field{"message", &api_error_body::message}};
}
static_assert(ores::utility::codec::names_every_member<api_error_body>());

}

http_response http_response::json(const std::string& body, http_status status) {
    http_response r;
    r.status = status;
    r.body = body;
    r.content_type = "application/json";
    return r;
}

http_response http_response::error(http_status status, const std::string& message) {
    return json(ores::utility::codec::json::write(error_message{message}), status);
}

http_response
http_response::api_error(http_status status, const std::string& code, const std::string& message) {
    return json(ores::utility::codec::json::write(api_error_body{code, message}), status);
}

http_response http_response::not_found(const std::string& message) {
    return error(http_status::not_found, message);
}

http_response http_response::range_not_satisfiable(std::uint64_t total) {
    auto resp =
        error(http_status::range_not_satisfiable, "The range starts past the end of the object.");
    /*
     * The caller cannot act on a refusal that does not say how much there was,
     * so the length travels in the header the standard names for it.
     */
    resp.headers["Content-Range"] = "bytes */" + std::to_string(total);
    return resp;
}

http_response http_response::unauthorized(const std::string& message) {
    return error(http_status::unauthorized, message);
}

http_response http_response::forbidden(const std::string& message) {
    return error(http_status::forbidden, message);
}

http_response http_response::bad_request(const std::string& message) {
    return error(http_status::bad_request, message);
}

http_response http_response::internal_error(const std::string& message) {
    return error(http_status::internal_server_error, message);
}

http_response& http_response::set_header(const std::string& name, const std::string& value) {
    headers[name] = value;
    return *this;
}

}
