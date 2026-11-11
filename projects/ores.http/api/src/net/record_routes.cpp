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
#include <utility>

namespace ores::http::net::record_routes {

using domain::http_response;
using domain::http_status;

void refuse(http_status status, std::string code, std::string message) {
    throw refusal{status, std::move(code), std::move(message)};
}

void invalid(std::string message) {
    refuse(http_status::bad_request, "invalid-request", std::move(message));
}

http_response answer(const refusal& r) {
    return http_response::api_error(r.status, r.code, r.message);
}

http_response no_content() {
    http_response response;
    response.status = http_status::no_content;
    return response;
}

std::string trimmed(const std::string& raw) {
    constexpr std::string_view blanks = " \t\r\n";
    const auto first = raw.find_first_not_of(blanks);
    if (first == std::string::npos)
        return {};
    return raw.substr(first, raw.find_last_not_of(blanks) - first + 1);
}

std::string code(const std::string& raw, const std::string& sentence) {
    auto value = trimmed(raw);
    if (value.empty() || value.size() > 100)
        invalid(sentence);
    return value;
}

bool has_reason(const std::string& reason) {
    const auto value = trimmed(reason);
    return !value.empty() && value.size() <= 200;
}

void require_ok(const ores::utility::domain::result& result) {
    using ores::utility::domain::outcome;
    switch (result.outcome) {
        case outcome::ok:
            return;
        case outcome::conflict:
            refuse(http_status::conflict,
                   "conflict",
                   result.message.empty() ?
                       "This record changed since it was read, or it already exists. "
                       "Reload and try again." :
                       result.message);
        case outcome::invalid:
            invalid(result.message);
        case outcome::denied:
            refuse(http_status::forbidden, "forbidden", result.message);
        case outcome::missing:
            refuse(http_status::not_found, "not-found", result.message);
        default:
            refuse(http_status::bad_gateway, "upstream-unavailable", result.message);
    }
}

}
