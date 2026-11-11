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
#include <cstdlib>
#include <utility>
#include <yyjson.h>

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


namespace {

/// Adds each default the write object leaves out, parsed from its JSON literal.
void fill_defaults(yyjson_mut_doc* doc,
                   yyjson_mut_val* write,
                   std::initializer_list<write_default> defaults) {
    if (!yyjson_mut_is_obj(write))
        return;
    for (const auto& d : defaults) {
        if (yyjson_mut_obj_getn(write, d.name.data(), d.name.size()) != nullptr)
            continue;
        yyjson_doc* literal = yyjson_read(d.json.data(), d.json.size(), 0);
        if (literal == nullptr)
            continue;
        auto* value = yyjson_val_mut_copy(doc, yyjson_doc_get_root(literal));
        yyjson_doc_free(literal);
        auto* key = yyjson_mut_strncpy(doc, d.name.data(), d.name.size());
        yyjson_mut_obj_add(write, key, value);
    }
}

}

std::string with_write_defaults(std::string_view body,
                                std::initializer_list<write_default> defaults) {
    yyjson_doc* parsed = yyjson_read(body.data(), body.size(), 0);
    if (parsed == nullptr)
        return std::string(body);
    yyjson_mut_doc* doc = yyjson_doc_mut_copy(parsed, nullptr);
    yyjson_doc_free(parsed);
    if (doc == nullptr)
        return std::string(body);
    auto* root = yyjson_mut_doc_get_root(doc);
    if (yyjson_mut_is_obj(root)) {
        fill_defaults(doc, yyjson_mut_obj_get(root, "write"), defaults);
        auto* changes = yyjson_mut_obj_get(root, "changes");
        if (yyjson_mut_is_arr(changes)) {
            std::size_t index = 0;
            std::size_t count = 0;
            yyjson_mut_val* change = nullptr;
            yyjson_mut_arr_foreach(changes, index, count, change) {
                fill_defaults(doc, yyjson_mut_obj_get(change, "write"), defaults);
            }
        }
    }
    char* text = yyjson_mut_write(doc, 0, nullptr);
    yyjson_mut_doc_free(doc);
    if (text == nullptr)
        return std::string(body);
    std::string out(text);
    std::free(text);
    return out;
}

}
