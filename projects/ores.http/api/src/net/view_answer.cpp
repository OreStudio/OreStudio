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
#include "ores.http.api/net/view_answer.hpp"
#include <cstdint>
#include <format>
#include <utility>

namespace ores::http::net {

namespace {

/// Whether an If-None-Match header names the tag: one of its list, weak or strong, or any.
bool names_tag(std::string_view header, std::string_view tag) {
    while (!header.empty()) {
        const auto comma = header.find(',');
        auto item = header.substr(0, comma);
        header = comma == std::string_view::npos ? std::string_view{} : header.substr(comma + 1);
        while (!item.empty() && (item.front() == ' ' || item.front() == '\t'))
            item.remove_prefix(1);
        while (!item.empty() && (item.back() == ' ' || item.back() == '\t'))
            item.remove_suffix(1);
        if (item.starts_with("W/"))
            item.remove_prefix(2);
        if (item == "*" || item == tag)
            return true;
    }
    return false;
}

}

std::string entity_tag(std::string_view body) {
    // FNV-1a over the bytes: the tag names a version of the answer, it does not guard it.
    std::uint64_t hash = 14695981039346656037ULL;
    for (const unsigned char c : body) {
        hash ^= c;
        hash *= 1099511628211ULL;
    }
    return std::format("\"{:016x}-{:x}\"", hash, body.size());
}

domain::http_response
view_answer(const domain::http_request& req, std::string body, const view_cache_policy& policy) {
    if (!policy.stale_after) {
        auto resp = domain::http_response::json(std::move(body));
        resp.headers["Cache-Control"] = "no-store";
        return resp;
    }
    const auto tag = entity_tag(body);
    domain::http_response resp;
    if (names_tag(req.get_header("If-None-Match"), tag)) {
        resp.status = domain::http_status::not_modified;
    } else {
        resp = domain::http_response::json(std::move(body));
    }
    resp.headers["ETag"] = tag;
    resp.headers["Cache-Control"] = std::format("private, max-age={}", policy.stale_after->count());
    return resp;
}

}
