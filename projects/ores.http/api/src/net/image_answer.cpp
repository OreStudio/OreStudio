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
#include "ores.http.api/net/image_answer.hpp"
#include <algorithm>
#include <cctype>

namespace ores::http::net {

namespace {

/// Whether a stored type names an image and is safe as a header value: image/
/// and then only the characters a media type's subtype may hold.
bool is_image_type(const std::string& type) {
    if (!type.starts_with("image/") || type.size() == 6)
        return false;
    return std::ranges::all_of(type.substr(6), [](unsigned char c) {
        return std::isalnum(c) || c == '+' || c == '-' || c == '.';
    });
}

}

domain::http_response image_answer(const std::string& mime_type, std::string bytes) {
    domain::http_response resp;
    resp.status = domain::http_status::ok;
    resp.content_type = mime_type.empty()        ? std::string("image/svg+xml") :
                        is_image_type(mime_type) ? mime_type :
                                                   std::string("application/octet-stream");
    resp.body = std::move(bytes);
    resp.headers["Cache-Control"] = "private, max-age=300";
    resp.headers["X-Content-Type-Options"] = "nosniff";
    resp.headers["Content-Security-Policy"] = "default-src 'none'; sandbox";
    return resp;
}

}
