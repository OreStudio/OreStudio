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
#include "ores.http/domain/http_request.hpp"

#include <algorithm>

namespace ores::http::domain {

std::string http_request::get_header(const std::string& name) const {
    // Headers are stored with lowercase keys (normalized on insertion)
    std::string lower_name = name;
    std::transform(lower_name.begin(), lower_name.end(), lower_name.begin(),
        [](unsigned char c) { return std::tolower(c); });
    auto it = headers.find(lower_name);
    if (it != headers.end()) {
        return it->second;
    }
    return "";
}

std::string http_request::get_query_param(const std::string& name) const {
    auto it = query_params.find(name);
    return it != query_params.end() ? it->second : "";
}

std::string http_request::get_path_param(const std::string& name) const {
    auto it = path_params.find(name);
    return it != path_params.end() ? it->second : "";
}

std::optional<std::string> http_request::get_bearer_token() const {
    const auto auth = get_header("Authorization");
    if (auth.empty()) {
        return std::nullopt;
    }

    const std::string bearer_prefix = "Bearer ";
    if (auth.size() > bearer_prefix.size() &&
        auth.substr(0, bearer_prefix.size()) == bearer_prefix) {
        return auth.substr(bearer_prefix.size());
    }

    return std::nullopt;
}

}
