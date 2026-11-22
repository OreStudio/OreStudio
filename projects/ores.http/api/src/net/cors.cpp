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
#include "ores.http.api/net/cors.hpp"
#include <algorithm>

namespace ores::http::net {

namespace {

/// The methods a browser may send. PATCH is here because the routes take it.
constexpr std::string_view allowed_methods = "GET, POST, PUT, PATCH, DELETE, OPTIONS";

/**
 * The headers a browser may send. =X-Ores-Request= is the one every write of the web
 * client carries, which the server requires so that a cross-site form cannot write with
 * a cookie it was given.
 */
constexpr std::string_view allowed_headers = "Content-Type, Authorization, X-Ores-Request";

/** The headers a browser may read. A refusal's request id is what leads to the log lines. */
constexpr std::string_view exposed_headers = "X-Request-Id, Content-Disposition";

constexpr std::string_view preflight_max_age = "86400";

/// The origins the option names, trimmed and in the order it names them.
std::vector<std::string> listed(const std::string& origins) {
    std::vector<std::string> out;
    std::string one;
    const auto take = [&out, &one] {
        const auto first = one.find_first_not_of(" \t");
        if (first == std::string::npos) {
            one.clear();
            return;
        }
        const auto last = one.find_last_not_of(" \t");
        out.push_back(one.substr(first, last - first + 1));
        one.clear();
    };
    for (const auto c : origins) {
        if (c == ',')
            take();
        else
            one.push_back(c);
    }
    take();
    return out;
}

/**
 * Whether the option names every origin. An empty option reads as the wildcard too: a
 * deployment that states no list keeps the behaviour it had before the list existed.
 */
bool names_every_origin(const std::string& origins) {
    return origins.empty() || origins == "*";
}

/// Whether @p origin earns an answer at all.
bool admitted(const http_server_options& options, std::string_view origin) {
    if (!options.enable_cors || origin.empty())
        return false;
    if (names_every_origin(options.cors_allowed_origins))
        return true;
    const auto all = listed(options.cors_allowed_origins);
    return std::ranges::find(all, std::string(origin)) != all.end();
}

}

std::vector<std::pair<std::string, std::string>> cors_headers(const http_server_options& options,
                                                             std::string_view origin) {
    if (!admitted(options, origin))
        return {};
    std::vector<std::pair<std::string, std::string>> out;
    if (names_every_origin(options.cors_allowed_origins)) {
        out.emplace_back("Access-Control-Allow-Origin", "*");
    } else {
        out.emplace_back("Access-Control-Allow-Origin", std::string(origin));
        out.emplace_back("Access-Control-Allow-Credentials", "true");
        // The answer varies by the origin asked for, so a cache must not share it.
        out.emplace_back("Vary", "Origin");
    }
    out.emplace_back("Access-Control-Expose-Headers", std::string(exposed_headers));
    return out;
}

std::vector<std::pair<std::string, std::string>>
cors_preflight_headers(const http_server_options& options, std::string_view origin) {
    auto out = cors_headers(options, origin);
    if (out.empty()) {
        // A preflight still gets its no-content answer, but nothing that admits it.
        return out;
    }
    out.emplace_back("Access-Control-Allow-Methods", std::string(allowed_methods));
    out.emplace_back("Access-Control-Allow-Headers", std::string(allowed_headers));
    out.emplace_back("Access-Control-Max-Age", std::string(preflight_max_age));
    return out;
}

bool cors_names(const http_server_options& options, std::string_view origin) {
    if (!options.enable_cors || origin.empty() || names_every_origin(options.cors_allowed_origins))
        return false;
    const auto all = listed(options.cors_allowed_origins);
    return std::ranges::find(all, std::string(origin)) != all.end();
}

}
