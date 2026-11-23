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
#include <cctype>

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

/// @p text folded to lowercase, because an origin is case-insensitive.
std::string folded(std::string_view text) {
    std::string out(text);
    std::ranges::transform(
        out, out.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return out;
}

/// The origins the option names, trimmed, folded and in the order it names them.
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
        out.push_back(folded(std::string_view(one).substr(first, last - first + 1)));
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

}

cors_policy make_cors_policy(const http_server_options& options) {
    cors_policy policy;
    policy.enabled = options.enable_cors;
    policy.every_origin = names_every_origin(options.cors_allowed_origins);
    if (!policy.every_origin)
        policy.origins = listed(options.cors_allowed_origins);
    return policy;
}

bool cors_policy::admits(std::string_view origin) const {
    if (!enabled || origin.empty())
        return false;
    if (every_origin)
        return true;
    return names(origin);
}

bool cors_policy::names(std::string_view origin) const {
    if (!enabled || origin.empty() || every_origin)
        return false;
    return std::ranges::find(origins, folded(origin)) != origins.end();
}

std::vector<std::pair<std::string, std::string>> cors_headers(const cors_policy& policy,
                                                              std::string_view origin) {
    if (!policy.admits(origin))
        return {};
    std::vector<std::pair<std::string, std::string>> out;
    if (policy.every_origin) {
        out.emplace_back("Access-Control-Allow-Origin", "*");
    } else {
        out.emplace_back("Access-Control-Allow-Origin", folded(origin));
        out.emplace_back("Access-Control-Allow-Credentials", "true");
        // The answer varies by the origin asked for, so a cache must not share it.
        out.emplace_back("Vary", "Origin");
    }
    out.emplace_back("Access-Control-Expose-Headers", std::string(exposed_headers));
    return out;
}

std::vector<std::pair<std::string, std::string>> cors_preflight_headers(const cors_policy& policy,
                                                                        std::string_view origin) {
    auto out = cors_headers(policy, origin);
    if (out.empty()) {
        // A preflight still gets its no-content answer, but nothing that admits it.
        return out;
    }
    out.emplace_back("Access-Control-Allow-Methods", std::string(allowed_methods));
    out.emplace_back("Access-Control-Allow-Headers", std::string(allowed_headers));
    out.emplace_back("Access-Control-Max-Age", std::string(preflight_max_age));
    return out;
}

}
