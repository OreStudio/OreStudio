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
#include "ores.http/net/router.hpp"

#include <sstream>

namespace ores::http::net {

using namespace ores::telemetry::log;

namespace {

/**
 * @brief Converts a path pattern like "/users/{id}" to a regex and extracts param names.
 */
std::pair<std::regex, std::vector<std::string>> compile_pattern(
    const std::string& pattern) {

    std::vector<std::string> param_names;
    std::ostringstream regex_str;
    regex_str << "^";

    std::size_t pos = 0;
    while (pos < pattern.size()) {
        if (pattern[pos] == '{') {
            auto end = pattern.find('}', pos);
            if (end != std::string::npos) {
                std::string param_name = pattern.substr(pos + 1, end - pos - 1);
                param_names.push_back(param_name);
                regex_str << "([^/]+)";
                pos = end + 1;
                continue;
            }
        }
        // Escape regex special characters
        char c = pattern[pos];
        if (c == '.' || c == '+' || c == '*' || c == '?' ||
            c == '(' || c == ')' || c == '[' || c == ']' ||
            c == '$' || c == '^' || c == '|' || c == '\\') {
            regex_str << '\\';
        }
        regex_str << c;
        ++pos;
    }

    regex_str << "$";
    return {std::regex(regex_str.str()), param_names};
}

}

// route_builder implementation

route_builder::route_builder(domain::http_method method, const std::string& pattern)
    : method_(method), pattern_(pattern) {
}

route_builder& route_builder::handler(domain::request_handler h) {
    handler_ = std::move(h);
    return *this;
}

route_builder& route_builder::auth_required() {
    requires_auth_ = true;
    return *this;
}

route_builder& route_builder::roles(std::vector<std::string> r) {
    required_roles_ = std::move(r);
    requires_auth_ = true;  // Roles imply auth required
    return *this;
}

route_builder& route_builder::summary(std::string s) {
    summary_ = std::move(s);
    return *this;
}

route_builder& route_builder::description(std::string d) {
    description_ = std::move(d);
    return *this;
}

route_builder& route_builder::tags(std::vector<std::string> t) {
    tags_ = std::move(t);
    return *this;
}

domain::route route_builder::build() const {
    auto [regex, param_names] = compile_pattern(pattern_);

    domain::route r;
    r.method = method_;
    r.pattern = pattern_;
    r.regex = std::move(regex);
    r.param_names = std::move(param_names);
    r.handler = handler_;
    r.requires_auth = requires_auth_;
    r.required_roles = required_roles_;
    r.summary = summary_;
    r.description = description_;
    r.tags = tags_;
    return r;
}

// router implementation

router::router() {
    BOOST_LOG_SEV(lg(), debug) << "Router created";
}

route_builder router::get(const std::string& pattern) {
    return route_builder(domain::http_method::get, pattern);
}

route_builder router::post(const std::string& pattern) {
    return route_builder(domain::http_method::post, pattern);
}

route_builder router::put(const std::string& pattern) {
    return route_builder(domain::http_method::put, pattern);
}

route_builder router::patch(const std::string& pattern) {
    return route_builder(domain::http_method::patch, pattern);
}

route_builder router::delete_(const std::string& pattern) {
    return route_builder(domain::http_method::delete_, pattern);
}

void router::add_route(const domain::route& route) {
    BOOST_LOG_SEV(lg(), info) << "Registered route: "
        << static_cast<int>(route.method) << " " << route.pattern;
    routes_.push_back(route);
}

std::optional<domain::route> router::match(domain::http_method method,
    const std::string& path,
    std::unordered_map<std::string, std::string>& path_params) const {

    BOOST_LOG_SEV(lg(), trace) << "Matching path: " << path
        << " method: " << static_cast<int>(method);

    for (const auto& route : routes_) {
        if (route.method != method) {
            continue;
        }

        if (route.match(path, path_params)) {
            BOOST_LOG_SEV(lg(), trace) << "Matched route: " << route.pattern;
            return route;
        }
    }

    BOOST_LOG_SEV(lg(), trace) << "No matching route found for: " << path;
    return std::nullopt;
}

}
