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
#include "ores.http.api/domain/session_cookie.hpp"
#include <vector>

namespace ores::http::domain {

namespace {

/// The prefix a browser accepts only with Secure, Path=/ and no Domain.
constexpr std::string_view host_prefix = "__Host-";

/// One =name=value= pair of a Cookie header, without its surrounding space.
std::string_view trim(std::string_view text) {
    while (!text.empty() && (text.front() == ' ' || text.front() == '\t'))
        text.remove_prefix(1);
    while (!text.empty() && (text.back() == ' ' || text.back() == '\t'))
        text.remove_suffix(1);
    return text;
}

}

std::string session_cookie::issue_name(const cookie_settings& settings) {
    if (!settings.secure)
        return settings.name;
    // A browser rejects __Host- from a plain connection, so the prefix is the
    // deployment's TLS statement and not a decoration.
    return std::string(host_prefix) + settings.name;
}

std::string session_cookie::issue(const cookie_settings& settings, const std::string& identifier) {
    auto value = issue_name(settings) + "=" + identifier + "; Path=" + settings.path +
                 "; HttpOnly; SameSite=Lax";
    if (settings.secure)
        value += "; Secure";
    return value;
}

std::string session_cookie::clear(const cookie_settings& settings) {
    auto value =
        issue_name(settings) + "=; Path=" + settings.path + "; HttpOnly; SameSite=Lax; Max-Age=0";
    if (settings.secure)
        value += "; Secure";
    return value;
}

std::optional<std::string> session_cookie::read(std::string_view header,
                                                const cookie_settings& settings) {
    const auto wanted = issue_name(settings);
    auto rest = header;

    while (!rest.empty()) {
        const auto separator = rest.find(';');
        const auto pair = trim(rest.substr(0, separator));
        rest =
            separator == std::string_view::npos ? std::string_view{} : rest.substr(separator + 1);

        const auto equals = pair.find('=');
        if (equals == std::string_view::npos)
            continue;
        if (trim(pair.substr(0, equals)) != wanted)
            continue;

        const auto value = trim(pair.substr(equals + 1));
        if (value.empty())
            return std::nullopt;
        return std::string(value);
    }
    return std::nullopt;
}

}
