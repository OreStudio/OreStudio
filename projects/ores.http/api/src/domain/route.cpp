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
#include "ores.http.api/domain/route.hpp"

namespace ores::http::domain {

namespace {

int hex_value(char c) {
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    return -1;
}

/// A path segment as its sender meant it: each %XX read as the byte it names.
/// A plus is a plus in a path, and an escape that is not two hex digits is kept
/// as written, so the lookup that uses it fails rather than guesses.
std::string percent_decoded(const std::string& text) {
    std::string out;
    out.reserve(text.size());
    for (std::size_t i = 0; i < text.size(); ++i) {
        if (text[i] == '%' && i + 2 < text.size()) {
            const auto high = hex_value(text[i + 1]);
            const auto low = hex_value(text[i + 2]);
            if (high >= 0 && low >= 0) {
                out += static_cast<char>(high * 16 + low);
                i += 2;
                continue;
            }
        }
        out += text[i];
    }
    return out;
}

}

bool route::match(const std::string& path,
                  std::unordered_map<std::string, std::string>& path_params) const {

    std::smatch matches;
    if (!std::regex_match(path, matches, regex)) {
        return false;
    }

    // A parameter reaches its handler decoded, as the browser encoded it with
    // encodeURIComponent and as the BFF's framework read it.
    for (std::size_t i = 0; i < param_names.size() && i + 1 < matches.size(); ++i) {
        path_params[param_names[i]] = percent_decoded(matches[i + 1].str());
    }

    return true;
}

}
