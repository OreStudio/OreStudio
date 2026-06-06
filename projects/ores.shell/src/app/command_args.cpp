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
#include "ores.shell/app/command_args.hpp"
#include <algorithm>
#include <stdexcept>

namespace ores::shell::app {

bool parsed_args::flag_set(const std::string& name) const {
    auto i = flags.find(name);
    return i != flags.end() && i->second == "true";
}

const std::string& parsed_args::flag(const std::string& name) const {
    return flags.at(name);
}

std::expected<parsed_args, std::string>
parse_args(const std::vector<std::string>& tokens,
           const std::vector<flag_spec>& specs) {
    parsed_args r;
    for (const auto& spec : specs)
        r.flags[spec.name] = spec.requires_value ? spec.default_value
                                                 : std::string("false");

    auto find_spec = [&](const std::string& name) {
        return std::find_if(specs.begin(), specs.end(),
                            [&](const auto& s) { return s.name == name; });
    };

    for (std::size_t i = 0; i < tokens.size(); ++i) {
        const auto& token = tokens[i];
        if (!token.starts_with("--")) {
            r.positionals.push_back(token);
            continue;
        }

        auto body = token.substr(2);
        std::string inline_value;
        bool has_inline_value = false;
        if (auto eq = body.find('='); eq != std::string::npos) {
            inline_value = body.substr(eq + 1);
            body = body.substr(0, eq);
            has_inline_value = true;
        }

        auto spec = find_spec(body);
        if (spec == specs.end())
            return std::unexpected("Unknown flag: --" + body);

        if (!spec->requires_value) {
            if (has_inline_value)
                return std::unexpected("Flag --" + body +
                                       " does not take a value");
            r.flags[body] = "true";
            continue;
        }

        if (has_inline_value) {
            r.flags[body] = inline_value;
            continue;
        }

        if (i + 1 >= tokens.size() || tokens[i + 1].starts_with("--"))
            return std::unexpected("Flag --" + body + " requires a value");
        r.flags[body] = tokens[++i];
    }
    return r;
}

std::optional<std::chrono::seconds>
parse_positive_seconds(const std::string& value) {
    try {
        std::size_t pos = 0;
        const long v = std::stol(value, &pos);
        if (pos != value.size() || v <= 0)
            return std::nullopt;
        return std::chrono::seconds(v);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

}
