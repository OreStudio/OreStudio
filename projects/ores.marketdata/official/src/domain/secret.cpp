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
#include "ores.marketdata.official/domain/secret.hpp"
#include <cstdlib>

namespace ores::marketdata::official::domain {

std::expected<std::string, std::string> resolve_secret(std::string_view ref,
                                                       const environment_lookup& lookup) {
    constexpr std::string_view env_prefix = "env:";
    if (!ref.starts_with(env_prefix) || ref.size() == env_prefix.size())
        return std::unexpected("'" + std::string(ref) +
                               "' is not a credential ref: it needs the form env:NAME");
    const std::string name(ref.substr(env_prefix.size()));
    const char* value = lookup(name.c_str());
    if (value == nullptr || *value == '\0')
        return std::unexpected("the variable " + name +
                               " is not set: put the key in the environment's .env");
    return std::string(value);
}

std::expected<std::string, std::string> resolve_secret(std::string_view ref) {
    return resolve_secret(ref, [](const char* name) { return std::getenv(name); });
}

}
