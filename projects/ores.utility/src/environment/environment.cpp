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
#define _CRT_SECURE_NO_WARNINGS // Avoid getenv warnings

#include "ores.utility/environment/environment.hpp"

#include <cstdlib>
#include "ores.utility/string/converter.hpp"

namespace ores::utility::environment {

std::optional<std::string>
environment::get_value(const std::string& name) {
    const char* value = std::getenv(name.c_str());
    if (value == nullptr)
        return {};
    return std::string(value);
}

std::string environment::get_value_or_default(const std::string& name,
    const std::string& default_value) {
    auto value = get_value(name);
    return value.value_or(default_value);
}

int environment::
get_int_value_or_default(const std::string& name, int default_value) {
    auto value = get_value(name);
    if (!value.has_value())
        return default_value;

    try {
        return string::converter::string_to_int(value.value());
    } catch(...) {
        return default_value;
    }
}

void environment::set_value(const std::string& name, const std::string& value) {
#ifdef _WIN32
    _putenv_s(name.c_str(), value.c_str());
#else
    setenv(name.c_str(), value.c_str(), 1);
#endif
}

}
