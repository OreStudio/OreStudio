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
#include "ores.platform/environment/real_environment_provider.hpp"
#include <cstdlib>
#include <string>

namespace ores::platform::environment {

std::optional<std::string> real_environment_provider::get(const std::string& name) const {
#ifdef _WIN32
    /*
     * The safe accessor, not getenv. The Windows build treats the CRT's
     * deprecation of getenv as an error, and the usual escape -- defining
     * _CRT_SECURE_NO_WARNINGS in this file -- cannot work under the unity
     * build: the file is compiled after others that have already pulled in the
     * CRT, so the define arrives too late to affect the declaration.
     */
    char* value = nullptr;
    std::size_t size = 0;
    if (_dupenv_s(&value, &size, name.c_str()) != 0 || value == nullptr)
        return {};
    std::string copy(value);
    std::free(value);
    return copy;
#else
    const char* value = std::getenv(name.c_str());
    if (value == nullptr)
        return {};
    return std::string(value);
#endif
}

void real_environment_provider::set(const std::string& name, const std::string& value) {
#ifdef _WIN32
    _putenv_s(name.c_str(), value.c_str());
#else
    setenv(name.c_str(), value.c_str(), 1);
#endif
}

void real_environment_provider::unset(const std::string& name) {
#ifdef _WIN32
    _putenv_s(name.c_str(), "");
#else
    unsetenv(name.c_str());
#endif
}

}
