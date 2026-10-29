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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_PLATFORM_TESTS_C_RUNTIME_ENVIRONMENT_HPP
#define ORES_PLATFORM_TESTS_C_RUNTIME_ENVIRONMENT_HPP

#include <cstdlib>
#include <optional>
#include <string>

/**
 * @brief The process environment, read and written the way each runtime asks.
 *
 * Some of these tests compare what the provider reports with what the C runtime
 * itself holds, so they cannot ask the provider: they have to reach the runtime.
 * That reaching is platform-specific, and it is done here once rather than in
 * every case.
 *
 * Nothing here suppresses a warning. The Windows CRT deprecates `getenv` in
 * favour of `_dupenv_s` and the build treats that deprecation as an error, so
 * the Windows branch calls the accessor the CRT recommends instead of silencing
 * the one it condemns. The same applies to the timezone calls: `tzset` is
 * POSIX, `_tzset` is the CRT's.
 */
namespace ores::platform::tests {

/** The value the C runtime holds for @p name, when it holds one. */
inline std::optional<std::string> c_runtime_environment_variable(const std::string& name) {
#ifdef _WIN32
    char* value = nullptr;
    std::size_t size = 0;
    const auto outcome = _dupenv_s(&value, &size, name.c_str());
    std::optional<std::string> found;
    if (outcome == 0 && value != nullptr)
        found = std::string(value);
    // _dupenv_s allocates whenever it fills the buffer, including on the paths
    // that then report a refusal, so the pointer is freed whichever way it went.
    std::free(value);
    return found;
#else
    const char* value = std::getenv(name.c_str());
    if (value == nullptr)
        return {};
    return std::string(value);
#endif
}

/** Sets @p name in the C runtime's own environment block. */
inline void c_runtime_set_environment_variable(const std::string& name, const std::string& value) {
#ifdef _WIN32
    _putenv_s(name.c_str(), value.c_str());
#else
    setenv(name.c_str(), value.c_str(), 1);
#endif
}

/** Removes @p name from the C runtime's own environment block. */
inline void c_runtime_unset_environment_variable(const std::string& name) {
#ifdef _WIN32
    _putenv_s(name.c_str(), "");
#else
    unsetenv(name.c_str());
#endif
}

/** Makes a change to TZ take effect in the C runtime's clock. */
inline void c_runtime_reload_environment_clock() {
#ifdef _WIN32
    _tzset();
#else
    tzset();
#endif
}

}

#endif
