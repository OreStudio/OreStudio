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
#include "ores.platform/process/executable.hpp"

#include <array>
#include <chrono>
#include <ctime>
#include <filesystem>
#include "ores.platform/time/time_utils.hpp"

#if defined(__linux__)
#include <climits>
#include <unistd.h>
#elif defined(__APPLE__)
#include <mach-o/dyld.h>
#elif defined(_WIN32)
#include <windows.h>
#endif

namespace ores::platform::process {

std::filesystem::path executable_path() {
#if defined(__linux__)
    std::array<char, PATH_MAX> buf{};
    const ssize_t len = readlink("/proc/self/exe", buf.data(), buf.size() - 1);
    if (len > 0)
        return {std::string(buf.data(), static_cast<std::size_t>(len))};
#elif defined(__APPLE__)
    uint32_t size = 0;
    _NSGetExecutablePath(nullptr, &size);
    std::string buf(size, '\0');
    if (_NSGetExecutablePath(buf.data(), &size) == 0)
        return std::filesystem::canonical(buf);
#elif defined(_WIN32)
    std::array<wchar_t, MAX_PATH> buf{};
    const DWORD len = GetModuleFileNameW(NULL, buf.data(), MAX_PATH);
    if (len > 0)
        return std::filesystem::path(buf.data());
#endif
    return {};
}

std::string executable_build_time() {
    const auto path = executable_path();
    if (path.empty())
        return "unknown";

    std::error_code ec;
    const auto ftime = std::filesystem::last_write_time(path, ec);
    if (ec)
        return "unknown";

    const auto stp = std::chrono::clock_cast<std::chrono::system_clock>(ftime);
    const auto tt = std::chrono::system_clock::to_time_t(stp);

    std::tm tm{};
    if (!ores::platform::time::time_utils::gmtime_safe(&tt, &tm))
        return "unknown";

    std::array<char, 32> buf{};
    std::strftime(buf.data(), buf.size(), "%Y/%m/%d %H:%M:%S", &tm);
    return {buf.data()};
}

}
