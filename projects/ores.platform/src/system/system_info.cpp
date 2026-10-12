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
#include "ores.platform/system/system_info.hpp"
#include <thread>

#if defined(_WIN32)
#    include <windows.h>
#elif defined(__APPLE__)
#    include <sys/sysctl.h>
#    include <sys/types.h>
#else
#    include <unistd.h>
#endif

namespace ores::platform::system {

namespace {

/// The physical memory in bytes, or zero when the computer does not say.
std::int64_t physical_memory_bytes() {
#if defined(_WIN32)
    MEMORYSTATUSEX status{};
    status.dwLength = sizeof(status);
    return GlobalMemoryStatusEx(&status) ? static_cast<std::int64_t>(status.ullTotalPhys) : 0;
#elif defined(__APPLE__)
    std::int64_t bytes = 0;
    std::size_t size = sizeof(bytes);
    return sysctlbyname("hw.memsize", &bytes, &size, nullptr, 0) == 0 ? bytes : 0;
#else
    const long pages = sysconf(_SC_PHYS_PAGES);
    const long page_size = sysconf(_SC_PAGESIZE);
    if (pages <= 0 || page_size <= 0)
        return 0;
    return static_cast<std::int64_t>(pages) * static_cast<std::int64_t>(page_size);
#endif
}

} // namespace

system_info get_system_info() {
    system_info info;
    info.cpu_count = static_cast<int>(std::thread::hardware_concurrency());
    info.ram_mb = physical_memory_bytes() / (1024 * 1024);
    return info;
}

}
