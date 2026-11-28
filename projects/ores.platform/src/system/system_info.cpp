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
#include <array>
#include <thread>

#if defined(_WIN32)
#    include <windows.h>
#elif defined(__APPLE__)
#    include <sys/mount.h>
#    include <sys/param.h>
#    include <sys/sysctl.h>
#    include <sys/types.h>
#else
#    include <sys/statvfs.h>
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

std::optional<filesystem_space> get_filesystem_space(const std::string& path) {
#if defined(_WIN32)
    ULARGE_INTEGER free_to_caller{};
    ULARGE_INTEGER total{};
    if (!GetDiskFreeSpaceExA(path.c_str(), &free_to_caller, &total, nullptr))
        return std::nullopt;
    filesystem_space space;
    space.free_bytes = free_to_caller.QuadPart;
    space.total_bytes = total.QuadPart;
    return space;
#else
    struct statvfs stats{};
    if (::statvfs(path.c_str(), &stats) != 0)
        return std::nullopt;
    /*
     * f_frsize is the fragment size the figures below are counted in; f_bsize is
     * the preferred block size and is not the unit of f_bavail. Reading them the
     * other way round overstates the space by the ratio between the two.
     */
    const auto unit =
        static_cast<std::uint64_t>(stats.f_frsize == 0 ? stats.f_bsize : stats.f_frsize);
    filesystem_space space;
    space.total_bytes = static_cast<std::uint64_t>(stats.f_blocks) * unit;
    space.free_bytes = static_cast<std::uint64_t>(stats.f_bavail) * unit;
    return space;
#endif
}

std::string get_host_name() {
#if defined(_WIN32)
    std::array<char, 256> buffer{};
    DWORD size = static_cast<DWORD>(buffer.size());
    return GetComputerNameA(buffer.data(), &size) ? std::string(buffer.data(), size) :
                                                    "unknown host";
#else
    std::array<char, 256> buffer{};
    if (::gethostname(buffer.data(), buffer.size() - 1) != 0)
        return "unknown host";
    buffer.back() = '\0';
    return std::string{buffer.data()};
#endif
}

system_info get_system_info() {
    system_info info;
    info.cpu_count = static_cast<int>(std::thread::hardware_concurrency());
    info.ram_mb = physical_memory_bytes() / (1024 * 1024);
    return info;
}

}
