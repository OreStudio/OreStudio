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
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <fstream>
#include <sstream>
#include <string>

namespace {

const std::string tags("[system]");

#if defined(__linux__)
/// MemTotal from /proc/meminfo in MiB, read through a different API from the subject's.
std::int64_t kernel_mem_total_mb() {
    std::ifstream meminfo("/proc/meminfo");
    for (std::string line; std::getline(meminfo, line);) {
        if (line.starts_with("MemTotal:")) {
            std::istringstream fields(line.substr(9));
            std::int64_t kilobytes = 0;
            fields >> kilobytes;
            return kilobytes / 1024;
        }
    }
    return 0;
}
#endif

} // namespace

using ores::platform::system::get_system_info;

TEST_CASE("system_info_reports_at_least_one_processor", tags) {
    CHECK(get_system_info().cpu_count >= 1);
}

TEST_CASE("system_info_reports_a_plausible_amount_of_memory", tags) {
    const auto ram_mb = get_system_info().ram_mb;

    // Over a hundred MiB and under sixteen TiB: a unit mistake would miss both.
    CHECK(ram_mb > 100);
    CHECK(ram_mb < 16LL * 1024 * 1024);
}

#if defined(__linux__)
TEST_CASE("system_info_agrees_with_the_kernel_about_memory", tags) {
    const auto reported = get_system_info().ram_mb;
    const auto kernel = kernel_mem_total_mb();

    REQUIRE(kernel > 0);
    // MemTotal leaves out the memory the kernel reserves, so it is a little less.
    CHECK(reported >= kernel);
    CHECK(reported <= kernel + kernel / 5);
}
#endif
