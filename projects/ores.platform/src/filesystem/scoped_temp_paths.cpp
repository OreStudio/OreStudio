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
#include "ores.platform/filesystem/io_error.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include "ores.platform/filesystem/scoped_temp_file.hpp"
#include "temp_path.hpp"
#include <boost/throw_exception.hpp>
#include <atomic>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <random>
#include <string>
#include <system_error>

namespace ores::platform::filesystem {

namespace {

// A separate engine per thread keeps the draw off a shared lock, and the
// seed mixes random_device with a counter so that two threads starting at
// the same instant do not draw the same sequence.
std::uint64_t random_suffix_seed() {
    static std::atomic<std::uint64_t> counter{0};

    std::random_device device;
    const std::uint64_t high = (static_cast<std::uint64_t>(device()) << 32) ^ device();
    return high ^ (counter.fetch_add(1, std::memory_order_relaxed) * 0x9E3779B97F4A7C15ULL);
}

std::string random_suffix() {
    thread_local std::mt19937_64 engine(random_suffix_seed());

    return std::format("{:016x}{:016x}", engine(), engine());
}

}

std::filesystem::path nonexistent_temp_path(std::string_view prefix) {
    const auto suffix = random_suffix();
    std::string name(prefix);
    name += suffix;
    return std::filesystem::temp_directory_path() / name;
}

scoped_temp_file::scoped_temp_file() {
    constexpr int max_attempts = 64;

    for (int attempt = 0; attempt < max_attempts; ++attempt) {
        auto candidate = nonexistent_temp_path("ores_temp_file_");
        if (std::filesystem::exists(candidate))
            continue;

        std::ofstream stream(candidate);
        if (stream) {
            path_ = std::move(candidate);
            return;
        }
    }

    BOOST_THROW_EXCEPTION(
        io_error("Could not create a temporary file under the system temporary directory."));
}

scoped_temp_file::~scoped_temp_file() {
    remove_now();
}

void scoped_temp_file::remove_now() noexcept {
    if (path_.empty())
        return;

    std::error_code ec;
    std::filesystem::remove(path_, ec);
}

scoped_temp_directory::scoped_temp_directory() {
    constexpr int max_attempts = 64;

    for (int attempt = 0; attempt < max_attempts; ++attempt) {
        auto candidate = nonexistent_temp_path("ores_temp_dir_");
        if (std::filesystem::exists(candidate))
            continue;

        std::error_code ec;
        if (std::filesystem::create_directory(candidate, ec)) {
            path_ = std::move(candidate);
            return;
        }
        if (ec)
            BOOST_THROW_EXCEPTION(io_error("Could not create a temporary directory under the "
                                           "system temporary directory. Error: " +
                                           ec.message()));
    }

    BOOST_THROW_EXCEPTION(
        io_error("Could not create a temporary directory under the system temporary directory."));
}

scoped_temp_directory::~scoped_temp_directory() {
    remove_now();
}

void scoped_temp_directory::remove_now() noexcept {
    if (path_.empty())
        return;

    std::error_code ec;
    std::filesystem::remove_all(path_, ec);
}

}
