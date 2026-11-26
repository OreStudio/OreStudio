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
#include "ores.compute.wrapper/app/job_scratch.hpp"
#include <cstdint>
#include <filesystem>
#include <system_error>

namespace ores::compute::wrapper::app {

namespace fs = std::filesystem;

namespace {

/**
 * @brief The bytes a folder tree holds, counting the files it reaches.
 *
 * A file that disappears while the walk runs is left out rather than counted
 * as an error: the folder is scratch and nothing reads these numbers but a log
 * line.
 */
std::uintmax_t folder_bytes(const fs::path& folder) {
    std::error_code ignored;
    std::uintmax_t total = 0;
    for (fs::recursive_directory_iterator it(folder, ignored), end; it != end;
         it.increment(ignored)) {
        if (ignored) {
            ignored.clear();
            continue;
        }
        if (!it->is_regular_file(ignored))
            continue;
        const auto size = it->file_size(ignored);
        if (!ignored)
            total += size;
        ignored.clear();
    }
    return total;
}

}

std::optional<std::uintmax_t> release_job_folder(const fs::path& job_folder) {
    std::error_code exists_error;
    if (!fs::exists(job_folder, exists_error))
        return std::uintmax_t{0};

    const auto bytes = folder_bytes(job_folder);

    std::error_code remove_error;
    fs::remove_all(job_folder, remove_error);
    if (remove_error)
        return std::nullopt;

    std::error_code still_there_error;
    if (fs::exists(job_folder, still_there_error))
        return std::nullopt;

    return bytes;
}

}
