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
#include "ores.compute.wrapper/app/package_cache.hpp"
#include "ores.storage.core/filesystem/archiver.hpp"
#include <filesystem>
#include <system_error>

namespace ores::compute::wrapper::app {

namespace fs = std::filesystem;

std::string package_cache_key(const messaging::work_assignment_event& evt) {
    return evt.package_sha256.empty() ? evt.app_version_id : evt.package_sha256;
}

fs::path package_cache_dir(const fs::path& packages_root,
                           const messaging::work_assignment_event& evt) {
    return packages_root / package_cache_key(evt);
}

void unpack_package(const fs::path& pkg_cache_dir, const fs::path& pkg_archive) {
    const fs::path unpack_dir = pkg_cache_dir.string() + ".tmp";
    std::error_code ignored;
    fs::remove_all(unpack_dir, ignored);
    fs::create_directories(unpack_dir);
    ores::storage::filesystem::archiver::extract(pkg_archive, unpack_dir);
    fs::remove_all(pkg_cache_dir, ignored);
    fs::rename(unpack_dir, pkg_cache_dir);
    fs::remove(pkg_archive, ignored);
}

}
