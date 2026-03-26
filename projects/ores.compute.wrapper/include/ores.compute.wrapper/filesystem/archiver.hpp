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
#ifndef ORES_COMPUTE_WRAPPER_FILESYSTEM_ARCHIVER_HPP
#define ORES_COMPUTE_WRAPPER_FILESYSTEM_ARCHIVER_HPP

#include <filesystem>

namespace ores::compute::wrapper::filesystem {

/**
 * @brief Cross-platform .tar.gz archive operations using libarchive.
 *
 * Encapsulates all libarchive API calls and handles the platform difference
 * in path encoding: on Windows, std::filesystem::path::c_str() returns
 * wchar_t*, so the wide-character libarchive variants must be used.
 * On POSIX, the standard char* variants are used.
 */
class archiver final {
public:
    /**
     * @brief Pack a directory tree into a .tar.gz archive.
     *
     * Recursively packs all regular files in @p source_dir into a gzipped
     * tar archive at @p output_archive. Entry paths stored in the archive
     * are relative to @p source_dir.
     *
     * @throws std::runtime_error on failure.
     */
    static void pack(const std::filesystem::path& source_dir,
        const std::filesystem::path& output_archive);

    /**
     * @brief Extract a .tar.gz archive into a destination directory.
     *
     * Extracts all entries from @p archive_path, rebasing them under
     * @p dest_dir.
     *
     * @throws std::runtime_error on failure.
     */
    static void extract(const std::filesystem::path& archive_path,
        const std::filesystem::path& dest_dir);
};

}

#endif
