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
#ifndef ORES_STORAGE_CORE_FILESYSTEM_ARCHIVE_READER_HPP
#define ORES_STORAGE_CORE_FILESYSTEM_ARCHIVE_READER_HPP

#include "ores.storage.core/export.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::storage::filesystem {

/**
 * @brief What an archive holds, read from the bytes of the archive itself.
 *
 * A result package is a .tar.gz the compute workers leave in the store, and a
 * person reads one file out of it. This reads the package as the object it is,
 * in memory, so the service that holds the object answers what is inside it
 * and no caller has to unpack it.
 */
class ORES_STORAGE_CORE_EXPORT archive_reader final {
public:
    /// One regular file an archive holds.
    struct entry final {
        std::string name;
        std::uint64_t size{};
    };

    /**
     * @brief The regular files an archive holds, in the order it states them.
     *
     * A directory or a link is not a file a person opens, so neither is stated.
     *
     * @throws std::runtime_error when the bytes are not an archive this build
     * reads.
     */
    static std::vector<entry> index(std::span<const std::byte> packed);

    /**
     * @brief The bytes of one entry, or nothing when the archive holds no entry
     * of that name.
     *
     * @throws std::runtime_error when the bytes are not an archive this build
     * reads.
     */
    static std::vector<std::byte> extract(std::span<const std::byte> packed, std::string_view name);
};

}

#endif
