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
#ifndef ORES_HTTP_CORE_ROUTES_BUNDLE_MANIFEST_HPP
#define ORES_HTTP_CORE_ROUTES_BUNDLE_MANIFEST_HPP

#include "ores.http.core/export.hpp"
#include <cstdint>
#include <filesystem>
#include <optional>
#include <string>
#include <unordered_map>

namespace ores::http::server::routes {

/**
 * @brief One file of the built browser bundle.
 */
struct ORES_HTTP_CORE_EXPORT bundle_entry final {
    /// The path a browser asks for, relative to the bundle root, with '/' separators.
    std::string path;

    /// The file's length, in bytes.
    std::uintmax_t size = 0;

    /// Whether a brotli copy of this file was built beside it.
    bool compressed = false;
};

/**
 * @brief What the bundle holds, read once from the directory the build wrote.
 *
 * A request is resolved by looking its path up here and nowhere else: no path is
 * joined to the root at request time, so a request that names =..= finds nothing
 * rather than escaping. This is the table the design asks for; the build writes
 * the files and the server reads the directory, which keeps the bundle's layout
 * out of the build tool's plugin configuration.
 */
class ORES_HTTP_CORE_EXPORT bundle_manifest final {
public:
    /**
     * @brief Reads the bundle the root holds.
     *
     * @throws std::runtime_error when the root is not a directory.
     */
    explicit bundle_manifest(std::filesystem::path root);

    /// The file a browser asked for, or nothing when the bundle does not hold it.
    [[nodiscard]] const bundle_entry* find(const std::string& path) const;

    /// The directory the files are read from.
    [[nodiscard]] const std::filesystem::path& root() const noexcept {
        return root_;
    }

    /// How many files the bundle holds.
    [[nodiscard]] std::size_t size() const noexcept {
        return entries_.size();
    }

private:
    std::filesystem::path root_;
    std::unordered_map<std::string, bundle_entry> entries_;
};

}

#endif
