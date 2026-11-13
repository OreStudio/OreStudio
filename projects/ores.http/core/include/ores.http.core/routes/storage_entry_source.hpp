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
#ifndef ORES_HTTP_CORE_ROUTES_STORAGE_ENTRY_SOURCE_HPP
#define ORES_HTTP_CORE_ROUTES_STORAGE_ENTRY_SOURCE_HPP

#include "ores.http.api/domain/body_source.hpp"
#include "ores.http.core/export.hpp"
#include "ores.storage.core/filesystem/archive_reader.hpp"
#include <cstdint>
#include <filesystem>
#include <memory>
#include <optional>
#include <string_view>

namespace ores::http::server::routes {

/**
 * @brief Serves one file out of a package, a buffer at a time.
 *
 * A caller that reads a result out of a twenty megabyte package asks for that
 * file, not the package. The archive is walked where it lies and the entry is
 * copied out as the socket asks for it, so neither the package nor the entry
 * costs its own size in memory.
 *
 * The length is the entry's own, which lets the response carry
 * =Content-Length=, or the part of it a range asked for.
 */
class ORES_HTTP_CORE_EXPORT storage_entry_source final : public domain::body_source {
public:
    /**
     * @throws std::runtime_error when the object is not a package this build
     * reads.
     */
    storage_entry_source(std::filesystem::path packed,
                         std::string_view name,
                         std::uint64_t offset = 0,
                         std::uint64_t length = storage::filesystem::archive_reader::stream::to_end);

    /// Whether the package holds a regular file of that name.
    [[nodiscard]] bool found() const noexcept {
        return entry_.found();
    }

    [[nodiscard]] std::optional<std::uint64_t> size() const override;

    std::size_t read(std::span<char> into) override;

private:
    storage::filesystem::archive_reader::stream entry_;
};

}

#endif
