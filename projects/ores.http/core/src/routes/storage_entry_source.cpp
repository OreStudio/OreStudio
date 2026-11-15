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
#include "ores.http.core/routes/storage_entry_source.hpp"
#include <stdexcept>

namespace ores::http::server::routes {

storage_entry_source::storage_entry_source(std::filesystem::path packed,
                                           std::string_view name,
                                           std::uint64_t offset,
                                           std::uint64_t length)
    : entry_(std::move(packed), name, offset, length) {}

std::optional<std::uint64_t> storage_entry_source::size() const {
    return entry_.size();
}

std::size_t storage_entry_source::read(std::span<char> into) {
    if (into.empty())
        return 0;
    const auto bytes = std::span(reinterpret_cast<std::byte*>(into.data()), into.size());
    const auto got = entry_.read(bytes);
    if (got == 0 && into.size() != 0)
        return 0;
    return got;
}

}
