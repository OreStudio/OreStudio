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
#include "ores.http.core/routes/storage_file_source.hpp"
#include <algorithm>
#include <stdexcept>
#include <system_error>

namespace ores::http::server::routes {

storage_file_source::storage_file_source(std::filesystem::path path,
                                           std::uint64_t offset,
                                           std::uint64_t length)
    : path_(std::move(path)) {

    std::error_code ec;
    const auto stated = std::filesystem::file_size(path_, ec);
    if (ec)
        throw std::runtime_error("Cannot read object: " + path_.string() + ": " + ec.message());

    const auto start = std::min(offset, stated);
    const auto rest = stated - start;
    size_ = length == to_end ? rest : std::min(length, rest);

    in_.open(path_, std::ios::binary);
    if (!in_)
        throw std::runtime_error("Cannot read object: " + path_.string());
    if (start != 0)
        in_.seekg(static_cast<std::streamoff>(start));
    if (!in_)
        throw std::runtime_error("Cannot read object: " + path_.string());
}

std::optional<std::uint64_t> storage_file_source::size() const {
    return size_;
}

std::size_t storage_file_source::read(std::span<char> into) {
    if (into.empty())
        return 0;

    const auto remaining = size_ - produced_;
    if (remaining == 0)
        return 0;
    const auto want = static_cast<std::size_t>(
        std::min<std::uint64_t>(remaining, static_cast<std::uint64_t>(into.size())));

    in_.read(into.data(), static_cast<std::streamsize>(want));
    if (in_.bad())
        throw std::runtime_error("Cannot read object: " + path_.string());

    const auto taken = static_cast<std::size_t>(in_.gcount());
    produced_ += taken;
    return taken > 0 ? taken : 0;
}

}
