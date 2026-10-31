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
#ifndef ORES_HTTP_CORE_ROUTES_STORAGE_FILE_SOURCE_HPP
#define ORES_HTTP_CORE_ROUTES_STORAGE_FILE_SOURCE_HPP

#include "ores.http.api/domain/body_source.hpp"
#include "ores.http.core/export.hpp"
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <optional>

namespace ores::http::server::routes {

/**
 * @brief Serves a file as the body of a response, a buffer at a time.
 *
 * The file is opened once and read as the socket asks for it, so a large object
 * costs a buffer and not itself. The length is the file's own, which lets the
 * response carry =Content-Length= and the client show progress.
 *
 * The read is blocking on the connection's thread. A local file is in the page
 * cache, and the design's asynchronous file path belongs with the net layer.
 */
class ORES_HTTP_CORE_EXPORT storage_file_source final : public domain::body_source {
public:
    /**
     * @brief Opens the file for reading.
     *
     * @throws std::runtime_error when the file cannot be opened.
     */
    explicit storage_file_source(std::filesystem::path path);

    [[nodiscard]] std::optional<std::uint64_t> size() const override;

    std::size_t read(std::span<char> into) override;

private:
    std::filesystem::path path_;
    std::ifstream in_;
    std::uint64_t size_ = 0;
};

}

#endif
