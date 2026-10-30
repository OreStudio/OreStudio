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
#ifndef ORES_HTTP_CORE_ROUTES_STORAGE_UPLOAD_SINK_HPP
#define ORES_HTTP_CORE_ROUTES_STORAGE_UPLOAD_SINK_HPP

#include "ores.http.api/domain/body_sink.hpp"
#include "ores.http.core/export.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <string>
#include <string_view>

namespace ores::http::server::routes {

/**
 * @brief Writes a streamed upload to a temporary file and renames it into place.
 *
 * The bytes of an object never sit in memory: each chunk is written as it
 * arrives, and the object appears at its key only when the last chunk has been
 * taken and the route has decided the caller may have it. A request that fails
 * anywhere leaves neither an object nor a part file.
 */
class ORES_HTTP_CORE_EXPORT storage_upload_sink final : public domain::body_sink {
public:
    /**
     * @brief Prepares a part file beside the object.
     *
     * @throws std::invalid_argument when the bucket or key cannot name an object,
     * which the session answers as a bad request rather than as a server fault.
     * @throws std::runtime_error when the part file cannot be created.
     */
    storage_upload_sink(storage::filesystem::local_store& store,
                        const std::string& bucket,
                        const std::string& key);

    /// The SHA-256 of the bytes taken, hex encoded. Empty until finish().
    [[nodiscard]] const std::string& digest() const noexcept {
        return digest_;
    }

    /// The number of bytes taken.
    [[nodiscard]] std::uint64_t byte_count() const noexcept {
        return byte_count_;
    }

protected:
    bool consume_chunk(std::string_view chunk) override;
    bool finish_body() override;
    bool publish_chunk() override;
    void discard() noexcept override;

private:
    void close_part() noexcept;

    storage::filesystem::local_store& store_;
    std::string bucket_;
    std::string key_;
    std::filesystem::path part_;
    std::ofstream out_;
    std::uint64_t byte_count_ = 0;
    std::string digest_;
    bool kept_ = false;
};

}

#endif
