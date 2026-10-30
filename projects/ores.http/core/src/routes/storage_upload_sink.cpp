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
#include "ores.http.core/routes/storage_upload_sink.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <stdexcept>
#include <system_error>

namespace ores::http::server::routes {

namespace {

/// A part file is written beside its object, so the rename stays on one disk.
std::filesystem::path part_of(const std::filesystem::path& object) {
    auto part = object;
    part += ".part";
    return part;
}

}

storage_upload_sink::storage_upload_sink(const storage::filesystem::local_store& store,
                                         const std::string& bucket,
                                         const std::string& key) {
    const auto object = store.resolve(bucket, key);
    if (!object)
        throw std::invalid_argument("Invalid bucket or key: " + bucket + "/" + key);

    object_ = *object;
    part_ = part_of(object_);

    std::error_code ec;
    std::filesystem::create_directories(object_.parent_path(), ec);
    if (ec)
        throw std::runtime_error("Cannot create bucket directory: " + ec.message());

    out_.open(part_, std::ios::binary | std::ios::trunc);
    if (!out_)
        throw std::runtime_error("Cannot write object: " + part_.string());
}

bool storage_upload_sink::consume_chunk(std::string_view chunk) {
    out_.write(chunk.data(), static_cast<std::streamsize>(chunk.size()));
    if (!out_)
        return false;
    byte_count_ += chunk.size();
    return true;
}

bool storage_upload_sink::finish_body() {
    out_.flush();
    const auto ok = static_cast<bool>(out_);
    close_part();
    if (!ok)
        return false;

    try {
        digest_ = utility::crypto::sha256::hex_digest_of_file(part_);
    } catch (const std::exception&) {
        return false;
    }
    return true;
}

bool storage_upload_sink::publish_chunk() {
    std::error_code ec;
    std::filesystem::rename(part_, object_, ec);
    if (ec)
        return false;
    kept_ = true;
    return true;
}

void storage_upload_sink::discard() noexcept {
    close_part();
    if (kept_)
        return;

    std::error_code ec;
    std::filesystem::remove(part_, ec);
}

void storage_upload_sink::close_part() noexcept {
    if (!out_.is_open())
        return;
    std::error_code ec;
    out_.close();
}

}
