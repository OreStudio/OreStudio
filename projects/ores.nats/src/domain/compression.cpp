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
#include "ores.nats/domain/compression.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.utility/compression/gzip.hpp"

namespace ores::nats {

std::vector<std::byte>
compress_if_worthwhile(std::span<const std::byte> data,
                       std::unordered_map<std::string, std::string>& headers) {
    if (data.size() < compression_threshold_bytes ||
        headers.contains(std::string(headers::x_content_encoding)))
        return {data.begin(), data.end()};

    const std::span<const char> as_chars(reinterpret_cast<const char*>(data.data()), data.size());
    auto compressed = ores::utility::compression::gzip_compress(as_chars);
    if (compressed.size() >= data.size())
        return {data.begin(), data.end()};

    headers[std::string(headers::x_content_encoding)] = std::string(headers::content_encoding_gzip);
    const auto* p = reinterpret_cast<const std::byte*>(compressed.data());
    return {p, p + compressed.size()};
}

std::vector<std::byte>
decompress_if_flagged(std::span<const std::byte> data,
                      const std::unordered_map<std::string, std::string>& headers) {
    const auto it = headers.find(std::string(headers::x_content_encoding));
    if (it == headers.end() || it->second != headers::content_encoding_gzip)
        return {data.begin(), data.end()};

    const std::span<const char> as_chars(reinterpret_cast<const char*>(data.data()), data.size());
    auto decompressed = ores::utility::compression::gzip_decompress(as_chars);
    const auto* p = reinterpret_cast<const std::byte*>(decompressed.data());
    return {p, p + decompressed.size()};
}

} // namespace ores::nats
