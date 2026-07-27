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
#ifndef ORES_NATS_DOMAIN_COMPRESSION_HPP
#define ORES_NATS_DOMAIN_COMPRESSION_HPP

#include "ores.nats/export.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::nats {

/**
 * @brief Below this size, gzip is skipped even if requested.
 *
 * Gzip's fixed framing overhead (~20 bytes) and the CPU cost of
 * compressing/decompressing make it not worth it for small payloads. The
 * overwhelming majority of NATS traffic (small request/response JSON)
 * stays under this threshold and is sent as-is, with no compression header.
 * Payloads at or above it (e.g. an image-carrying response) are
 * compressed transparently. See the "Fix image-batch NATS payload
 * overflow" story's decision task for the data behind treating
 * compression as sufficient without also needing a non-JSON wire encoding
 * for large payloads.
 */
inline constexpr std::size_t compression_threshold_bytes = 4096;

/**
 * @brief Gzip-compresses @p data and marks @p headers accordingly, if and
 * only if doing so is worthwhile.
 *
 * Skips compression (returning a copy of @p data unchanged) when:
 * - @p data is smaller than compression_threshold_bytes, or
 * - @p headers already carries a content-encoding header (never overrides
 *   a caller's explicit choice), or
 * - compressing wouldn't actually shrink the payload (rare past the
 *   threshold, but a free safety net).
 *
 * On success, sets headers[x_content_encoding] = content_encoding_gzip so
 * the receiving end's decompress_if_flagged() knows to reverse it.
 *
 * @param data    Raw bytes to conditionally compress.
 * @param headers Outbound message headers, mutated in place when
 *                compression is applied.
 * @return The bytes that should actually be sent on the wire.
 */
[[nodiscard]] ORES_NATS_EXPORT std::vector<std::byte>
compress_if_worthwhile(std::span<const std::byte> data,
                       std::unordered_map<std::string, std::string>& headers);

/**
 * @brief Reverses compress_if_worthwhile() based on @p headers.
 *
 * Returns a copy of @p data unchanged if @p headers does not carry the
 * gzip content-encoding marker; otherwise gunzips it. Called
 * unconditionally by every inbound message path (ores::nats::message
 * construction from a raw NATS message) so every consumer -- server
 * handlers and client callers alike -- always sees the original,
 * uncompressed bytes regardless of how the sender decided to transmit
 * them.
 *
 * @param data    Message payload as received off the wire.
 * @param headers The same message's headers.
 * @return The original, decompressed bytes.
 * @throws std::runtime_error if the header claims gzip but the payload is
 *         not valid gzip data.
 */
[[nodiscard]] ORES_NATS_EXPORT std::vector<std::byte>
decompress_if_flagged(std::span<const std::byte> data,
                      const std::unordered_map<std::string, std::string>& headers);

} // namespace ores::nats

#endif
