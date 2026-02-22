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
#include "ores.iam/messaging/session_samples_protocol.hpp"

#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::iam::messaging {

using ores::utility::serialization::reader;
using ores::utility::serialization::writer;
using ores::utility::serialization::error_code;

// ---------------------------------------------------------------------------
// get_session_samples_request
// ---------------------------------------------------------------------------

std::vector<std::byte> get_session_samples_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, session_id);
    return buffer;
}

std::expected<get_session_samples_request, error_code>
get_session_samples_request::deserialize(std::span<const std::byte> data) {
    get_session_samples_request r;

    auto id = reader::read_uuid(data);
    if (!id) return std::unexpected(id.error());
    r.session_id = *id;

    return r;
}

// ---------------------------------------------------------------------------
// get_session_samples_response
// ---------------------------------------------------------------------------

std::vector<std::byte> get_session_samples_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(samples.size()));
    for (const auto& s : samples) {
        writer::write_uint64(buffer, s.sample_time_ms);
        writer::write_uint64(buffer, s.bytes_sent);
        writer::write_uint64(buffer, s.bytes_received);
        writer::write_uint64(buffer, s.latency_ms);
    }
    return buffer;
}

std::expected<get_session_samples_response, error_code>
get_session_samples_response::deserialize(std::span<const std::byte> data) {
    get_session_samples_response r;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    r.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    r.message = *message_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    r.samples.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        session_sample_dto s;

        auto ts = reader::read_uint64(data);
        if (!ts) return std::unexpected(ts.error());
        s.sample_time_ms = *ts;

        auto bs = reader::read_uint64(data);
        if (!bs) return std::unexpected(bs.error());
        s.bytes_sent = *bs;

        auto br = reader::read_uint64(data);
        if (!br) return std::unexpected(br.error());
        s.bytes_received = *br;

        auto lat = reader::read_uint64(data);
        if (!lat) return std::unexpected(lat.error());
        s.latency_ms = *lat;

        r.samples.push_back(std::move(s));
    }

    return r;
}

}
