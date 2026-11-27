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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.platform/time/zone_offset.hpp"
#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iterator>
#include <string>
#include <vector>

namespace ores::platform::time {

namespace {

using bytes = std::vector<unsigned char>;

/// A TZif header. The counts are the ones the rest of the file is sized by.
struct header final {
    char version;
    std::uint32_t isutcnt;
    std::uint32_t isstdcnt;
    std::uint32_t leapcnt;
    std::uint32_t timecnt;
    std::uint32_t typecnt;
    std::uint32_t charcnt;
};

/// A zone type: the offset it keeps, and whether it is a daylight saving one.
struct zone_type final {
    std::int32_t utoff;
    bool isdst;
};

std::uint32_t read_be32(const unsigned char* at) {
    return (static_cast<std::uint32_t>(at[0]) << 24) | (static_cast<std::uint32_t>(at[1]) << 16) |
           (static_cast<std::uint32_t>(at[2]) << 8) | static_cast<std::uint32_t>(at[3]);
}

std::int64_t read_be64(const unsigned char* at) {
    return (static_cast<std::int64_t>(read_be32(at)) << 32) |
           static_cast<std::int64_t>(read_be32(at + 4));
}

constexpr std::size_t header_size = 44;

std::optional<header> read_header(const bytes& data, std::size_t at) {
    if (data.size() < at + header_size)
        return std::nullopt;
    if (std::memcmp(data.data() + at, "TZif", 4) != 0)
        return std::nullopt;
    header h{};
    h.version = static_cast<char>(data[at + 4]);
    h.isutcnt = read_be32(data.data() + at + 20);
    h.isstdcnt = read_be32(data.data() + at + 24);
    h.leapcnt = read_be32(data.data() + at + 28);
    h.timecnt = read_be32(data.data() + at + 32);
    h.typecnt = read_be32(data.data() + at + 36);
    h.charcnt = read_be32(data.data() + at + 40);
    if (h.typecnt == 0)
        return std::nullopt;
    return h;
}

/// The bytes the block of a header occupies, excluding the header itself.
std::size_t block_size(const header& h, std::size_t time_size) {
    return h.timecnt * time_size + h.timecnt + h.typecnt * 6 + h.charcnt +
           h.leapcnt * (time_size + 4) + h.isstdcnt + h.isutcnt;
}

std::optional<bytes> read_zone_file(std::string_view iana_zone) {
    if (iana_zone.empty() || iana_zone.front() == '/')
        return std::nullopt;
    /// The database is a directory tree, so a name that climbs it names a file
    /// the caller did not ask for.
    if (iana_zone.find("..") != std::string_view::npos)
        return std::nullopt;

    std::string root = "/usr/share/zoneinfo";
    if (const char* from_env = std::getenv("TZDIR"); from_env != nullptr && *from_env != '\0')
        root = from_env;

    std::ifstream in(root + "/" + std::string(iana_zone), std::ios::binary);
    if (!in)
        return std::nullopt;
    return bytes(std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>());
}

}

std::optional<std::chrono::seconds> zone_offset_at(std::string_view iana_zone,
                                                   std::chrono::system_clock::time_point instant) {
    const auto data = read_zone_file(iana_zone);
    if (!data)
        return std::nullopt;

    auto h = read_header(*data, 0);
    if (!h)
        return std::nullopt;

    std::size_t at = header_size;
    std::size_t time_size = 4;
    /// A version 2 file repeats the whole description with 64-bit times, which
    /// is the one to read: the 32-bit block cannot describe an instant past 2038.
    if (h->version >= '2') {
        at += block_size(*h, time_size);
        auto wide = read_header(*data, at);
        if (!wide)
            return std::nullopt;
        h = wide;
        at += header_size;
        time_size = 8;
    }

    if (data->size() < at + block_size(*h, time_size))
        return std::nullopt;

    const auto type_count = static_cast<std::size_t>(h->typecnt);
    const auto time_count = static_cast<std::size_t>(h->timecnt);
    const auto types_at = at + time_count * time_size + time_count;
    std::vector<zone_type> types;
    types.reserve(type_count);
    for (std::size_t i = 0; i < type_count; ++i) {
        const auto* entry = data->data() + types_at + i * 6;
        types.push_back(zone_type{static_cast<std::int32_t>(read_be32(entry)), entry[4] != 0});
    }

    const auto transition_at = [&](std::size_t i) -> std::int64_t {
        const auto* entry = data->data() + at + i * time_size;
        if (time_size == 8)
            return read_be64(entry);
        return static_cast<std::int64_t>(static_cast<std::int32_t>(read_be32(entry)));
    };

    const auto seconds =
        std::chrono::duration_cast<std::chrono::seconds>(instant.time_since_epoch()).count();

    /// The type in force is the one the last transition at or before the instant
    /// names. Before the first transition a file describes, the first type that
    /// is not a daylight saving one applies.
    auto chosen = types.front();
    if (time_count == 0 || seconds < transition_at(0)) {
        const auto standard =
            std::find_if(types.begin(), types.end(), [](const zone_type& t) { return !t.isdst; });
        if (standard != types.end())
            chosen = *standard;
    } else {
        std::size_t low = 0;
        std::size_t high = time_count;
        while (low + 1 < high) {
            const auto middle = low + (high - low) / 2;
            if (transition_at(middle) <= seconds)
                low = middle;
            else
                high = middle;
        }
        const auto index = static_cast<std::size_t>(data->at(at + time_count * time_size + low));
        if (index >= types.size())
            return std::nullopt;
        chosen = types[index];
    }

    return std::chrono::seconds{chosen.utoff};
}

}
