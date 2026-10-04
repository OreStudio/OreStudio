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
#include "ores.assets.core/validation/image_upload_validator.hpp"
#include <algorithm>
#include <array>
#include <format>
#include <span>
#include <string_view>

namespace ores::assets::validation {

namespace {

constexpr std::size_t bytes_per_megabyte = 1024 * 1024;

struct image_dimensions {
    std::size_t width = 0;
    std::size_t height = 0;
};

std::uint32_t read_be32(std::span<const std::uint8_t> data, std::size_t offset) {
    return (static_cast<std::uint32_t>(data[offset]) << 24) |
           (static_cast<std::uint32_t>(data[offset + 1]) << 16) |
           (static_cast<std::uint32_t>(data[offset + 2]) << 8) |
           static_cast<std::uint32_t>(data[offset + 3]);
}

std::uint16_t read_be16(std::span<const std::uint8_t> data, std::size_t offset) {
    return static_cast<std::uint16_t>((static_cast<std::uint16_t>(data[offset]) << 8) |
                                      static_cast<std::uint16_t>(data[offset + 1]));
}

std::uint16_t read_le16(std::span<const std::uint8_t> data, std::size_t offset) {
    return static_cast<std::uint16_t>(static_cast<std::uint16_t>(data[offset]) |
                                      (static_cast<std::uint16_t>(data[offset + 1]) << 8));
}

std::uint32_t read_le24(std::span<const std::uint8_t> data, std::size_t offset) {
    return static_cast<std::uint32_t>(data[offset]) |
           (static_cast<std::uint32_t>(data[offset + 1]) << 8) |
           (static_cast<std::uint32_t>(data[offset + 2]) << 16);
}

std::uint32_t read_le32(std::span<const std::uint8_t> data, std::size_t offset) {
    return static_cast<std::uint32_t>(data[offset]) |
           (static_cast<std::uint32_t>(data[offset + 1]) << 8) |
           (static_cast<std::uint32_t>(data[offset + 2]) << 16) |
           (static_cast<std::uint32_t>(data[offset + 3]) << 24);
}

std::optional<image_dimensions> read_png(std::span<const std::uint8_t> data) {
    constexpr std::array<std::uint8_t, 8> signature{0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    // The signature, the IHDR chunk header and its eight-byte body.
    if (data.size() < 24)
        return std::nullopt;
    if (!std::equal(signature.begin(), signature.end(), data.begin()))
        return std::nullopt;
    // The IHDR chunk must come first, and its type sits at bytes 12 to 15.
    if (!(data[12] == 'I' && data[13] == 'H' && data[14] == 'D' && data[15] == 'R'))
        return std::nullopt;
    const auto width = read_be32(data, 16);
    const auto height = read_be32(data, 20);
    if (width == 0 || height == 0)
        return std::nullopt;
    return image_dimensions{width, height};
}

std::optional<image_dimensions> read_jpeg(std::span<const std::uint8_t> data) {
    if (data.size() < 4 || data[0] != 0xFF || data[1] != 0xD8)
        return std::nullopt;
    std::size_t offset = 2;
    while (offset + 1 < data.size()) {
        if (data[offset] != 0xFF)
            return std::nullopt;
        // Markers may pad with any number of 0xFF fill bytes.
        while (offset < data.size() && data[offset] == 0xFF)
            ++offset;
        if (offset >= data.size())
            return std::nullopt;
        const auto marker = data[offset++];
        // Standalone markers carry no payload.
        if (marker == 0x01 || (marker >= 0xD0 && marker <= 0xD7))
            continue;
        // The scan starts before any frame header, so the size is unknown.
        if (marker == 0xD9 || marker == 0xDA)
            return std::nullopt;
        if (offset + 2 > data.size())
            return std::nullopt;
        const auto length = read_be16(data, offset);
        if (length < 2)
            return std::nullopt;
        // SOF0 to SOF15 hold the frame size, except C4, C8 and CC, which are
        // other segments.
        const bool is_sof =
            marker >= 0xC0 && marker <= 0xCF && marker != 0xC4 && marker != 0xC8 && marker != 0xCC;
        if (is_sof) {
            // The length, the sample precision, the height and the width.
            if (offset + 7 > data.size())
                return std::nullopt;
            const auto height = read_be16(data, offset + 3);
            const auto width = read_be16(data, offset + 5);
            if (width == 0 || height == 0)
                return std::nullopt;
            return image_dimensions{width, height};
        }
        if (offset + length > data.size())
            return std::nullopt;
        offset += length;
    }
    return std::nullopt;
}

std::optional<image_dimensions> read_webp(std::span<const std::uint8_t> data) {
    // The RIFF header, the first chunk header and the smallest chunk body.
    if (data.size() < 30)
        return std::nullopt;
    if (!(data[0] == 'R' && data[1] == 'I' && data[2] == 'F' && data[3] == 'F'))
        return std::nullopt;
    if (!(data[8] == 'W' && data[9] == 'E' && data[10] == 'B' && data[11] == 'P'))
        return std::nullopt;
    const auto chunk = std::string_view(reinterpret_cast<const char*>(data.data() + 12), 4);
    std::size_t width = 0;
    std::size_t height = 0;
    if (chunk == "VP8X") {
        // The flags and the reserved bytes, then the canvas width and height,
        // each stated minus one.
        width = read_le24(data, 24) + 1;
        height = read_le24(data, 27) + 1;
    } else if (chunk == "VP8L") {
        // The signature byte, then the width and the height, each stated minus
        // one in fourteen bits.
        if (data[20] != 0x2F)
            return std::nullopt;
        const auto bits = read_le32(data, 21);
        width = (bits & 0x3FFF) + 1;
        height = ((bits >> 14) & 0x3FFF) + 1;
    } else if (chunk == "VP8 ") {
        // The frame tag, then the start code, which marks a key frame.
        if (!(data[23] == 0x9D && data[24] == 0x01 && data[25] == 0x2A))
            return std::nullopt;
        width = read_le16(data, 26) & 0x3FFF;
        height = read_le16(data, 28) & 0x3FFF;
    } else {
        return std::nullopt;
    }
    if (width == 0 || height == 0)
        return std::nullopt;
    return image_dimensions{width, height};
}

using dimension_reader = std::optional<image_dimensions> (*)(std::span<const std::uint8_t>);

struct accepted_format {
    std::string_view mime_type;
    dimension_reader read_dimensions;
};

// The formats an upload accepts, each with the reader that proves the bytes
// carry that format. The policy's format list is built from this table, so the
// formats the server states and the formats it can read cannot drift apart.
constexpr std::array<accepted_format, 3> accepted_formats{{
    {"image/png", read_png},
    {"image/jpeg", read_jpeg},
    {"image/webp", read_webp},
}};

std::string join_choices(const std::vector<std::string>& choices) {
    std::string joined;
    for (std::size_t i = 0; i < choices.size(); ++i) {
        if (i > 0)
            joined += i + 1 == choices.size() ? " or " : ", ";
        joined += choices[i];
    }
    return joined;
}

std::string size_in_words(std::size_t bytes) {
    if (bytes % bytes_per_megabyte == 0)
        return std::format("{} MB", bytes / bytes_per_megabyte);
    return std::format("{} bytes", bytes);
}

}

image_upload_policy image_upload_validator::policy() {
    image_upload_policy rules{
        .max_size_bytes = 2 * bytes_per_megabyte, .min_width = 128, .min_height = 128};
    for (const auto& format : accepted_formats)
        rules.formats.emplace_back(format.mime_type);
    return rules;
}

std::optional<image_upload_refusal>
image_upload_validator::validate(const std::string& mime_type,
                                 const std::vector<std::uint8_t>& data) {
    const auto rules = policy();
    const auto format =
        std::ranges::find_if(accepted_formats, [&mime_type](const accepted_format& entry) {
            return entry.mime_type == mime_type;
        });
    if (format == accepted_formats.end())
        return image_upload_refusal{.code = "unsupported_media_type",
                                    .field = "mime_type",
                                    .message = "The image media type must be one of " +
                                               join_choices(rules.formats) + "."};
    if (data.size() > rules.max_size_bytes)
        return image_upload_refusal{.code = "image_too_large",
                                    .field = "data",
                                    .message = "The image is larger than the " +
                                               size_in_words(rules.max_size_bytes) + " limit."};
    const auto dimensions = format->read_dimensions(data);
    if (!dimensions)
        return image_upload_refusal{.code = "invalid_image",
                                    .field = "data",
                                    .message = "The data is not a readable image of media type " +
                                               mime_type + "."};
    if (dimensions->width < rules.min_width || dimensions->height < rules.min_height)
        return image_upload_refusal{
            .code = "image_too_small",
            .field = "data",
            .message = std::format(
                "The image is smaller than {} by {} pixels.", rules.min_width, rules.min_height)};
    return std::nullopt;
}

}
