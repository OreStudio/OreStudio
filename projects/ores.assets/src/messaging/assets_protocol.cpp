/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.assets/messaging/assets_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

namespace ores::assets::messaging {

using namespace ores::comms::messaging;

// get_currency_images_request

std::vector<std::byte> get_currency_images_request::serialize() const {
    return {};
}

std::expected<get_currency_images_request, comms::messaging::error_code>
get_currency_images_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(comms::messaging::error_code::invalid_request);
    }
    return get_currency_images_request{};
}

std::ostream& operator<<(std::ostream& s, const get_currency_images_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_currency_images_response

std::vector<std::byte> get_currency_images_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(currency_images.size()));

    for (const auto& ci : currency_images) {
        writer::write_string(buffer, ci.iso_code);
        writer::write_string(buffer, ci.image_id);
        writer::write_string(buffer, ci.assigned_by);
        writer::write_string(buffer, ci.assigned_at);
    }

    return buffer;
}

std::expected<get_currency_images_response, comms::messaging::error_code>
get_currency_images_response::deserialize(std::span<const std::byte> data) {
    get_currency_images_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.currency_images.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::currency_image ci;

        auto iso_code_result = reader::read_string(data);
        if (!iso_code_result) return std::unexpected(iso_code_result.error());
        ci.iso_code = *iso_code_result;

        auto image_id_result = reader::read_string(data);
        if (!image_id_result) return std::unexpected(image_id_result.error());
        ci.image_id = *image_id_result;

        auto assigned_by_result = reader::read_string(data);
        if (!assigned_by_result) return std::unexpected(assigned_by_result.error());
        ci.assigned_by = *assigned_by_result;

        auto assigned_at_result = reader::read_string(data);
        if (!assigned_at_result) return std::unexpected(assigned_at_result.error());
        ci.assigned_at = *assigned_at_result;

        response.currency_images.push_back(std::move(ci));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_currency_images_response& v) {
    rfl::json::write(v, s);
    return s;
}

// get_images_request

std::vector<std::byte> get_images_request::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(image_ids.size()));

    for (const auto& id : image_ids) {
        writer::write_string(buffer, id);
    }

    return buffer;
}

std::expected<get_images_request, comms::messaging::error_code>
get_images_request::deserialize(std::span<const std::byte> data) {
    get_images_request request;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    // Enforce maximum batch size
    if (count > MAX_IMAGES_PER_REQUEST) {
        return std::unexpected(comms::messaging::error_code::payload_too_large);
    }

    request.image_ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_string(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.image_ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_images_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_images_response

std::vector<std::byte> get_images_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(images.size()));

    for (const auto& img : images) {
        writer::write_uint32(buffer, static_cast<std::uint32_t>(img.version));
        writer::write_string(buffer, img.image_id);
        writer::write_string(buffer, img.key);
        writer::write_string(buffer, img.description);
        writer::write_string(buffer, img.svg_data);
        writer::write_string(buffer, img.recorded_by);
        writer::write_string(buffer, img.recorded_at);
    }

    return buffer;
}

std::expected<get_images_response, comms::messaging::error_code>
get_images_response::deserialize(std::span<const std::byte> data) {
    get_images_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.images.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::image img;

        auto version_result = reader::read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        img.version = static_cast<int>(*version_result);

        auto image_id_result = reader::read_string(data);
        if (!image_id_result) return std::unexpected(image_id_result.error());
        img.image_id = *image_id_result;

        auto key_result = reader::read_string(data);
        if (!key_result) return std::unexpected(key_result.error());
        img.key = *key_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        img.description = *description_result;

        auto svg_data_result = reader::read_string(data);
        if (!svg_data_result) return std::unexpected(svg_data_result.error());
        img.svg_data = *svg_data_result;

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        img.recorded_by = *recorded_by_result;

        auto recorded_at_result = reader::read_string(data);
        if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
        img.recorded_at = *recorded_at_result;

        response.images.push_back(std::move(img));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_images_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
