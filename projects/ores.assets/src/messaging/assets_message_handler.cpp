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
#include "ores.assets/messaging/assets_message_handler.hpp"

#include "ores.assets/messaging/assets_protocol.hpp"

namespace ores::assets::messaging {

using namespace ores::telemetry::log;

assets_message_handler::assets_message_handler(database::context ctx)
    : ctx_(std::move(ctx)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
assets_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, [[maybe_unused]] const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling assets message type " << type;

    switch (type) {
    case comms::messaging::message_type::get_currency_images_request:
        co_return co_await handle_get_currency_images_request(payload);
    case comms::messaging::message_type::get_images_request:
        co_return co_await handle_get_images_request(payload);
    case comms::messaging::message_type::list_images_request:
        co_return co_await handle_list_images_request(payload);
    case comms::messaging::message_type::set_currency_image_request:
        co_return co_await handle_set_currency_image_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown assets message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::messaging::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
assets_message_handler::
handle_get_currency_images_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_currency_images_request.";

    // Deserialize request
    auto request_result = get_currency_images_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_currency_images_request";
        co_return std::unexpected(request_result.error());
    }

    get_currency_images_response response;
    try {
        // Read all currency-image mappings from repository
        response.currency_images = currency_image_repo_.read_latest(ctx_);

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.currency_images.size()
                                  << " currency-image mappings";

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error reading currency images: " << e.what();
        co_return std::unexpected(comms::messaging::error_code::database_error);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
assets_message_handler::
handle_get_images_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_images_request.";

    // Deserialize request
    auto request_result = get_images_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_images_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Requested " << request.image_ids.size() << " images";

    get_images_response response;
    try {
        // Read images by IDs from repository
        response.images = image_repo_.read_latest_by_ids(ctx_, request.image_ids);

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.images.size()
                                  << " images";

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error reading images: " << e.what();
        co_return std::unexpected(comms::messaging::error_code::database_error);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
assets_message_handler::
handle_list_images_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_images_request.";

    // Deserialize request
    auto request_result = list_images_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_images_request";
        co_return std::unexpected(request_result.error());
    }

    list_images_response response;
    try {
        // Read all images from repository
        auto images = image_repo_.read_latest(ctx_);

        // Convert to image_info (without SVG data)
        response.images.reserve(images.size());
        for (const auto& img : images) {
            response.images.push_back({
                .image_id = img.image_id,
                .key = img.key,
                .description = img.description
            });
        }

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.images.size()
                                  << " image infos";

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error listing images: " << e.what();
        co_return std::unexpected(comms::messaging::error_code::database_error);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
assets_message_handler::
handle_set_currency_image_request(std::span<const std::byte> payload) {
    BOOST_LOG_SEV(lg(), debug) << "Processing set_currency_image_request.";

    // Deserialize request
    auto request_result = set_currency_image_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize set_currency_image_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Setting image for currency " << request.iso_code
                               << " to " << (request.image_id.empty() ? "(none)" : request.image_id);

    set_currency_image_response response;
    try {
        if (request.image_id.empty()) {
            // Remove the currency-image mapping
            currency_image_repo_.remove(ctx_, request.iso_code);
            BOOST_LOG_SEV(lg(), info) << "Removed image mapping for currency "
                                      << request.iso_code;
            response.success = true;
            response.message = "Image removed from currency";
        } else {
            // Set the currency-image mapping
            domain::currency_image mapping;
            mapping.iso_code = request.iso_code;
            mapping.image_id = request.image_id;
            mapping.assigned_by = request.assigned_by;
            // assigned_at will be set by the database

            currency_image_repo_.write(ctx_, mapping);
            BOOST_LOG_SEV(lg(), info) << "Set image " << request.image_id
                                      << " for currency " << request.iso_code;
            response.success = true;
            response.message = "Image assigned to currency";
        }

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error setting currency image: " << e.what();
        response.success = false;
        response.message = e.what();
        co_return response.serialize();
    }
}

}
