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

#include <boost/uuid/uuid_io.hpp>
#include "ores.assets/messaging/assets_protocol.hpp"

namespace ores::assets::messaging {

using namespace ores::logging;

assets_message_handler::assets_message_handler(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : ctx_(std::move(ctx)), sessions_(std::move(sessions)) {}

database::context assets_message_handler::make_request_context(
    const comms::service::session_info& session) const {
    return ctx_.with_tenant(boost::uuids::to_string(session.tenant_id));
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
assets_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling assets message type " << type;

    switch (type) {
    case comms::messaging::message_type::get_images_request:
        co_return co_await handle_get_images_request(payload, remote_address);
    case comms::messaging::message_type::list_images_request:
        co_return co_await handle_list_images_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown assets message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
assets_message_handler::
handle_get_images_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing get_images_request.";

    // Require authentication
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "Get images denied: no active session for "
                                  << remote_address;
        co_return std::unexpected(ores::utility::serialization::error_code::authentication_failed);
    }

    // Deserialize request
    auto request_result = get_images_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_images_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Requested " << request.image_ids.size() << " images";

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);

    get_images_response response;
    try {
        // Read images by IDs from repository
        response.images = image_repo_.read_latest_by_ids(ctx, request.image_ids);

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.images.size()
                                  << " images";

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error reading images: " << e.what();
        co_return std::unexpected(ores::utility::serialization::error_code::database_error);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
assets_message_handler::
handle_list_images_request(std::span<const std::byte> payload,
    const std::string& remote_address) {
    BOOST_LOG_SEV(lg(), debug) << "Processing list_images_request.";

    // Require authentication
    auto session = sessions_->get_session(remote_address);
    if (!session) {
        BOOST_LOG_SEV(lg(), warn) << "List images denied: no active session for "
                                  << remote_address;
        co_return std::unexpected(ores::utility::serialization::error_code::authentication_failed);
    }

    // Deserialize request
    auto request_result = list_images_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize list_images_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Create per-request context with session's tenant
    auto ctx = make_request_context(*session);

    list_images_response response;
    try {
        // Read images from repository - filtered if modified_since is set
        std::vector<domain::image> images;
        if (request.modified_since) {
            BOOST_LOG_SEV(lg(), debug) << "Filtering images modified since timestamp";
            images = image_repo_.read_latest_since(ctx, *request.modified_since);
        } else {
            images = image_repo_.read_latest(ctx);
        }

        // Convert to image_info (without SVG data)
        response.images.reserve(images.size());
        for (const auto& img : images) {
            response.images.push_back({
                .image_id = boost::uuids::to_string(img.image_id),
                .key = img.key,
                .description = img.description,
                .recorded_at = img.recorded_at
            });
        }

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.images.size()
                                  << " image infos";

        // Serialize response
        co_return response.serialize();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Database error listing images: " << e.what();
        co_return std::unexpected(ores::utility::serialization::error_code::database_error);
    }
}

}
