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
#ifndef ORES_ASSETS_MESSAGING_ASSETS_MESSAGE_HANDLER_HPP
#define ORES_ASSETS_MESSAGING_ASSETS_MESSAGE_HANDLER_HPP

#include "ores.comms/messaging/message_handler.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.assets/repository/image_repository.hpp"

namespace ores::assets::messaging {

/**
 * @brief Message handler for assets subsystem messages.
 *
 * Processes messages in the assets subsystem range (0x4000-0x4FFF).
 *
 * Currently handles:
 *
 * - get_images_request: Retrieves images by their IDs (batched)
 * - list_images_request: Lists all available images (metadata only)
 */
class assets_message_handler final : public comms::messaging::message_handler {
private:
    inline static std::string_view logger_name =
        "ores.assets.messaging.assets_message_handler";

   static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct an assets message handler.
     *
     * @param ctx Database context for repository access
     */
    explicit assets_message_handler(database::context ctx);

    /**
     * @brief Handle an assets subsystem message.
     *
     * @param type The message type (must be in range 0x4000-0x4FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        [[maybe_unused]] const std::string& remote_address) override;

private:
    /**
     * @brief Handle get_images_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_get_images_request(std::span<const std::byte> payload);

    /**
     * @brief Handle list_images_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_list_images_request(std::span<const std::byte> payload);

    database::context ctx_;
    repository::image_repository image_repo_;
};

}

#endif
