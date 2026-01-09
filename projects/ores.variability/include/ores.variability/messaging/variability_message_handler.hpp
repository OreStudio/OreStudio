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
#ifndef ORES_VARIABILITY_MESSAGING_VARIABILITY_MESSAGE_HANDLER_HPP
#define ORES_VARIABILITY_MESSAGING_VARIABILITY_MESSAGE_HANDLER_HPP

#include "ores.comms/messaging/message_handler.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.variability/repository/feature_flags_repository.hpp"

namespace ores::variability::messaging {

/**
 * @brief Message handler for variability subsystem messages.
 *
 * Processes messages in the variability subsystem range (0x3000-0x3FFF).
 *
 * Currently handles:
 *
 * - get_feature_flags_request: Retrieves all feature flags from the repository
 * - save_feature_flag_request: Creates or updates a feature flag
 * - delete_feature_flag_request: Deletes a feature flag by name
 */
class variability_message_handler final : public comms::messaging::message_handler {
private:
    inline static std::string_view logger_name =
        "ores.variability.messaging.variability_message_handler";

   static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a variability message handler.
     *
     * @param ctx Database context for repository access
     */
    explicit variability_message_handler(database::context ctx);

    /**
     * @brief Handle a variability subsystem message.
     *
     * @param type The message type (must be in range 0x3000-0x3FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        [[maybe_unused]] const std::string& remote_address) override;

private:
    /**
     * @brief Handle get_feature_flags_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_feature_flags_request(std::span<const std::byte> payload);

    /**
     * @brief Handle save_feature_flag_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_feature_flag_request(std::span<const std::byte> payload);

    /**
     * @brief Handle delete_feature_flag_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_feature_flag_request(std::span<const std::byte> payload);

    /**
     * @brief Handle get_feature_flag_history_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_feature_flag_history_request(std::span<const std::byte> payload);

    repository::feature_flags_repository feature_flags_repo_;
};

}

#endif
