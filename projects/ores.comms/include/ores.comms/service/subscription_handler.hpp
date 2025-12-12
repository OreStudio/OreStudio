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
#ifndef ORES_COMMS_SERVICE_SUBSCRIPTION_HANDLER_HPP
#define ORES_COMMS_SERVICE_SUBSCRIPTION_HANDLER_HPP

#include <memory>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/service/subscription_manager.hpp"

namespace ores::comms::service {

/**
 * @brief Message handler for subscription protocol messages.
 *
 * Handles subscribe_request and unsubscribe_request messages from clients,
 * delegating to the subscription_manager to track subscriptions.
 *
 * Note: notification messages are server-initiated and are sent via the
 * subscription_manager's notify() method, not through this handler.
 */
class subscription_handler final : public messaging::message_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.comms.service.subscription_handler");
        return instance;
    }

public:
    /**
     * @brief Construct a subscription handler.
     *
     * @param manager Shared subscription manager for tracking subscriptions.
     */
    explicit subscription_handler(
        std::shared_ptr<subscription_manager> manager);

    /**
     * @brief Handle a subscription protocol message.
     *
     * @param type The message type (subscribe_request or unsubscribe_request).
     * @param payload The message payload.
     * @param remote_address The remote endpoint address of the client.
     * @return Expected containing response payload, or error code.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, messaging::error_code>>
    handle_message(messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    /**
     * @brief Handle subscribe_request message.
     */
    std::expected<std::vector<std::byte>, messaging::error_code>
    handle_subscribe_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle unsubscribe_request message.
     */
    std::expected<std::vector<std::byte>, messaging::error_code>
    handle_unsubscribe_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    std::shared_ptr<subscription_manager> manager_;
};

}

#endif
