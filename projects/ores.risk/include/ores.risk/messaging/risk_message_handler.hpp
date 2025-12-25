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
#ifndef ORES_RISK_MESSAGING_RISK_MESSAGE_HANDLER_HPP
#define ORES_RISK_MESSAGING_RISK_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.variability/service/system_flags_service.hpp"
#include "ores.risk/repository/currency_repository.hpp"

namespace ores::risk::messaging {

/**
 * @brief Message handler for risk subsystem messages.
 *
 * Processes messages in the risk subsystem range (0x1000-0x1FFF).
 * Currently handles:
 * - get_currencies_request: Retrieves all currencies from the repository
 */
class risk_message_handler final : public comms::messaging::message_handler {
private:
    inline static std::string_view logger_name =
        "ores.risk.messaging.risk_message_handler";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a risk message handler.
     *
     * @param ctx Database context for repository access
     * @param system_flags Shared system flags service for flag access
     */
    risk_message_handler(database::context ctx,
        std::shared_ptr<variability::service::system_flags_service> system_flags);

    /**
     * @brief Handle a risk subsystem message.
     *
     * @param type The message type (must be in range 0x1000-0x1FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    /**
     * @brief Handle get_currencies_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_get_currencies_request(std::span<const std::byte> payload);

    /**
     * @brief Handle save_currency_request message (create or update).
     *
     * Due to bitemporal storage, both create and update operations
     * result in writing a new record. Database triggers handle temporal
     * versioning automatically.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_save_currency_request(std::span<const std::byte> payload);

    /**
     * @brief Handle delete_currency_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_delete_currency_request(std::span<const std::byte> payload);

    /**
     * @brief Handle get_currency_history_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, comms::messaging::error_code>>
    handle_get_currency_history_request(std::span<const std::byte> payload);

    database::context ctx_;
    std::shared_ptr<variability::service::system_flags_service> system_flags_;
    repository::currency_repository currency_repo_;
};

}

#endif
