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

#include "ores.comms/protocol/message_handler.hpp"
#include "ores.risk/repository/context.hpp"
#include "ores.risk/repository/currency_repository.hpp"

namespace ores::risk::messaging {

/**
 * @brief Message handler for risk subsystem messages.
 *
 * Processes messages in the risk subsystem range (0x1000-0x1FFF).
 * Currently handles:
 * - get_currencies_request: Retrieves all currencies from the repository
 */
class risk_message_handler final : public comms::protocol::message_handler {
public:
    /**
     * @brief Construct a risk message handler.
     *
     * @param ctx Database context for repository access
     */
    explicit risk_message_handler(repository::context ctx);

    /**
     * @brief Handle a risk subsystem message.
     *
     * @param type The message type (must be in range 0x1000-0x1FFF)
     * @param payload The message payload
     * @return Expected containing response payload, or error code
     */
    boost::asio::awaitable<std::expected<std::vector<std::uint8_t>, comms::protocol::error_code>>
    handle_message(comms::protocol::message_type type,
        std::span<const std::uint8_t> payload) override;

private:
    /**
     * @brief Handle get_currencies_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::uint8_t>, comms::protocol::error_code>>
    handle_get_currencies_request(std::span<const std::uint8_t> payload);

    repository::context ctx_;
    repository::currency_repository currency_repo_;
};

}

#endif
