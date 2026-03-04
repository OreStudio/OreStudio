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
#ifndef ORES_MQ_MESSAGING_MQ_MESSAGE_HANDLER_HPP
#define ORES_MQ_MESSAGING_MQ_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/tenant_aware_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::mq::messaging {

/**
 * @brief Message handler for the MQ subsystem messages.
 *
 * Processes messages in the MQ subsystem range (0xB000-0xBFFF).
 * Handles queue management (create, drop, purge) and message operations
 * (send, read, pop, ack, nack) by delegating to service::mq_service.
 */
class mq_message_handler final
    : public comms::messaging::tenant_aware_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.mq.messaging.mq_message_handler");
        return instance;
    }

public:
    mq_message_handler(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_queues_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_queue_stats_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_queue_stats_samples_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_create_queue_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_drop_queue_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_purge_queue_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_send_message_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_read_messages_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_pop_messages_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_ack_messages_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_nack_message_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_messages_request(std::span<const std::byte> payload,
        const std::string& remote_address);
};

}

#endif
