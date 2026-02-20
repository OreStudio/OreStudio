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
#ifndef ORES_TRADE_MESSAGING_TRADE_MESSAGE_HANDLER_HPP
#define ORES_TRADE_MESSAGING_TRADE_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/tenant_aware_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::trade::messaging {

/**
 * @brief Message handler for trade subsystem messages.
 *
 * Processes messages in the trade subsystem range (0x8000-0x8FFF).
 * Handles:
 * - Trade type CRUD + history
 * - Lifecycle event CRUD + history
 * - Party role type CRUD + history
 * - Trade ID type CRUD + history
 * - Trade CRUD + history
 * - Trade identifier CRUD + history
 * - Trade party role CRUD + history
 */
class trade_message_handler final : public comms::messaging::tenant_aware_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.trade.messaging.trade_message_handler");
        return instance;
    }

public:
    trade_message_handler(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    // Trade type handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_types_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_trade_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_trade_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_type_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Lifecycle event handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_lifecycle_events_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_lifecycle_event_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_lifecycle_event_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_lifecycle_event_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Party role type handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_party_role_types_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_party_role_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_party_role_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_party_role_type_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Trade ID type handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_id_types_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_trade_id_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_trade_id_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_id_type_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Trade handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trades_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_trade_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_trade_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Trade identifier handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_identifiers_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_trade_identifier_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_trade_identifier_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_identifier_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Trade party role handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_party_roles_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_trade_party_role_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_trade_party_role_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_trade_party_role_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);
};

}

#endif
