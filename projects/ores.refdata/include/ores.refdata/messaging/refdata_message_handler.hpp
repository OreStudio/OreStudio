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
#ifndef ORES_REFDATA_MESSAGING_REFDATA_MESSAGE_HANDLER_HPP
#define ORES_REFDATA_MESSAGING_REFDATA_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/tenant_aware_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.variability/service/system_flags_service.hpp"

namespace ores::refdata::messaging {

/**
 * @brief Message handler for refdata subsystem messages.
 *
 * Processes messages in the refdata subsystem range (0x1000-0x1FFF).
 * Currently handles:
 * - Currency CRUD + history
 * - Country CRUD + history
 * - Business centre CRUD + history
 * - Party type CRUD + history
 * - Party status CRUD + history
 * - Party ID scheme CRUD + history
 * - Contact type CRUD + history
 * - Party CRUD + history
 * - Counterparty CRUD + history
 * - Business unit CRUD + history
 * - Portfolio CRUD + history
 * - Book CRUD + history
 */
class refdata_message_handler final : public comms::messaging::tenant_aware_handler {
private:
    inline static std::string_view logger_name =
        "ores.refdata.messaging.refdata_message_handler";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a refdata message handler.
     *
     * @param ctx Database context for repository access
     * @param system_flags Shared system flags service for flag access
     * @param sessions Session service for authentication verification
     */
    refdata_message_handler(database::context ctx,
        std::shared_ptr<variability::service::system_flags_service> system_flags,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    /**
     * @brief Handle a refdata subsystem message.
     *
     * @param type The message type (must be in range 0x1000-0x1FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    /**
     * @brief Handle get_currencies_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_currencies_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle save_currency_request message (create or update).
     *
     * Due to bitemporal storage, both create and update operations
     * result in writing a new record. Database triggers handle temporal
     * versioning automatically.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_currency_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_currency_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_currency_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_currency_history_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_currency_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_countries_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_countries_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle save_country_request message (create or update).
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_country_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle delete_country_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_country_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_country_history_request message.
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_country_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Party type handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_party_types_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_party_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_party_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_party_type_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Party status handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_party_statuses_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_party_status_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_party_status_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_party_status_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Party ID scheme handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_party_id_schemes_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_party_id_scheme_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_party_id_scheme_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_party_id_scheme_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Contact type handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_contact_types_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_contact_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_contact_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_contact_type_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Party handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_parties_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_party_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_party_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_party_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Business centre handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_business_centres_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_business_centre_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_business_centre_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_business_centre_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Counterparty handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_counterparties_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_counterparty_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_counterparty_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_counterparty_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Business unit handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_business_units_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_business_unit_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_business_unit_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_business_unit_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Portfolio handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_portfolios_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_portfolio_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_portfolio_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_portfolio_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Book handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_books_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_save_book_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_delete_book_request(std::span<const std::byte> payload,
        const std::string& remote_address);
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_book_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    std::shared_ptr<variability::service::system_flags_service> system_flags_;
};

}

#endif
