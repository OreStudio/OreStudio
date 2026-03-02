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
#ifndef ORES_REPORTING_MESSAGING_REPORTING_MESSAGE_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_REPORTING_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/tenant_aware_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::reporting::messaging {

/**
 * @brief Message handler for reporting subsystem messages.
 *
 * Processes messages in the reporting subsystem range (0xA000-0xAFFF).
 * Handles report types, concurrency policies, report definitions, and
 * report instances (get, save, delete, history).
 */
class reporting_message_handler final
    : public comms::messaging::tenant_aware_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.reporting.messaging.reporting_message_handler");
        return instance;
    }

public:
    reporting_message_handler(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    // Report type handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_report_types_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_report_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_report_type_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_report_type_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Concurrency policy handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_concurrency_policies_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_concurrency_policy_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_concurrency_policy_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_concurrency_policy_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Report definition handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_report_definitions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_report_definition_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_report_definition_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_report_definition_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_schedule_report_definitions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_unschedule_report_definitions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    // Report instance handlers
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_report_instances_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_save_report_instance_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_delete_report_instance_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_report_instance_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);
};

}

#endif
