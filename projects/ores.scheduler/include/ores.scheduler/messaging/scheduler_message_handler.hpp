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
#ifndef ORES_SCHEDULER_MESSAGING_SCHEDULER_MESSAGE_HANDLER_HPP
#define ORES_SCHEDULER_MESSAGING_SCHEDULER_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/tenant_aware_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::scheduler::messaging {

/**
 * @brief Message handler for scheduler subsystem messages.
 *
 * Processes messages in the scheduler subsystem range (0x9000-0x9FFF).
 * Handles:
 * - Job definition listing (get_job_definitions)
 * - Job scheduling (schedule_job)
 * - Job unscheduling (unschedule_job)
 * - Job execution history (get_job_history)
 */
class scheduler_message_handler final
    : public comms::messaging::tenant_aware_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.scheduler.messaging.scheduler_message_handler");
        return instance;
    }

public:
    scheduler_message_handler(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_job_definitions_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_schedule_job_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_unschedule_job_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_get_job_history_request(std::span<const std::byte> payload,
        const std::string& remote_address);
};

}

#endif
