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
#ifndef ORES_SYNTHETIC_MESSAGING_SYNTHETIC_MESSAGE_HANDLER_HPP
#define ORES_SYNTHETIC_MESSAGING_SYNTHETIC_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.comms/messaging/tenant_aware_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::synthetic::messaging {

/**
 * @brief Message handler for Synthetic subsystem messages (0x7000-0x7FFF).
 *
 * Handles organisation generation requests.
 */
class synthetic_message_handler final
    : public comms::messaging::tenant_aware_handler {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.synthetic.messaging.synthetic_message_handler");
        return instance;
    }

public:
    synthetic_message_handler(database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions,
        std::shared_ptr<iam::service::authorization_service> auth_service);

    using handler_result = boost::asio::awaitable<
        std::expected<std::vector<std::byte>,
                      ores::utility::serialization::error_code>
    >;

    handler_result
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    using auth_check_result = std::expected<
        comms::service::session_info,
        ores::utility::serialization::error_code
    >;

    auth_check_result check_authorization(
        const std::string& remote_address,
        std::string_view permission,
        std::string_view operation_name);

    handler_result handle_generate_organisation_request(
        std::span<const std::byte> payload,
        const std::string& remote_address);

    std::shared_ptr<iam::service::authorization_service> auth_service_;
};

}

#endif
