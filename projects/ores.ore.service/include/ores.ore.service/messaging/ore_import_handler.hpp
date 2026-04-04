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
#ifndef ORES_ORE_SERVICE_MESSAGING_ORE_IMPORT_HANDLER_HPP
#define ORES_ORE_SERVICE_MESSAGING_ORE_IMPORT_HANDLER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"

namespace ores::ore::service::messaging {

/**
 * @brief NATS handler for workflow.v1.ore.import requests.
 *
 * Authenticates the caller JWT, creates a workflow_instance record,
 * drives the ore_import_workflow saga, and replies with
 * ore_import_response.  All log lines carry correlation_id so the full
 * server-side trace for one import can be grepped across services.
 */
class ore_import_handler {
private:
    inline static std::string_view logger_name =
        "ores.ore.service.messaging.ore_import_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    ore_import_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer,
        ores::nats::service::nats_client outbound_nats,
        std::string http_base_url,
        std::string work_dir);

    /**
     * @brief Handles a workflow.v1.ore.import NATS request.
     */
    void ore_import(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    ores::nats::service::nats_client outbound_nats_;
    std::string http_base_url_;
    std::string work_dir_;
};

}

#endif
