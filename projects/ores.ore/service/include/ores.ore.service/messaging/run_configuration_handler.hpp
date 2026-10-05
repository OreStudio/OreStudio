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
#ifndef ORES_ORE_SERVICE_MESSAGING_RUN_CONFIGURATION_HANDLER_HPP
#define ORES_ORE_SERVICE_MESSAGING_RUN_CONFIGURATION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <string>

namespace ores::ore::service::messaging {

/**
 * @brief Serves the client side of a run's configuration: starting an import
 * and exporting a report definition's ORE input.
 *
 * The import starts the run_configuration_import_workflow and answers with
 * its instance id. The export reads the documents through their owners under
 * the caller's delegated session and answers with the files.
 */
class run_configuration_handler {
private:
    inline static std::string_view logger_name =
        "ores.ore.service.messaging.run_configuration_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    run_configuration_handler(ores::nats::service::client& nats,
                              ores::database::context ctx,
                              ores::security::jwt::jwt_authenticator signer,
                              ores::nats::service::nats_client outbound_nats);

    void import_run_configuration(ores::nats::message msg);
    void export_run_configuration(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    ores::nats::service::nats_client outbound_nats_;
};

/**
 * @brief Runs the run_configuration_import_workflow's step and its
 * compensation, as the caller whose token the workflow carries.
 */
class run_configuration_import_handler {
private:
    inline static std::string_view logger_name =
        "ores.ore.service.messaging.run_configuration_import_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    run_configuration_import_handler(ores::nats::service::client& nats,
                                     ores::nats::service::nats_client outbound_nats);

    void execute(ores::nats::message msg);
    void rollback(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    ores::nats::service::nats_client outbound_nats_;
};

}

#endif
