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
#ifndef ORES_TELEMETRY_MESSAGING_REGISTRAR_HPP
#define ORES_TELEMETRY_MESSAGING_REGISTRAR_HPP

#include <memory>
#include "ores.comms/net/server.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::telemetry::messaging {

/**
 * @brief Register telemetry subsystem message handlers with the server.
 *
 * Registers handlers for all telemetry subsystem messages (0x5000-0x5FFF).
 * Must be called before server.run().
 *
 * The telemetry subsystem handles:
 * - submit_log_records_request (0x5000): Persist client log batches
 * - get_telemetry_logs_request (0x5010): Query raw log entries
 * - get_telemetry_stats_request (0x5020): Query aggregated statistics
 */
class registrar {
private:
    inline static std::string_view logger_name =
        "ores.telemetry.messaging.registrar";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register telemetry message handlers with the server.
     *
     * @param server The server to register handlers with
     * @param ctx Database context for telemetry repository access
     * @param sessions Shared auth session service for session/account lookup
     */
    static void register_handlers(comms::net::server& server,
        database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);
};

}

#endif
