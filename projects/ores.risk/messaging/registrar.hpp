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
#ifndef ORES_RISK_MESSAGING_REGISTRAR_HPP
#define ORES_RISK_MESSAGING_REGISTRAR_HPP

#include "ores.comms/server.hpp"
#include "ores.utility/log/logger.hpp"
#include "ores.utility/repository/context.hpp"

namespace ores::risk::messaging {

/**
 * @brief Register risk subsystem message handlers with the server.
 *
 * Registers handlers for all risk subsystem messages (0x1000-0x1FFF).
 * Must be called before server.run().
 *
 * @param server The server to register handlers with
 * @param ctx Database context for repository access
 */
class registrar {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static logger instance = logger_factory(
            "ores.risk.messaging.registrar");
        return instance;
    }

public:
    static void register_handlers(comms::server& server,
        utility::repository::context ctx);
};

}

#endif
