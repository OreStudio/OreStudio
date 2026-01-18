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
#ifndef ORES_DQ_MESSAGING_REGISTRAR_HPP
#define ORES_DQ_MESSAGING_REGISTRAR_HPP

#include "ores.comms/net/server.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::dq::messaging {

/**
 * @brief Register Data Quality subsystem message handlers with the server.
 *
 * Registers handlers for all DQ subsystem messages (0x6000-0x6FFF).
 * Must be called before server.run().
 *
 * @param server The server to register handlers with
 * @param ctx Database context for repository access
 */
class registrar {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.dq.messaging.registrar");
        return instance;
    }

public:
    static void register_handlers(comms::net::server& server,
        database::context ctx);
};

}

#endif
