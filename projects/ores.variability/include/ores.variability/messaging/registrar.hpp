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
#ifndef ORES_VARIABILITY_MESSAGING_REGISTRAR_HPP
#define ORES_VARIABILITY_MESSAGING_REGISTRAR_HPP

#include "ores.comms/net/server.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.database/context.hpp"

namespace ores::variability::messaging {

/**
 * @brief Register variability subsystem message handlers with the server.
 *
 * Registers handlers for all variability subsystem messages (0x3000-0x3FFF).
 * Must be called before server.run().
 *
 * @param server The server to register handlers with
 * @param ctx Database context for repository access
 */
class registrar {
private:
    inline static std::string_view logger_name =
        "ores.variability.messaging.registrar";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static void register_handlers(comms::net::server& server,
        database::context ctx);
};

}

#endif
