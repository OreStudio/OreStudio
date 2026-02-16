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
#ifndef ORES_SYNTHETIC_MESSAGING_REGISTRAR_HPP
#define ORES_SYNTHETIC_MESSAGING_REGISTRAR_HPP

#include "ores.comms/net/server.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/service/authorization_service.hpp"

namespace ores::synthetic::messaging {

/**
 * @brief Register Synthetic subsystem message handlers with the server.
 *
 * Registers handlers for all Synthetic subsystem messages (0x7000-0x7FFF).
 * Must be called before server.run().
 */
class registrar {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(
            "ores.synthetic.messaging.registrar");
        return instance;
    }

public:
    static void register_handlers(comms::net::server& server,
        database::context ctx,
        std::shared_ptr<iam::service::authorization_service> auth_service);
};

}

#endif
