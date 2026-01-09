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
#include "ores.assets/messaging/registrar.hpp"

#include "ores.assets/messaging/assets_message_handler.hpp"

namespace ores::assets::messaging {

using namespace ores::logging;

void registrar::register_handlers(comms::net::server& server,
    database::context ctx) {
    BOOST_LOG_SEV(lg(), info) << "Registering assets subsystem message handlers.";

    auto handler = std::make_shared<assets_message_handler>(std::move(ctx));
    comms::messaging::message_type_range assets_range{
        .min = comms::messaging::ASSETS_SUBSYSTEM_MIN,
        .max = comms::messaging::ASSETS_SUBSYSTEM_MAX
    };
    server.register_handler(assets_range, std::move(handler));

    BOOST_LOG_SEV(lg(), info) << "Assets subsystem message handlers registered successfully.";
}

}
