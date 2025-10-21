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
#include <memory>
#include "ores.risk/messaging/registration.hpp"
#include "ores.risk/messaging/risk_message_handler.hpp"
#include "ores.utility/log/logger.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.risk.messaging.registration"));

}

namespace ores::risk::messaging {

void register_risk_handlers(comms::server& server,
    utility::repository::context ctx) {
    BOOST_LOG_SEV(lg, info) << "Registering risk subsystem message handlers";

    // Create handler for risk subsystem
    auto handler = std::make_shared<risk_message_handler>(std::move(ctx));

    // Register for risk message type range
    comms::protocol::message_type_range risk_range{.min=0x1000, .max=0x1FFF};
    server.register_handler(risk_range, std::move(handler));

    BOOST_LOG_SEV(lg, info) << "Risk subsystem message handlers registered successfully";
}

}
