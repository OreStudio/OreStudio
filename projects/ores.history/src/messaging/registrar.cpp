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
#include "ores.history/messaging/registrar.hpp"
#include "ores.history/messaging/history_handler.hpp"
#include "ores.history/messaging/history_protocol.hpp"
#include <memory>

namespace ores::history::messaging {

ores::nats::service::subscription
register_history_handlers(ores::nats::service::client& nats,
                          const service::dispatch_registry& registry,
                          std::string_view queue_group) {
    auto handler = std::make_shared<history_handler>(nats, registry);
    return nats.queue_subscribe(
        get_entity_history_request::nats_subject, queue_group, [handler](ores::nats::message msg) {
            handler->history(std::move(msg));
        });
}

}
