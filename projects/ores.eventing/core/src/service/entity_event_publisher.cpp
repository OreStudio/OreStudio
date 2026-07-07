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
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.logging/make_logger.hpp"
#include <rfl/json.hpp>
#include <algorithm>

namespace ores::eventing::service {

using namespace ores::logging;

namespace {
auto& lg() {
    static auto instance = make_logger("ores.eventing.service.entity_event_publisher");
    return instance;
}
}

void publish_entity_event(ores::nats::service::client& nats,
                          const std::string& subject,
                          const domain::entity_change_event& notification) {
    try {
        const auto json = rfl::json::write(notification);
        std::vector<std::byte> data(json.size());
        std::transform(json.begin(), json.end(), data.begin(), [](char c) { return std::byte(c); });
        nats.publish(subject, std::move(data), {});
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to publish event to '" << subject << "': " << e.what();
    }
}

}
