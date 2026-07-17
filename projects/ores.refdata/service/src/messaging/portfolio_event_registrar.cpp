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
#include "ores.refdata.service/messaging/portfolio_event_registrar.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/registrar.hpp"
#include "ores.refdata.api/eventing/portfolio_changed_event.hpp"

namespace ores::refdata::service::messaging {

namespace {
namespace ev = ores::eventing;
}

[[nodiscard]] ev::service::subscription
register_portfolio_event_mapping(ev::service::postgres_event_source& event_source,
                                 ev::service::event_bus& event_bus,
                                 ores::nats::service::client& nats) {
    ev::service::registrar::register_mapping<refdata::eventing::portfolio_changed_event>(
        event_source, "ores.refdata.portfolio", "ores_refdata_portfolios");

    return event_bus.subscribe<refdata::eventing::portfolio_changed_event>(
        [&nats](const refdata::eventing::portfolio_changed_event& e) {
            ev::service::publish_entity_event(
                nats,
                std::string(
                    ev::domain::event_traits<refdata::eventing::portfolio_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.portfolio",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.portfolio_ids,
                                                .tenant_id = e.tenant_id});
        });
}

} // namespace ores::refdata::service::messaging
