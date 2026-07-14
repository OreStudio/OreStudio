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
#ifndef ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_IDENTIFIER_EVENT_REGISTRAR_HPP
#define ORES_REFDATA_SERVICE_MESSAGING_COUNTERPARTY_IDENTIFIER_EVENT_REGISTRAR_HPP

#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.nats/service/client.hpp"

namespace ores::refdata::service::messaging {

[[nodiscard]] ores::eventing::service::subscription register_counterparty_identifier_event_mapping(
    ores::eventing::service::postgres_event_source& event_source,
    ores::eventing::service::event_bus& event_bus,
    ores::nats::service::client& nats);

} // namespace ores::refdata::service::messaging

#endif
