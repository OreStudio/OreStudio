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
#include "ores.iam.service/messaging/event_registrar.hpp"

// Per-entity generated event-mapping registrars.
#include "ores.iam.service/messaging/account_contact_information_event_registrar.hpp"
#include "ores.iam.service/messaging/account_event_registrar.hpp"
#include "ores.iam.service/messaging/account_type_event_registrar.hpp"
#include "ores.iam.service/messaging/login_info_event_registrar.hpp"
#include "ores.iam.service/messaging/permission_event_registrar.hpp"
#include "ores.iam.service/messaging/role_event_registrar.hpp"
#include "ores.iam.service/messaging/session_event_registrar.hpp"
#include "ores.iam.service/messaging/tenant_event_registrar.hpp"
#include "ores.iam.service/messaging/tenant_status_event_registrar.hpp"
#include "ores.iam.service/messaging/tenant_type_event_registrar.hpp"

namespace ores::iam::service::messaging {

std::vector<ores::eventing::service::subscription> event_registrar::register_event_mappings(
    ores::eventing::service::postgres_event_source& event_source,
    ores::eventing::service::event_bus& event_bus,
    ores::nats::service::client& nats) {
    std::vector<ores::eventing::service::subscription> subs;

    // Each register_<entity>_event_mapping() maps the entity's Postgres NOTIFY
    // channel to its event type and returns the event_bus subscription that
    // republishes it to NATS. The subscriptions are returned so the caller can
    // keep them alive for the service's lifetime; a mapping with no live
    // subscription never reaches its subject.
    subs.push_back(register_account_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_account_contact_information_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_account_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_login_info_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_permission_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_role_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_session_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenant_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenant_status_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenant_type_event_mapping(event_source, event_bus, nats));

    return subs;
}

}
