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
#include "ores.dq.service/messaging/event_registrar.hpp"

// Per-entity generated event-mapping registrars.
#include "ores.dq.service/messaging/artefact_type_event_registrar.hpp"
#include "ores.dq.service/messaging/badge_definition_event_registrar.hpp"
#include "ores.dq.service/messaging/badge_severity_event_registrar.hpp"
#include "ores.dq.service/messaging/catalog_event_registrar.hpp"
#include "ores.dq.service/messaging/change_reason_category_event_registrar.hpp"
#include "ores.dq.service/messaging/change_reason_event_registrar.hpp"
#include "ores.dq.service/messaging/code_domain_event_registrar.hpp"
#include "ores.dq.service/messaging/data_domain_event_registrar.hpp"
#include "ores.dq.service/messaging/dataset_bundle_event_registrar.hpp"
#include "ores.dq.service/messaging/lei_entity_event_registrar.hpp"
#include "ores.dq.service/messaging/lei_relationship_event_registrar.hpp"
#include "ores.dq.service/messaging/report_definition_event_registrar.hpp"
#include "ores.dq.service/messaging/subject_area_event_registrar.hpp"
#include "ores.dq.service/messaging/synthetic_fx_spot_config_event_registrar.hpp"

namespace ores::dq::service::messaging {

std::vector<ores::eventing::service::subscription> event_registrar::register_event_mappings(
    ores::eventing::service::postgres_event_source& event_source,
    ores::eventing::service::event_bus& event_bus,
    ores::nats::service::client& nats) {
    std::vector<ores::eventing::service::subscription> subs;

    // ----------------------------------------------------------------
    // Per-entity event mappings. Each register_<entity>_event_mapping()
    // registers the entity's Postgres NOTIFY channel and returns the
    // event_bus subscription that republishes it to NATS; we take
    // ownership of the subscriptions here so they outlive this call.
    // ----------------------------------------------------------------
    subs.push_back(register_artefact_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_badge_definition_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_badge_severity_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_catalog_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_change_reason_category_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_change_reason_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_code_domain_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_data_domain_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_dataset_bundle_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_lei_entity_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_lei_relationship_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_report_definition_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_subject_area_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_synthetic_fx_spot_config_event_mapping(event_source, event_bus, nats));

    return subs;
}

}
