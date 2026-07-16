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
#include "ores.synthetic.service/messaging/event_registrar.hpp"

// Per-entity generated event-mapping registrars.
#include "ores.synthetic.service/messaging/fx_spot_generation_config_event_registrar.hpp"
#include "ores.synthetic.service/messaging/gmm_component_event_registrar.hpp"
#include "ores.synthetic.service/messaging/ir_curve_generation_config_event_registrar.hpp"
#include "ores.synthetic.service/messaging/yield_curve_process_type_event_registrar.hpp"
#include "ores.synthetic.service/messaging/ir_curve_template_entry_event_registrar.hpp"
#include "ores.synthetic.service/messaging/market_data_generation_config_event_registrar.hpp"

namespace ores::synthetic::service::messaging {

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
    subs.push_back(register_fx_spot_generation_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_gmm_component_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_ir_curve_generation_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_yield_curve_process_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_ir_curve_template_entry_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_market_data_generation_config_event_mapping(event_source, event_bus, nats));

    return subs;
}

}
