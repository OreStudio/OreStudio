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
#include "ores.refdata.service/messaging/event_registrar.hpp"

// Per-entity generated event-mapping registrars.
//
// Not every ores.refdata domain_entity is wired here yet:
// counterparty, counterparty_contact_information, counterparty_identifier,
// currency_market_tier, monetary_nature, party, party_contact_information,
// party_identifier, and portfolio have stale nats-eventing output (their
// changed_event's id field would rename from `ids` to `<entity>_ids` on
// regeneration) — pre-existing drift unrelated to this registrar, tracked
// separately. Those nine stay hand-wired in application.cpp until that drift
// is resolved; see the migrate-remaining-entities capture. business_unit was
// migrated here (its own such drift resolved) while moving it, business_unit_type,
// and party_id_scheme onto full codegen — see the "Migrate all entities onto the
// generic HistoryDialog" story.
#include "ores.refdata.service/messaging/asset_class_code_event_registrar.hpp"
#include "ores.refdata.service/messaging/book_event_registrar.hpp"
#include "ores.refdata.service/messaging/book_purpose_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/book_status_event_registrar.hpp"
#include "ores.refdata.service/messaging/business_day_convention_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/business_unit_event_registrar.hpp"
#include "ores.refdata.service/messaging/business_unit_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/contact_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/country_event_registrar.hpp"
#include "ores.refdata.service/messaging/crm_driver_pair_event_registrar.hpp"
#include "ores.refdata.service/messaging/crm_enabled_derived_pair_event_registrar.hpp"
#include "ores.refdata.service/messaging/crm_topology_config_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_market_tier_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_pair_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/currency_pair_event_registrar.hpp"
#include "ores.refdata.service/messaging/day_count_fraction_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/instrument_code_event_registrar.hpp"
#include "ores.refdata.service/messaging/ledger_feed_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/monetary_nature_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_id_scheme_event_registrar.hpp"
#include "ores.refdata.service/messaging/party_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/payment_frequency_event_registrar.hpp"
#include "ores.refdata.service/messaging/purpose_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/regulatory_book_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/rounding_type_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_anchor_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_convention_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_kind_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_resolution_algorithm_event_registrar.hpp"
#include "ores.refdata.service/messaging/curve_role_event_registrar.hpp"
#include "ores.refdata.service/messaging/tenor_unit_event_registrar.hpp"

namespace ores::refdata::service::messaging {

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
    subs.push_back(register_asset_class_code_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_book_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_book_purpose_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_book_status_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_business_day_convention_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_business_unit_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_business_unit_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_contact_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_country_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_crm_driver_pair_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_crm_enabled_derived_pair_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_crm_topology_config_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_market_tier_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_pair_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_currency_pair_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_day_count_fraction_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_instrument_code_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_ledger_feed_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_monetary_nature_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_id_scheme_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_party_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_payment_frequency_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_purpose_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_regulatory_book_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_rounding_type_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_anchor_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_convention_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_kind_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_curve_role_event_mapping(event_source, event_bus, nats));
    subs.push_back(register_tenor_unit_event_mapping(event_source, event_bus, nats));
    subs.push_back(
        register_tenor_resolution_algorithm_event_mapping(event_source, event_bus, nats));

    return subs;
}

}
