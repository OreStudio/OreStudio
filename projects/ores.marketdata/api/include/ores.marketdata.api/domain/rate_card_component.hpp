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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_DOMAIN_RATE_CARD_COMPONENT_HPP
#define ORES_MARKETDATA_API_DOMAIN_RATE_CARD_COMPONENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One charge of the rate card of a source: what the vendor charges for, and how much.
 *
 * A rate card belongs to a source. It is the set of components that apply to the source in a
 * period. Each component has a driver, which says what the vendor counts, and a charge for one unit
 * of it. A rating step applies the components to the usage events of a period and gives the bill of
 * the source.
 *
 * A simulated source carries a card of its own with illustrative charges, or published ones where
 * they exist. The model is the same for every source, so a simulated source gives a shadow bill
 * with the same lines as a live one. The system tenant owns the cards, because the platform owns
 * the vendor contracts.
 */
struct rate_card_component final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this component.
     */
    boost::uuids::uuid id;

    /**
     * @brief References feed.id: the source whose vendor charges this way. A rate
     * card belongs to a source.
     */
    boost::uuids::uuid feed_id;

    /**
     * @brief What the charge counts: fixed_per_period, per_instrument, per_party, per_licence or
     * per_thousand_ticks.
     */
    std::string driver;

    /**
     * @brief The right a per_licence component charges for: non_display or derived. Empty for every
     * other driver.
     */
    std::string licence_right = "";

    /**
     * @brief When the component starts to apply. A new contract is a new component from a new date,
     * so the old events keep the old charge.
     */
    std::chrono::system_clock::time_point effective_from;

    /**
     * @brief The ISO code of the currency the charge is in.
     */
    std::string currency = "USD";

    /**
     * @brief What one unit of the driver costs, in millionths of the currency. Whole numbers keep a
     * bill exact: the charge for a count is the count times this number.
     */
    std::int64_t charge_micros = 0;

    /**
     * @brief When the component stops to apply, or null while it applies.
     */
    std::optional<std::chrono::system_clock::time_point> effective_to;

    /**
     * @brief Username of the person who last modified this rate card component.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const rate_card_component&, const rate_card_component&) = default;
};

/**
 * @brief Dispatch-key identifier for rate_card_component, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const rate_card_component&) {
    return "ores.marketdata.rate_card_component";
}

}

#endif
