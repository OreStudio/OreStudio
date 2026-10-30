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
#ifndef ORES_MARKETDATA_API_DOMAIN_DEMAND_INTERVAL_HPP
#define ORES_MARKETDATA_API_DOMAIN_DEMAND_INTERVAL_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief A period in which an item was in the demand of a party.
 *
 * A demand interval is a fact about usage. It records that one item was in the demand set of a
 * party, and so was published by the source that served the party, from one time to another. Demand
 * is a control signal for the adapter and a billing fact for the vendor: a live adapter pays for
 * the items in the set.
 *
 * The service opens an interval when an item enters the demand of a party and closes it when the
 * item leaves, or when the party stops being served by the source.
 */
struct demand_interval final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this interval.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that used the feed. Set from the service that records the event and enforced
     * by row level security, so a party administrator reads only the events of the party.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief References provider.code: the provider whose feed the event is about.
     */
    std::string provider_code;

    /**
     * @brief The item the party demanded, as its oresmd quote URI.
     */
    std::string oresmd_uri;

    /**
     * @brief When the item entered the demand of the party.
     */
    std::chrono::system_clock::time_point interval_from;

    /**
     * @brief References feed.id: the source that served the party when the event
     * happened. A rate card is on the source, so the rating step needs it.
     */
    boost::uuids::uuid feed_id;

    /**
     * @brief When the item left the demand of the party, or null while it is in it.
     */
    std::optional<std::chrono::system_clock::time_point> interval_to;

    /**
     * @brief Username of the person who last modified this demand interval.
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
    friend bool operator==(const demand_interval&, const demand_interval&) = default;
};

/**
 * @brief Dispatch-key identifier for demand_interval, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const demand_interval&) {
    return "ores.marketdata.demand_interval";
}

}

#endif
