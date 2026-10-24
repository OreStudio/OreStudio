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
#ifndef ORES_MARKETDATA_API_DOMAIN_SUBSCRIPTION_INTERVAL_HPP
#define ORES_MARKETDATA_API_DOMAIN_SUBSCRIPTION_INTERVAL_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief A period in which a party held a subscription, with its rights, and was routed to a
 * source.
 *
 * A subscription interval is a fact about usage. It records that a party held a subscription to a
 * provider with given rights and was routed to a source, from one time to another. It holds no
 * price. A rating step applies the rate card of the source to the interval later, so a new contract
 * can price old facts.
 *
 * The service opens an interval when a route and its subscription come to hold together and closes
 * it when either changes or ends. A change of the source or of the rights closes one interval and
 * opens the next. A party administrator reads the intervals of the party and the system tenant
 * reads all of them.
 */
struct subscription_interval final {
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
     * @brief When the interval opened: a route and a subscription with these rights held together
     * from this time.
     */
    std::chrono::system_clock::time_point interval_from;

    /**
     * @brief References market_data_source.id: the source that served the party when the event
     * happened. A rate card is on the source, so the rating step needs it.
     */
    boost::uuids::uuid source_id;

    /**
     * @brief The subscription held the display right during the interval.
     */
    bool display = false;

    /**
     * @brief The subscription held the non-display right during the interval.
     */
    bool non_display = false;

    /**
     * @brief The subscription held the derived right during the interval.
     */
    bool derived = false;

    /**
     * @brief When the interval closed, or null while it is open. A change of source or of rights
     * closes the interval and opens another.
     */
    std::optional<std::chrono::system_clock::time_point> interval_to;

    /**
     * @brief Username of the person who last modified this subscription interval.
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
    friend bool operator==(const subscription_interval&, const subscription_interval&) = default;
};

/**
 * @brief Dispatch-key identifier for subscription_interval, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const subscription_interval&) {
    return "ores.marketdata.subscription_interval";
}

}

#endif
