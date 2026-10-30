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
#ifndef ORES_MARKETDATA_API_DOMAIN_DELIVERY_COUNT_HPP
#define ORES_MARKETDATA_API_DOMAIN_DELIVERY_COUNT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief How many ticks of an item the fan-out delivered to a party in an hour.
 *
 * A delivery count is a fact about usage. The fan-out counts the ticks it delivers to each party
 * for each item and writes the counts in batches. A count holds no price. The only cost at tick
 * time is one increment in memory, and one row is written for each party, item and hour at each
 * write.
 *
 * A row counts the deliveries since the last write, so the rating step adds the rows of an hour.
 */
struct delivery_count final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this count.
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
     * @brief The item whose ticks were delivered, as its oresmd quote URI.
     */
    std::string oresmd_uri;

    /**
     * @brief The start of the hour the ticks were delivered in.
     */
    std::chrono::system_clock::time_point period_start;

    /**
     * @brief When the service wrote the count. The counts of one hour that were written at
     * different times are separate rows, and the rating step adds them.
     */
    std::chrono::system_clock::time_point flushed_at;

    /**
     * @brief References feed.id: the source that served the party when the event
     * happened. A rate card is on the source, so the rating step needs it.
     */
    boost::uuids::uuid feed_id;

    /**
     * @brief How many ticks of the item the fan-out delivered to the party in the hour, since the
     * last write.
     */
    std::int64_t deliveries = 0;

    /**
     * @brief Username of the person who last modified this delivery count.
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
    friend bool operator==(const delivery_count&, const delivery_count&) = default;
};

/**
 * @brief Dispatch-key identifier for delivery_count, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const delivery_count&) {
    return "ores.marketdata.delivery_count";
}

}

#endif
