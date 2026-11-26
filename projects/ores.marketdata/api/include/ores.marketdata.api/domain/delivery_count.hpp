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
 * A row holds the deliveries counted so far in an hour, so a flush adds to the total and the rating
 * step reads one row for the hour.
 *
 * The key is the party, the provider, the feed, the item and the hour, and the tenant is not in it.
 * A party belongs to exactly one tenant, so two tenants cannot collide on this key, and the column
 * is carried for row level security and for the reads that filter by it. The schema validator
 * states the same exception, with that reason, in projects/ores.sql/utility/validation_ignore.txt.
 */
struct delivery_count final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

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
     * @brief References feed.id: the source that served the party when the event happened. A rate
     * card is on the source, so the rating step needs it.
     */
    boost::uuids::uuid feed_id;

    /**
     * @brief The item whose ticks were delivered, as its oresmd quote URI.
     */
    std::string oresmd_uri;

    /**
     * @brief The start of the hour the ticks were delivered in.
     */
    std::chrono::system_clock::time_point period_start;

    /**
     * @brief UUID identifying this count, and the identifier its events and screen use.
     */
    boost::uuids::uuid id;

    /**
     * @brief When the service last wrote the count. The row holds the running total for the hour,
     * so a later write replaces it rather than adding a row, and the rating step reads the total
     * instead of summing the rows of the hour.
     */
    std::chrono::system_clock::time_point flushed_at;

    /**
     * @brief How many ticks of the item the fan-out has delivered to the party in the hour so far.
     */
    std::int64_t deliveries = 0;

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
