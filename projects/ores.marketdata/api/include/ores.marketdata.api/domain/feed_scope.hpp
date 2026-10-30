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
#ifndef ORES_MARKETDATA_API_DOMAIN_FEED_SCOPE_HPP
#define ORES_MARKETDATA_API_DOMAIN_FEED_SCOPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One item a feed fetches, with its asset class and the vendor instrument that carries it.
 *
 * A feed scope states what a feed fetches. Each row names the feed, the asset
 * class and the item, and may name the vendor instrument the supplier uses for it.
 * The scope is the set of rows a feed holds; a read of a feed's scope is the rows
 * that name it.
 *
 * No item belongs to two feeds of one publisher. A publisher may have an
 * end_of_day feed and a real_time feed, but an item arrives through one of
 * them, so a party reading the item reads one feed's values and never a mixture.
 */
struct feed_scope final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this scope row.
     */
    boost::uuids::uuid id;

    /**
     * @brief References feed.id: the feed whose scope this row belongs to.
     */
    boost::uuids::uuid feed_id;

    /**
     * @brief References the asset class codes: fx, ir, equity and so on.
     */
    std::string asset_class;

    /**
     * @brief The internal identity of the value, as an oresmd URI, such as
     * =oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD=.
     */
    std::string item;

    /**
     * @brief References market_data_vendor_instrument.id: the supplier's id for this item. Null
     * when the feed's adapter has no vendor instrument, as the service does for a derived item.
     */
    boost::uuids::uuid vendor_instrument_id;

    /**
     * @brief Username of the person who last modified this feed scope.
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
    friend bool operator==(const feed_scope&, const feed_scope&) = default;
};

/**
 * @brief Dispatch-key identifier for feed_scope, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const feed_scope&) {
    return "ores.marketdata.feed_scope";
}

}

#endif
