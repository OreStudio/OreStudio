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
#ifndef ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_COLLECTION_KIND_HPP
#define ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_COLLECTION_KIND_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief One kind of collection an ORE today's market document holds, with the element and
 * attributes its entries use.
 *
 * A todaysmarket.xml document is a set of collections of twenty-four kinds --
 * YieldCurves, DiscountingCurves, FxSpots and the rest -- plus
 * configurations that pick one collection of each kind. The schema fixes the set
 * as the children of its TodaysMarket element, apart from Configuration, and
 * fixes for each kind the element its entries are written as and the attribute
 * or two attributes an entry identifies itself by.
 *
 * The kind is a seeded lookup, seeded for the system tenant and read by every
 * tenant, so a collection or a binding that names a kind ORE does not have is
 * refused, and an entry no longer repeats its kind's key attribute.
 */
struct todays_market_collection_kind final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The collection's element name in todaysmarket.xml, exactly as ORE spells it:
     * 'YieldCurves', 'DiscountingCurves', 'FxSpots'. A collection row and a configuration binding
     * both name their kind by it.
     */
    std::string code;

    /**
     * @brief The element one entry of this kind is written as: 'YieldCurve' for 'YieldCurves',
     * 'Index' for 'IndexForwardingCurves'.
     */
    std::string entry_element;

    /**
     * @brief The attribute an entry identifies itself by: 'name' for nineteen kinds, 'currency' for
     * 'DiscountingCurves', 'pair' for the two FX kinds and 'key' for the swaption and cap and floor
     * volatilities.
     */
    std::string key_attribute;

    /**
     * @brief The second attribute of the two kinds an entry identifies by two: 'currency' for
     * 'SwaptionVolatilities' and 'CapFloorVolatilities'. Null for every other kind.
     */
    std::optional<std::string> key_attribute_2;

    /**
     * @brief What the kind holds, in one line, for a reader who does not know ORE.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this today's market collection kind.
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
    friend bool operator==(const todays_market_collection_kind&,
                           const todays_market_collection_kind&) = default;
};

/**
 * @brief Dispatch-key identifier for todays_market_collection_kind, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const todays_market_collection_kind&) {
    return "ores.analytics.todays_market_collection_kind";
}

}

#endif
