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
#ifndef ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_ENTRY_HPP
#define ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_ENTRY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief One entry of one collection in a today's market document.
 *
 * One entry of one collection. Twenty-three of the document's twenty-four
 * collections are this shape: an entry identifying itself by one attribute and
 * carrying a reference as its text, in a position.
 *
 * The attribute is not one name. It is name for nineteen collections,
 * currency for DiscountingCurves, pair for the two FX collections and the
 * literal string key for SwaptionVolatilities and CapFloorVolatilities.
 * key_attribute records which one this row used, so a column named key_value
 * does not have to pretend the attribute is always the same word.
 *
 * SwaptionVolatilities and CapFloorVolatilities identify by two attributes at
 * once, so key_value_2 carries the second. SwapIndexCurves is the only
 * collection whose entry nests, and discounting carries its one child.
 */
struct todays_market_entry final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this entry.
     *
     * Surrogate key for the entry.
     */
    boost::uuids::uuid id;

    /**
     * @brief The document this entry belongs to.
     */
    boost::uuids::uuid todays_market_config_id;

    /**
     * @brief The collection this entry belongs to.
     */
    boost::uuids::uuid todays_market_collection_id;

    /**
     * @brief The attribute the entry identified itself by.
     *
     * One of 'name', 'currency', 'pair' or 'key'. Recorded rather than assumed, because one
     * document uses more than one of them.
     */
    std::string key_attribute;

    /**
     * @brief The value of that attribute.
     *
     * Examples: 'EUR', 'EUR-EURIBOR-3M', 'EUR/USD'.
     */
    std::string key_value;

    /**
     * @brief The value of the second key attribute, where a collection has two.
     *
     * Set only for SwaptionVolatilities and CapFloorVolatilities, which identify by key and
     * currency together.
     */
    std::string key_value_2;

    /**
     * @brief The entry's own id attribute, where the document writes one.
     *
     * No corpus file writes it, but every entry type declares it optional, so a document may.
     * Carried so that such a document is not silently changed.
     */
    std::string entry_id;

    /**
     * @brief The reference the entry resolves to, which is the element's text.
     *
     * Examples: 'Yield/EUR/EUR1D', 'SwaptionVolatility/EUR/EUR_SW_ATM'.
     */
    std::string target;

    /**
     * @brief The nested Discounting child, for the one collection that has one.
     *
     * Only SwapIndexCurves nests, and the census measured the nesting as exactly one level deep and
     * never more, so it is a column rather than a table.
     */
    std::string discounting;

    /**
     * @brief The order the document wrote the entry in.
     *
     * A collection may write the same key twice, so the order cannot be recovered by sorting.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this today's market entry.
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
    friend bool operator==(const todays_market_entry&, const todays_market_entry&) = default;
};

/**
 * @brief Dispatch-key identifier for todays_market_entry, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const todays_market_entry&) {
    return "ores.analytics.todays_market_entry";
}

}

#endif
