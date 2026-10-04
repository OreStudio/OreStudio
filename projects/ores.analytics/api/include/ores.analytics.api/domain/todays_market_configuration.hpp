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
#ifndef ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_CONFIGURATION_HPP
#define ORES_ANALYTICS_API_DOMAIN_TODAYS_MARKET_CONFIGURATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief One named configuration in a today's market document.
 *
 * One Configuration block. It is not a collection of entries: it is a bundle of
 * references, each naming one entry of one collection, so that a report can select
 * a coherent set of curves and volatilities by name instead of restating them.
 *
 * It identifies itself by a required id attribute, which is unique within a
 * document. The references it makes live on
 * todays_market_configuration_binding.
 */
struct todays_market_configuration final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this configuration block.
     *
     * Surrogate key for the configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The document this configuration belongs to.
     */
    boost::uuids::uuid todays_market_config_id;

    /**
     * @brief The id attribute the document wrote on the Configuration element.
     *
     * Unique within a document, not within a tenant: two documents may each have a configuration
     * called 'default'. So it is not a natural key, and uniqueness is held by the pair of the
     * document and this value rather than by this value alone. Examples: 'default',
     * 'collateral_inccy', 'ois'.
     */
    std::string configuration_id;

    /**
     * @brief The order the document wrote the configuration in.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this today's market configuration block.
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
    friend bool operator==(const todays_market_configuration&,
                           const todays_market_configuration&) = default;
};

/**
 * @brief Dispatch-key identifier for todays_market_configuration, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const todays_market_configuration&) {
    return "ores.analytics.todays_market_configuration";
}

}

#endif
