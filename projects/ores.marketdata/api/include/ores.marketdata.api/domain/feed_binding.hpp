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
#ifndef ORES_MARKETDATA_API_DOMAIN_FEED_BINDING_HPP
#define ORES_MARKETDATA_API_DOMAIN_FEED_BINDING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Persisted mapping that binds one producer source (source_name) to the party that consumes
 * it; enables/disables the ingest loop subscription.
 *
 * A feed binding records that a party consumes one producer source. It is the
 * one place a live tick finds its owners, for every feed and every asset class: a
 * tick names its own datum by its oresmd quote URI and its producer by source,
 * and the ingest loop stores it once for each enabled binding of that source,
 * under the binding's tenant and party, and republishes it on the per-party
 * realtime stream marketdata.v1.tick.<tenant_id>.<party_id>.<ore_key>, where the
 * key is the datum's canonical ORE key. Bindings are created by provisioning
 * against the system party's config, not per office: each party consumes the
 * shared stream into its own per-party series.
 *
 * Rebinding (editing source_name) switches the ingest source without restarting
 * producers. Setting enabled  false= suspends the subscription without deleting
 * the binding.
 *
 * This model binds to no variability profile. Its features match
 * uuid-surrogate-lookup, but that profile also enables the shell command
 * facet, which feed bindings do not have today. Binding it is a decision about
 * the shell surface, not about the model.
 */
struct feed_binding final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this feed binding.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this feed binding.
     *
     * Set server-side from the authenticated session. Enforced by RLS.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The producer, unique within a tenant: the source every tick it publishes carries, and
     * the key of the ingest loop's binding cache. Matches the source_name of the feed config the
     * binding was created from.
     */
    std::string source_name;

    /**
     * @brief When true the marketdata service maintains an active NATS subscription for this
     * binding. Setting to false suspends ingestion without removing the binding record.
     */
    bool enabled = true;

    /**
     * @brief Username of the person who last modified this feed binding.
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
    friend bool operator==(const feed_binding&, const feed_binding&) = default;
};

/**
 * @brief Dispatch-key identifier for feed_binding, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const feed_binding&) {
    return "ores.marketdata.feed_binding";
}

}

#endif
