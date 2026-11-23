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
#ifndef ORES_SYNTHETIC_API_DOMAIN_STOPPED_SIMULATED_SOURCE_HPP
#define ORES_SYNTHETIC_API_DOMAIN_STOPPED_SIMULATED_SOURCE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A simulated source an operator stopped, kept until someone starts it.
 *
 * A simulated source of the market data catalogue that an operator stopped. The
 * synthetic service serves each simulated source with an adapter, which runs the
 * feeds the store demands of it and holds the vol surfaces its provider quotes. A
 * row here keeps that adapter stopped, a restart of the service included, until
 * someone starts the source again and the row is removed. The row's history is
 * the record of who stopped and started the source, when, and why.
 *
 * The catalogue owns the sources and has no flag for this, and a collection's own
 * enabled flag gates the hand starts of its feeds too, so the stop is held here.
 *
 * An operator stops and starts a source through the simulated source verbs, which
 * take the feeds run permission, write this row and stop or start the adapter
 * together. The generated write and delete of this entity take their own
 * permissions, in the system tenant only, and change the row alone: the adapter
 * follows the row at the next start of the service. They are for the record, not
 * for operating a source.
 */
struct stopped_simulated_source final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The catalogue name of the source, such as bloomberg-a: the id its adapter announces
     * under.
     */
    std::string source_name;

    /**
     * @brief The provider whose notation the source's ticks use, so a screen of the provider finds
     * its stopped sources without reading the catalogue.
     */
    std::string provider_code;

    /**
     * @brief Username of the person who last modified this stopped simulated source.
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
    friend bool operator==(const stopped_simulated_source&,
                           const stopped_simulated_source&) = default;
};

/**
 * @brief Dispatch-key identifier for stopped_simulated_source, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const stopped_simulated_source&) {
    return "ores.synthetic.stopped_simulated_source";
}

}

#endif
