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
#ifndef ORES_SYNTHETIC_API_DOMAIN_STOPPED_FEED_NODE_HPP
#define ORES_SYNTHETIC_API_DOMAIN_STOPPED_FEED_NODE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief A feed node an operator stopped, kept until someone starts it.
 *
 * A node of the synthetic feed graph that an operator stopped, named by the
 * naming scheme. Two kinds of node are stopped here:
 *
 * - a /source/, a simulated source of the market data catalogue such as
 *   bloomberg-a, served by an adapter that runs the feeds the store demands of
 *   it and holds the vol surfaces its provider quotes; its stop stops the
 *   adapter;
 * - a /view/, one vendor's quotes of a latent vol surface, such as
 *   synthetic.ice.fx.fx_option.audusd; its stop withholds that vendor's points
 *   of that surface, and the surface and the other vendors run on.
 *
 * A row keeps the node stopped, a restart of the service included, until someone
 * starts it again and the row is removed. The row's history is the record of who
 * stopped and started the node, when, and why. One table holds every kind, so a
 * node is stopped and started by one pair of verbs, by its name; see
 * [[id:2376A086-4C3C-4D61-92E5-29B122DB6A41][Synthetic feed graph]].
 *
 * An operator stops and starts a node through the feed node verbs, which take
 * the feeds run permission, write this row and stop or start the node together.
 * The generated write and delete of this entity take their own permissions, in
 * the system tenant only, and change the row alone: the node follows the row at
 * the next start of the service. They are for the record, not for operating a
 * node.
 */
struct stopped_feed_node final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The node's name: a source's catalogue name, such as bloomberg-a, the id its adapter
     * announces under, or a view's scheme name, such as synthetic.ice.fx.fx_option.audusd.
     */
    std::string node_name;

    /**
     * @brief What sort of node it is: source or view.
     */
    std::string kind;

    /**
     * @brief The provider whose notation the node's ticks use, so a screen of the provider finds
     * its stopped nodes without reading the catalogue.
     */
    std::string provider_code;

    /**
     * @brief Username of the person who last modified this stopped feed node.
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
    friend bool operator==(const stopped_feed_node&, const stopped_feed_node&) = default;
};

/**
 * @brief Dispatch-key identifier for stopped_feed_node, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const stopped_feed_node&) {
    return "ores.synthetic.stopped_feed_node";
}

}

#endif
