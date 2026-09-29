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
#ifndef ORES_REFDATA_API_DOMAIN_COMMODITY_FORWARD_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_COMMODITY_FORWARD_CONVENTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Conventions for a commodity forward, where one leg pays a fixed price and the other the
 * spot price.
 *
 * Describes how ORE builds a commodity forward: how many days after the trade it
 * starts, how its points are scaled, the calendar it advances on, and whether it
 * is quoted outright or as a spread to spot. Corresponds to the
 * <CommodityForward> element in ORE conventions.xml. The id field is the natural
 * key (ORE <Id> element).
 *
 * One field is required and seven are optional. Two files carry five elements
 * between them, and every one sets all seven optional fields except the delivery
 * location, which none sets.
 */
struct commodity_forward_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief Unique commodity forward identifier.
     */
    std::string id;

    /**
     * @brief Number of business days between the trade and the forward's start.
     */
    std::optional<int> spot_days;

    /**
     * @brief Factor the quoted points are multiplied by to give a price.
     */
    std::optional<double> points_factor;

    /**
     * @brief Calendar the forward's start date advances on.
     */
    std::optional<std::string> advance_calendar;

    /**
     * @brief Whether the forward is quoted relative to spot rather than outright.
     */
    std::optional<bool> spot_relative;

    /**
     * @brief Location the underlying is delivered to.
     */
    std::optional<std::string> delivery_location;

    /**
     * @brief Business day convention the start date rolls on.
     */
    std::optional<std::string> business_day_convention;

    /**
     * @brief Whether the forward trades outright rather than as a spread.
     */
    std::optional<bool> outright;

    /**
     * @brief Username of the person who last modified this commodity forward convention.
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
    friend bool operator==(const commodity_forward_convention&,
                           const commodity_forward_convention&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity_forward_convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_forward_convention&) {
    return "ores.refdata.commodity_forward_convention";
}

}

#endif
