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
#ifndef ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_CHANGE_HPP
#define ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_CHANGE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief One proposed setting value, held for its approval request.
 *
 * A number-changing group of engine run settings never changes in place. A write
 * to such a group is a proposal: the values the person asked for are held here,
 * one row for each setting of the group, and the version in force keeps the rows a
 * run reads until the request is signed off.
 *
 * The apply reads the rows of an approved request and writes the group it names
 * with the values stated here, so a proposal that waits changes no part.
 *
 * A row names the setting by its code, which is the group and the column joined,
 * so a proposal survives a change to the columns of a part.
 */
struct engine_settings_change final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID identifying this line.
     *
     * One proposed setting value inside a request.
     */
    boost::uuids::uuid id;

    /**
     * @brief The approval request this line belongs to.
     *
     * References the approval requests table.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The engine run settings the proposal would change.
     *
     * References ores_reporting_engine_settings_tbl.id.
     */
    boost::uuids::uuid engine_settings_id;

    /**
     * @brief The group of settings the proposal writes, for example fixings.
     *
     * References ores_reporting_engine_settings_groups_tbl.code.
     */
    std::string group_code;

    /**
     * @brief The setting the line proposes a value for, which is its group and its column joined
     * with a colon, for example fixings:imply_todays_fixings.
     *
     * References ores_reporting_engine_settings_group_settings_tbl.code.
     */
    std::string setting_code;

    /**
     * @brief The value the person proposes, as the screen reads it, or null for the engine default.
     * A flag is true or false, a whole is its digits, and a choice is one of the words its setting
     * names.
     */
    std::optional<std::string> text_value;

    /**
     * @brief The version of the group's row the person read, or zero for a group the configuration
     * had not set. The apply claims it, so a group that moved since the proposal was raised is
     * refused rather than overwritten.
     */
    int base_version = 0;

    /**
     * @brief Whether the apply has written this line. A line is applied once.
     */
    bool applied = false;

    /**
     * @brief Username of the person who last modified this engine settings change.
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
    friend bool operator==(const engine_settings_change&, const engine_settings_change&) = default;
};

/**
 * @brief Dispatch-key identifier for engine_settings_change, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const engine_settings_change&) {
    return "ores.reporting.engine_settings_change";
}

}

#endif
