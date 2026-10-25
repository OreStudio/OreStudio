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
#ifndef ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_GROUP_HPP
#define ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_GROUP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A group of engine settings a configuration holds as one part.
 *
 * The engine settings a screen draws come in groups, and a configuration keeps one
 * row of each group it sets. The group states the code the screen and the service
 * travel the part under, the words a person reads and the place it takes in the
 * screen. The settings of the group are rows of engine_settings_group_setting.
 *
 * Until this table held the groups, a list of them lived in the browser's server
 * code, so a new group of settings took a change of code. The list is data now, so
 * the screen draws the groups the data holds, in the order the data states.
 */
struct engine_settings_group final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code of the group, and the word the part of a configuration travels under,
     * for example fixings.
     */
    std::string code;

    /**
     * @brief The name a person reads, for example Fixings.
     */
    std::string name;

    /**
     * @brief What the settings of this group decide.
     */
    std::string description;

    /**
     * @brief Where the group sits in the screen, counting from zero.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this engine settings group.
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
    friend bool operator==(const engine_settings_group&, const engine_settings_group&) = default;
};

/**
 * @brief Dispatch-key identifier for engine_settings_group, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const engine_settings_group&) {
    return "ores.reporting.engine_settings_group";
}

}

#endif
