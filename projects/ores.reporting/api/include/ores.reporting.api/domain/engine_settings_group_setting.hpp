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
#ifndef ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_GROUP_SETTING_HPP
#define ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_GROUP_SETTING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief One setting of a group of engine settings.
 *
 * One setting of one group, as the screen draws it: the column its value travels
 * under, the ORE parameter that documents it, the kind of control it takes, and
 * the range or the list of words the value must be one of. The group's settings are
 * what a screen shows and what a write to the group accepts, so a setting is data
 * here and no longer a list in the browser's server code.
 *
 * Whether a setting changes the numbers a run computes is stated here, so the
 * screen can warn a person before a write that moves them.
 */
struct engine_settings_group_setting final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The group and the column joined with a colon, for example
     * fixings:imply_todays_fixings.
     */
    std::string code;

    /**
     * @brief The group this setting belongs to. Soft foreign key to
     * ores_reporting_engine_settings_groups_tbl.
     */
    std::string group_code;

    /**
     * @brief The column of the part the value travels under, for example imply_todays_fixings.
     */
    std::string column_name;

    /**
     * @brief The parameter as ORE writes it, which names the ore parameter that documents the
     * setting, for example implyTodaysFixings.
     */
    std::string ore_name;

    /**
     * @brief The control the screen draws and the values the setting accepts: flag, whole or
     * choice.
     */
    std::string kind;

    /**
     * @brief Whether the setting changes the numbers a run computes, and is not only operational.
     */
    bool changes_numbers = false;

    /**
     * @brief The words a choice setting accepts, comma separated, in the order the screen lists
     * them. Empty for a setting that is not a choice.
     */
    std::string options;

    /**
     * @brief The lowest value a whole setting accepts. Empty when it has no floor.
     */
    std::optional<double> minimum;

    /**
     * @brief The highest value a whole setting accepts. Empty when it has no ceiling.
     */
    std::optional<double> maximum;

    /**
     * @brief Where the setting sits in its group, counting from zero.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this engine settings group setting.
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
    friend bool operator==(const engine_settings_group_setting&,
                           const engine_settings_group_setting&) = default;
};

/**
 * @brief Dispatch-key identifier for engine_settings_group_setting, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const engine_settings_group_setting&) {
    return "ores.reporting.engine_settings_group_setting";
}

}

#endif
