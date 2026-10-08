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
#ifndef ORES_VARIABILITY_API_DOMAIN_SYSTEM_SETTING_HPP
#define ORES_VARIABILITY_API_DOMAIN_SYSTEM_SETTING_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::variability::domain {

/**
 * @brief A named, typed runtime configuration value.
 *
 * A system setting is a named configuration value the platform reads while it
 * runs, such as system.bootstrap_mode or onboarding.party. The value is
 * stored as text and data_type says how to read it, because one table carries
 * every type rather than one table per type.
 *
 * A setting is scoped to a tenant and a party. Tenant-wide settings live under
 * the tenant's system party, and a party-specific setting such as
 * onboarding.party lives under that party's own id. The pair is what makes a
 * setting name unique: the same name may hold different values for two parties
 * in one tenant, so a caller always reads within a scope.
 *
 * The table is bitemporal like every other entity, so a setting's history is
 * kept and a configuration change is auditable and reversible. The name is the
 * natural key callers use; id is the surrogate the store keeps for
 * foreign-key stability.
 */
struct system_setting final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate identifier for the setting row.
     */
    boost::uuids::uuid id;

    /**
     * @brief Dotted setting name, for example system.bootstrap_mode. Callers ask for a setting by
     * this name, within a tenant and party scope.
     */
    std::string name;

    /**
     * @brief Party that scopes this setting. Tenant-wide settings use the tenant's system party; a
     * party-specific setting uses that party's own id. Together with name it makes one setting
     * distinguishable from another of the same name.
     *
     * A caller writing a tenant-wide setting states the nil uuid, because it has no party to name,
     * and the database resolves the tenant's system party for it before the row lands. The column
     * is never null, so the composite unique index separates one party's rows from another's.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The setting's value, always stored as text. data_type says how to read it.
     */
    std::string value;

    /**
     * @brief How to read value: boolean, integer, string or json. Stored beside the value because
     * one table carries every type.
     */
    std::string data_type;

    /**
     * @brief What the setting controls, for an operator reading the list.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this system setting.
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
    friend bool operator==(const system_setting&, const system_setting&) = default;
};

/**
 * @brief Dispatch-key identifier for system_setting, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const system_setting&) {
    return "ores.variability.system_setting";
}

}

#endif
