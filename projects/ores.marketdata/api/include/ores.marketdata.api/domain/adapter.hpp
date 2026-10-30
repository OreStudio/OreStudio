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
#ifndef ORES_MARKETDATA_API_DOMAIN_ADAPTER_HPP
#define ORES_MARKETDATA_API_DOMAIN_ADAPTER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief A code module that speaks one supplier protocol, with its version, services and
 * configuration schema.
 *
 * The catalogue of adapter modules. An adapter is a class the release defines; a
 * person configures a session of it and then the feeds that come off the session.
 * The class announces itself when it comes up, and that announce is what marks the
 * module installed on this installation, so an installation that never runs a
 * module never claims it.
 * The catalogue states the module's version, the asset classes it can serve, the
 * services it offers, and the configuration a session must provide, so the screen
 * can build the session form from data instead of from code.
 *
 * The system tenant owns the rows. Every tenant reads them and only the system
 * tenant writes them.
 */
struct adapter final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique adapter code: lower case letters, digits, underscores and hyphens, starting
     * with a letter.
     *
     * Examples: 'frankfurter', 'bloomberg-api', 'simulation'.
     */
    std::string code;

    /**
     * @brief The adapter's name as a person knows it.
     */
    std::string name;

    /**
     * @brief The module version the release provides, such as 1.4.0.
     */
    std::string module_version;

    /**
     * @brief What the adapter reads, in a sentence or two.
     */
    std::string description;

    /**
     * @brief The process that hosts the adapter, such as ores.marketdata.official.
     */
    std::string process;

    /**
     * @brief The asset classes the adapter can serve, as a comma-separated list in lower case, such
     * as fx,ir.
     */
    std::string asset_classes;

    /**
     * @brief The services the adapter offers, as a comma-separated list, such as
     * real_time,end_of_day,reference.
     */
    std::string services;

    /**
     * @brief The fields a session of this adapter must provide, as a JSON object of name to type,
     * required flag and default. The session form is built from it.
     */
    std::string configuration_schema = "{}";

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this adapter.
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
    friend bool operator==(const adapter&, const adapter&) = default;
};

/**
 * @brief Dispatch-key identifier for adapter, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const adapter&) {
    return "ores.marketdata.adapter";
}

}

#endif
