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
#ifndef ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_FIXINGS_HPP
#define ORES_REPORTING_API_DOMAIN_ENGINE_SETTINGS_FIXINGS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief Fixings and cash flows of a named engine settings configuration.
 *
 * The fixings and cash flows part of a named engine settings configuration. Which fixings and cash
 * flows count in a run. A change here changes the numbers. A column that is null means the engine
 * default, and a configuration with no row in this part uses the default for every setting in it. A
 * write to this part bumps the version of the central engine_settings row in the same transaction.
 */
struct engine_settings_fixings final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The surrogate key of this part row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The central engine settings row this part belongs to.
     */
    boost::uuids::uuid engine_settings_id;

    /**
     * @brief The party that owns the configuration this part belongs to. Carried so row level
     * security can isolate the part without a join, and stamped from the session that writes it.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Whether the engine implies the fixing of the valuation date from the curves instead of
     * loading it.
     */
    std::optional<bool> imply_todays_fixings;

    /**
     * @brief Whether a cash flow that pays on the valuation date is part of the NPV.
     */
    std::optional<bool> include_todays_cash_flows;

    /**
     * @brief Whether an event on the valuation date is still counted in the NPV.
     */
    std::optional<bool> include_reference_date_events;

    /**
     * @brief Whether a gap in the fixing history of an index is filled from a neighbouring fixing.
     */
    std::optional<bool> enrich_index_fixings;

    /**
     * @brief How many days older than the gap the previous fixing may be when it fills the gap.
     * Zero turns the check off.
     */
    std::optional<int> ignore_fixing_lag;

    /**
     * @brief How many days ahead of the gap the next fixing may be when it fills the gap. Zero
     * turns the check off.
     */
    std::optional<int> ignore_fixing_lead;

    /**
     * @brief Username of the person who last modified this engine settings fixings.
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
    friend bool operator==(const engine_settings_fixings&,
                           const engine_settings_fixings&) = default;
};

/**
 * @brief Dispatch-key identifier for engine_settings_fixings, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const engine_settings_fixings&) {
    return "ores.reporting.engine_settings_fixings";
}

}

#endif
