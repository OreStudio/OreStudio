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
#ifndef ORES_TRADING_API_DOMAIN_FPML_EVENT_TYPE_HPP
#define ORES_TRADING_API_DOMAIN_FPML_EVENT_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Standard FpML lc_EventType_Type coding scheme (New, Amendment, Novation,
 * PartialTermination, FullTermination).
 *
 * Reference data table holding the standard FpML lc_EventType_Type coding
 * scheme. Each row names one wire-format event code: New, Amendment,
 * Novation, PartialTermination and FullTermination. These are the FpML
 * equivalents the internal activity types map to.
 *
 * The table is bi-temporal and audited: it carries version, the four audit
 * columns and the valid_from/valid_to pair with the GIST exclusion and the
 * delete rule, so the model takes the ordinary audited shape and needs no
 * shape flag.
 */
struct fpml_event_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique FpML event type code.
     *
     * Examples: 'New', 'Amendment', 'Novation', 'PartialTermination', 'FullTermination'.
     */
    std::string code;

    /**
     * @brief Detailed description of the FpML event type.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this FpML event type.
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
    friend bool operator==(const fpml_event_type&, const fpml_event_type&) = default;
};

/**
 * @brief Dispatch-key identifier for fpml_event_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fpml_event_type&) {
    return "ores.trading.fpml_event_type";
}

}

#endif
