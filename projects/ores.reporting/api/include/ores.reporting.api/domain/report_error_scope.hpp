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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_ERROR_SCOPE_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_ERROR_SCOPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief What a report error is about, named in the engine's own vocabulary.
 *
 * The vocabulary a report error's scope is drawn from. The engine that computes a
 * report states the scope of every error it emits, in its structured log, as a
 * group. The codes here are those group names verbatim, so the model adds no
 * classification of its own: the engine says Trade and the row says Trade.
 *
 *
 * Report is the only code this model states rather than copies. Every other code
 * is a string the engine writes, and a code the engine writes that is not a row
 * here is a vocabulary the engine has grown: the row is refused, loudly, rather
 * than stored with a scope no reader can place.
 */
struct report_error_scope final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The engine's own group name, verbatim: Trade, Analytics, Curve, Configuration, or
     * Report for an error about the run itself.
     */
    std::string code;

    /**
     * @brief The human-readable name of the scope.
     */
    std::string name;

    /**
     * @brief What an error of this scope is about, in one line.
     */
    std::string description;

    /**
     * @brief The order the scope appears in a list, narrowest subject first.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this report error scope.
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
    friend bool operator==(const report_error_scope&, const report_error_scope&) = default;
};

/**
 * @brief Dispatch-key identifier for report_error_scope, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_error_scope&) {
    return "ores.reporting.report_error_scope";
}

}

#endif
