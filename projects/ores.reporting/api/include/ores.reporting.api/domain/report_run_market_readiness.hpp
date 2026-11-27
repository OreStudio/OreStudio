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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_RUN_MARKET_READINESS_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_RUN_MARKET_READINESS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief How much of a run's population its market answered, at the run's as-of.
 *
 * One row per run, written before the engine sees any work.
 *
 * A run resolves each trade's market data requirements before it submits
 * anything, and leaves out every trade whose requirements its market does not
 * answer. The counts here are that partition: every trade the scope holds is
 * valued, failed or excluded, so booked_count is complete_count plus the
 * number the run left out. The counts are stored rather than counted from the
 * detail rows at read time, because a reader of a finished run asks how much of
 * the book survived, and that answer must not depend on how much detail the run
 * kept.
 *
 * The as-of is the date the points were read at, stated on the record because
 * two runs of one book on different dates can differ and neither is the other's
 * error.
 */
struct report_run_market_readiness final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this readiness record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run this record states the readiness of. One run has one record.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The date the run read its points at. A trade is complete against this date and not
     * against another.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief How many trades the run's scope holds, before any of them was excluded.
     */
    int booked_count = 0;

    /**
     * @brief How many the market answered in full, which are the ones the run submitted.
     */
    int complete_count = 0;

    /**
     * @brief How many the run left out. The run does not store it as a subtraction, so a record
     * that disagrees with its own counts is refused at write rather than read as a partition that
     * holds.
     */
    int excluded_count = 0;

    /**
     * @brief Username of the person who last modified this run readiness.
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
    friend bool operator==(const report_run_market_readiness&,
                           const report_run_market_readiness&) = default;
};

/**
 * @brief Dispatch-key identifier for report_run_market_readiness, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_run_market_readiness&) {
    return "ores.reporting.report_run_market_readiness";
}

}

#endif
