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
#ifndef ORES_REFDATA_API_DOMAIN_BASE_CORRELATION_CONFIG_HPP
#define ORES_REFDATA_API_DOMAIN_BASE_CORRELATION_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The settings of one ORE base correlation entry.
 *
 * The settings of one BaseCorrelation entry of a curveconfig.xml, one row per
 * curve_definition in the BaseCorrelations section: a credit index tranche
 * correlation surface over terms and detachment points.
 *
 * The schema also allows RecoveryGrid, RecoveryProbabilities and QuoteTypes
 * lists, which no corpus document writes; the mapper refuses an entry that has
 * them rather than lose them.
 */
struct base_correlation_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The curve entry this row belongs to.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief The terms of the surface, as ORE's comma separated text.
     */
    std::string terms;

    /**
     * @brief The tranche detachment points, as ORE's comma separated text.
     */
    std::string detachment_points;

    /**
     * @brief The number of settlement days.
     */
    double settlement_days;

    /**
     * @brief The surface's calendar, as the document spells it.
     */
    std::string calendar;

    /**
     * @brief The business day convention of the surface's dates.
     */
    std::string business_day_convention;

    /**
     * @brief The day counter of the surface, as the document spells it.
     */
    std::string day_counter;

    /**
     * @brief Whether the surface extrapolates, as the ORE boolean the document wrote.
     */
    std::optional<std::string> extrapolate;

    /**
     * @brief The name the surface's quotes are found under.
     */
    std::optional<std::string> quote_name;

    /**
     * @brief The date the surface starts from.
     */
    std::optional<std::string> start_date;

    /**
     * @brief The date generation rule of the surface's schedule.
     */
    std::optional<std::string> rule;

    /**
     * @brief Whether detachment points are adjusted for index losses, as the ORE boolean the
     * document wrote.
     */
    std::optional<std::string> adjust_for_losses;

    /**
     * @brief The term of the credit index.
     */
    std::optional<std::string> index_term;

    /**
     * @brief The quote of the credit index spread.
     */
    std::optional<std::string> index_spread;

    /**
     * @brief The currency of the credit index.
     */
    std::optional<std::string> currency;

    /**
     * @brief Whether the constituents are calibrated to the index spread, as the ORE boolean the
     * document wrote.
     */
    std::optional<std::string> calibrate_constituents_to_index_spread;

    /**
     * @brief Whether an assumed recovery is used, as the ORE boolean the document wrote.
     */
    std::optional<std::string> use_assumed_recovery;

    /**
     * @brief Username of the person who last modified this base correlation config.
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
    friend bool operator==(const base_correlation_config&,
                           const base_correlation_config&) = default;
};

/**
 * @brief Dispatch-key identifier for base_correlation_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const base_correlation_config&) {
    return "ores.refdata.base_correlation_config";
}

}

#endif
