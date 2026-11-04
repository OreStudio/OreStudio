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
#ifndef ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_INPUT_POINT_HPP
#define ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_INPUT_POINT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief One fixing date of a variance calibration input, used or left out with its reason.
 *
 * One fixing date of a variance_calibration_input: the fixing as it was stored, the
 * number read from it, and the daily log return it gives in the pair's market
 * convention. A point the fit may not use is kept with the reason, such as a value
 * that is not a number, so the input states every date the window held and not only
 * the ones that counted.
 *
 * log_return is the return from the previous used point to this one, after the
 * inversion the input names. The first used point has no return, and a point whose
 * value cannot be read has neither a value nor a return.
 */
struct variance_calibration_input_point final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this point.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party, the input's.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The variance_calibration_input this point belongs to.
     */
    boost::uuids::uuid input_id;

    /**
     * @brief The fixing date.
     */
    std::chrono::year_month_day fixing_date;

    /**
     * @brief The fixing exactly as the store held it, before it was read as a number.
     */
    std::string fixing_text;

    /**
     * @brief The fixing read as a number, in the item's own convention; absent when the text is not
     * a number.
     */
    std::optional<double> fixing_value;

    /**
     * @brief The daily log return to this point in the pair's market convention; absent for the
     * first used point and for a point that was not read.
     */
    std::optional<double> log_return;

    /**
     * @brief Whether the fit may use this point's return.
     */
    bool used = true;

    /**
     * @brief Why the point is not used, such as "the value is not a number" or "the first point has
     * no return"; empty when it is used.
     */
    std::string exclusion_reason = "";

    /**
     * @brief Username of the person who last modified this variance calibration input point.
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
    friend bool operator==(const variance_calibration_input_point&,
                           const variance_calibration_input_point&) = default;
};

/**
 * @brief Dispatch-key identifier for variance_calibration_input_point, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const variance_calibration_input_point&) {
    return "ores.synthetic.variance_calibration_input_point";
}

}

#endif
