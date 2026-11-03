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
#ifndef ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_INPUT_HPP
#define ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_INPUT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief The stored fixings a GJR-GARCH variance calibration is fitted from.
 *
 * One capture of the data a variance calibration reads: the item that was read for a
 * pair, under which calibration policy, over which window, and whether its value was
 * inverted to reach the pair's market convention. Each fixing date of the window is
 * a variance_calibration_input_point, kept whether it was used or not, so a reader
 * can see every number a fit saw. A fit reads only a stored input, never the live
 * fixings, so the same input always gives the same fit. See "A calibration is stored
 * and reviewed before a feed runs it" in
 * [[id:351CB816-28F5-428A-A289-414B6F4B9F36][Volatility clustering and GARCH models]].
 *
 * Who captured the input and when are the performed_by and valid_from every row
 * carries. A capture over the same window of the same item is a new version of the
 * input, not a second input, by the natural key: the party, the fixing URI and the
 * window. The item names the pair it is read for, so the pair is not part of the key.
 */
struct variance_calibration_input final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this input.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party this input belongs to: the system party, as for the feeds.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The oresmd fixing URI of the item that was read, such as
     * =oresmd://fx/USD?type=fixing&index=fx&source=IMF&ccy=AUD=.
     */
    std::string fixing_uri;

    /**
     * @brief The last date of the window: the newest fixing date the capture asked for.
     */
    std::chrono::year_month_day window_end;

    /**
     * @brief The length of the window in calendar days, the policy's variance window at the time of
     * the capture.
     */
    int window_days = 0;

    /**
     * @brief The pair's base currency, in the pair's market convention.
     */
    std::string base_currency_code;

    /**
     * @brief The pair's quote currency.
     */
    std::string quote_currency_code;

    /**
     * @brief The provider whose fixings of the item were read, chosen by the policy's provider
     * order.
     */
    std::string provider_code;

    /**
     * @brief Whether the item quotes the pair the other way round, so each fixing was read as one
     * over its value; the points' returns are in the pair's market convention either way.
     */
    bool inverted = false;

    /**
     * @brief Soft reference to the calibration_policy the item and the window were chosen under.
     */
    boost::uuids::uuid policy_id;

    /**
     * @brief The version of that policy, so a later change to the policy does not rewrite what this
     * input was read under.
     */
    int policy_version = 0;

    /**
     * @brief How many fixing dates the window held, each a point of this input.
     */
    int point_count = 0;

    /**
     * @brief How many of those points gave a return the fit may use.
     */
    int used_count = 0;

    /**
     * @brief Username of the person who last modified this variance calibration input.
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
    friend bool operator==(const variance_calibration_input&,
                           const variance_calibration_input&) = default;
};

/**
 * @brief Dispatch-key identifier for variance_calibration_input, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const variance_calibration_input&) {
    return "ores.synthetic.variance_calibration_input";
}

}

#endif
