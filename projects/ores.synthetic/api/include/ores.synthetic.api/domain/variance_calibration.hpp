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
#ifndef ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_HPP
#define ORES_SYNTHETIC_API_DOMAIN_VARIANCE_CALIBRATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief One GJR-GARCH shape for a pair, fitted from a stored input or authored, and its review.
 *
 * One GJR-GARCH shape of a pair's daily variance and the evidence a reviewer judges
 * it by. A fitted calibration names the variance_calibration_input it was fitted
 * from and the version of the method that fitted it; an authored one names no input
 * and is reviewed like any other. A feed runs only a calibration that is approved,
 * and names it. See "A calibration is stored and reviewed before a feed runs it" in
 * [[id:351CB816-28F5-428A-A289-414B6F4B9F36][Volatility clustering and GARCH models]].
 *
 * A calibration is proposed when it is stored. A person approves or rejects it and
 * states why in state_reason; approving one makes the pair's earlier approval
 * superseded in the same change, so one approved calibration of a pair is current.
 * Each decision is a new version of the row, so its history shows who decided what
 * and when. A fit that could not be made, for too few returns or a shape at the
 * stationarity bound, is stored as rejected with the reason, so the attempt is kept;
 * such a row may have no shape, so the shape is required, and admissible, in the
 * other three states only. Each bound is written so that a missing value or NaN
 * fails it: a check passes on null, and PostgreSQL orders NaN above every number. A
 * value that is present is finite in every state, rejected included, so the audit
 * trail holds no NaN. The evidence of a fit agrees with its method: an authored shape
 * has no starts, returns or likelihood, and a fit that was not rejected has all three.
 *
 * The checks do not tie persistence to the three parameters, because p_minus is
 * not stored; the fit that writes a calibration owns that equality.
 *
 * persistence is alpha + gamma p_minus + beta, with p_minus the share of the
 * daily return's variance that falls on negative days, so it is stored rather than
 * recomputed from the three parameters alone.
 */
struct variance_calibration final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this calibration.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party: the system party, as for the feeds.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The pair's base currency, held here so the approval index needs no join.
     */
    std::string base_currency_code;

    /**
     * @brief The pair's quote currency.
     */
    std::string quote_currency_code;

    /**
     * @brief The variance_calibration_input this calibration was fitted from; absent for an
     * authored calibration, which reads no input. A foreign key holds a fitted calibration to an
     * input that exists.
     */
    std::optional<boost::uuids::uuid> input_id;

    /**
     * @brief How the shape was obtained: gjr_garch_qmle, variance targeting and Gaussian quasi
     * maximum likelihood over the input's returns, or authored, written by a person.
     */
    std::string method;

    /**
     * @brief The version of the method, so a fit made by an earlier estimator stays distinguishable
     * from one made by a later one.
     */
    int method_version = 0;

    /**
     * @brief The reaction to yesterday's squared return.
     */
    std::optional<double> alpha;

    /**
     * @brief The extra reaction to a negative return, the asymmetry; alpha + gamma is not negative.
     */
    std::optional<double> gamma;

    /**
     * @brief The weight of yesterday's variance.
     */
    std::optional<double> beta;

    /**
     * @brief alpha + gamma p_minus + beta, below one for a stationary shape.
     */
    std::optional<double> persistence;

    /**
     * @brief The unconditional daily variance the history the shape was fitted to returns to,
     * positive. Absent for an authored shape: a feed always takes its long-run level from its own
     * mixture, so the level only describes the fit's data.
     */
    std::optional<double> long_run_variance;

    /**
     * @brief The variance at the end of the input's history over the long-run level: where a feed's
     * first day starts. One for an authored shape.
     */
    std::optional<double> starting_variance_ratio;

    /**
     * @brief The Gaussian log likelihood of the input's returns at the fitted shape; absent for an
     * authored shape.
     */
    std::optional<double> log_likelihood;

    /**
     * @brief How many returns the fit read; zero for an authored shape.
     */
    int returns_used = 0;

    /**
     * @brief How many starting points the simplex was run from; zero for an authored shape.
     */
    int starts = 0;

    /**
     * @brief Whether every start reached the same optimum, within the fit's tolerance; a fit whose
     * starts disagree deserves a closer look before it is approved.
     */
    bool starts_agreed = false;

    /**
     * @brief proposed, approved, rejected or superseded.
     */
    std::string state = "proposed";

    /**
     * @brief Why the calibration is in its state: the reviewer's reason for an approval or a
     * rejection, the refusal of a fit that could not be made, or the calibration that superseded
     * it.
     */
    std::string state_reason = "";

    /**
     * @brief Username of the person who last modified this variance calibration.
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
    friend bool operator==(const variance_calibration&, const variance_calibration&) = default;
};

/**
 * @brief Dispatch-key identifier for variance_calibration, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const variance_calibration&) {
    return "ores.synthetic.variance_calibration";
}

}

#endif
