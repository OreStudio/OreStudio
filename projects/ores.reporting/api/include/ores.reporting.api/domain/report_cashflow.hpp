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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_CASHFLOW_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_CASHFLOW_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief One figure of one cashflow a run produced, with the keys that name the cashflow.
 *
 * A cashflow is not a metric. A metric states one figure about a subject; a
 * cashflow states a row per payment, keyed by the leg and the payment number, and
 * the stressed families state each of its figures three times, as a base, a
 * scenario and the difference between them.
 *
 * The row here is therefore narrow, and keeps the cashflow's own keys: one figure,
 * named by its heading, with the part of the stressed comparison it states. A
 * family of cashflows adds no column to this table, and no figure of a cashflow is
 * flattened into report_metric, because the keys that make a cashflow a cashflow
 * have no place in a metric.
 *
 * The row is written once, with the run, and never edited; a correction is a new
 * run.
 */
struct report_cashflow final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this cashflow figure.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run that produced the cashflow.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The family the cashflow came from: flows, stresstest_flows, or the calibration's
     * cashflows. It says which reader read the row.
     */
    std::string family;

    /**
     * @brief The kind of record the cashflow belongs to: a trade, or a market object.
     */
    std::string subject_type;

    /**
     * @brief The engine's own identifier for that record. It is text because the engine names its
     * records with strings, not with uuids.
     */
    std::string subject_id;

    /**
     * @brief The scenario the cashflow was stressed under, empty when the file states none.
     */
    std::string scenario;

    /**
     * @brief The engine's number for the payment within its leg.
     */
    std::string cashflow_no;

    /**
     * @brief The engine's number for the leg the payment belongs to.
     */
    std::string leg_no;

    /**
     * @brief The observation date the cashflow belongs to: the run's priced date when the file
     * states none, and the row's own date when it does.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The engine's name for the figure, which is its heading: Amount, Coupon,
     * PresentValue(Base).
     */
    std::string measure;

    /**
     * @brief Which part of a stressed comparison the figure is: base, scen or diff. A family that
     * states one figure for each payment leaves it empty.
     */
    std::string part;

    /**
     * @brief The report the run came from, so a definition's runs are one query.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief The day the payment falls due.
     */
    std::optional<std::chrono::system_clock::time_point> pay_date;

    /**
     * @brief The engine's name for the kind of payment, such as Interest or Notional.
     */
    std::string flow_type;

    /**
     * @brief The currency the cashflow is stated in.
     */
    std::string currency;

    /**
     * @brief Every other qualifier the file stated, as a JSON object of name and value, so a family
     * gains no column in the table: the accrual period, the fixing date, the strike and the
     * volatility the payment was calculated at all travel here.
     */
    std::string qualifiers;

    /**
     * @brief The figure, as the engine stated it and never formatted.
     */
    double value;

    /**
     * @brief The family and the row the reader read, for provenance.
     */
    std::string source;

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
    friend bool operator==(const report_cashflow&, const report_cashflow&) = default;
};

/**
 * @brief Dispatch-key identifier for report_cashflow, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_cashflow&) {
    return "ores.reporting.report_cashflow";
}

}

#endif
