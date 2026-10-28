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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_RESULT_ROW_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_RESULT_ROW_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief One figure a run produced, for one subject in one measure.
 *
 * One figure a run produced, for one subject in one measure, at the run's priced
 * date. The row is written once, with the run, and never edited; a correction is a
 * new run. as_of is the run's priced date, and it is the time column of the
 * series. The canonical row's shape and the reason it is narrow are stated in the
 * report post processing note.
 */
struct report_result_row final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this result row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run that produced the figure.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The run's priced date, and the time column of the series.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The kind of record the figure is about: trade.
     */
    std::string subject_type;

    /**
     * @brief The engine's own identifier for the record.
     */
    boost::uuids::uuid subject_id;

    /**
     * @brief The engine's name for the figure: npv, npv_base, notional, delta, gamma. Open, so a
     * new analytic adds a value rather than a column.
     */
    std::string measure;

    /**
     * @brief The currency the figure is stated in, empty when the measure has none.
     */
    std::string currency;

    /**
     * @brief The report the run came from, so a definition's runs are one query.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief The counterparty the engine stated beside the trade, when it stated one. It is a
     * dimension of the row rather than a measure: the screen groups by it, and reading it here is
     * what spares a request from joining the trading records.
     */
    boost::uuids::uuid counterparty_id;

    /**
     * @brief The figure, as the engine stated it and never formatted.
     */
    double figure;

    /**
     * @brief The file and the row the marshal read, for provenance.
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
    friend bool operator==(const report_result_row&, const report_result_row&) = default;
};

/**
 * @brief Dispatch-key identifier for report_result_row, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_result_row&) {
    return "ores.reporting.report_result_row";
}

}

#endif
