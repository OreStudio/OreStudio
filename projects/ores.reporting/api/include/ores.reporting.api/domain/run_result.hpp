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
#ifndef ORES_REPORTING_API_DOMAIN_RUN_RESULT_HPP
#define ORES_REPORTING_API_DOMAIN_RUN_RESULT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief One figure of a run, at the keys and the measure the result screen reads.
 *
 * The read model the result screen reads: one figure of a run, at the trade it is
 * about, named by the measure the screen shows. It is a view over
 * ores_reporting_report_metrics_tbl, so the figures a run produced are stored
 * once, in the shape the readers write, and the screen's shape is a projection of
 * them.
 *
 * The view is where the engine's headings meet the screen's words, and it is the
 * only place that knows either. The engine names the figure NPV, NPV(Base) and
 * Notional; the screen asks for npv, npv_base and notional. A screen that
 * learned the headings would break the day a configuration renamed a column, and a
 * screen that joined the tables itself would state the same rollup twice.
 */
struct run_result final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The metric this row stands for.
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
     * @brief The report the run came from.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief The observation date the figure belongs to.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The kind of record the figure is about: a trade, a netting set, a portfolio.
     */
    std::string subject_type;

    /**
     * @brief The engine's own identifier for that record. It is text because the engine names its
     * records with strings, not with uuids.
     */
    std::string subject_id;

    /**
     * @brief The name the screen knows the figure by: npv, npv_base, notional. The view states
     * which heading each one is.
     */
    std::string measure;

    /**
     * @brief The currency the figure is stated in, empty when it states none.
     */
    std::string currency;

    /**
     * @brief The counterparty the engine stated, when it stated one as a record id.
     */
    std::optional<boost::uuids::uuid> counterparty_id;

    /**
     * @brief The figure, as the engine stated it and never formatted.
     */
    double figure;

    /**
     * @brief The family the figure came from, for provenance.
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
    friend bool operator==(const run_result&, const run_result&) = default;
};

/**
 * @brief Dispatch-key identifier for run_result, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const run_result&) {
    return "ores.reporting.run_result";
}

}

#endif
