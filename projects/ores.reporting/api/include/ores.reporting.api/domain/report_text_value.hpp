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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_TEXT_VALUE_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_TEXT_VALUE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief One value a run stated as text: an additional result, a portfolio detail or the calibrated
 * model.
 *
 * One value a run stated as text rather than as a figure. Some outputs say what a
 * run was about rather than what it computed: the pricing engine and model a trade
 * used, the market objects and risk factors a portfolio depends on, the indices and
 * counterparties it names, and the calibrated model. Each value is a name and its
 * text, about a trade when the file names one, with the number beside it when the
 * engine stated the value as a double.
 *
 * The row is written once, with the run, and never edited; a correction is a new
 * run.
 */
struct report_text_value final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this value.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run that stated the value.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The compute result whose output states the value.
     */
    boost::uuids::uuid result_id;

    /**
     * @brief The file the reader read, as the engine named it.
     */
    std::string source;

    /**
     * @brief The line of the file that states the value, counting the heading as line one.
     */
    int line = 0;

    /**
     * @brief The run's priced date: these files state no date of their own.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The report the run came from, so a definition's runs are one query.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief The family the value came from, as the catalogue names it: additional_results,
     * marketObjects, calibration.
     */
    std::string family;

    /**
     * @brief The kind of record the value is about: trade, or empty when the file names none.
     */
    std::string subject_type;

    /**
     * @brief The engine's identifier for that record, such as a trade id, empty when the file names
     * none.
     */
    std::string subject_id;

    /**
     * @brief What the value is, as the engine names it: a result such as PricingConfigEngine, a
     * market object type such as DiscountCurve, or the heading of a list.
     */
    std::string name;

    /**
     * @brief The type the engine stated for the value, such as string, double or cashflows, or what
     * the file implies: text for a list, xml for the calibrated model.
     */
    std::string value_type;

    /**
     * @brief The value as the engine wrote it.
     */
    std::string value;

    /**
     * @brief The value as a number, when the engine stated it as a double.
     */
    std::optional<double> figure;

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
    friend bool operator==(const report_text_value&, const report_text_value&) = default;
};

/**
 * @brief Dispatch-key identifier for report_text_value, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_text_value&) {
    return "ores.reporting.report_text_value";
}

}

#endif
