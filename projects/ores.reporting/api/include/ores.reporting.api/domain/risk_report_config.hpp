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
#ifndef ORES_REPORTING_API_DOMAIN_RISK_REPORT_CONFIG_HPP
#define ORES_REPORTING_API_DOMAIN_RISK_REPORT_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The ORE-level run settings a party shares between its risk report definitions.
 *
 * The ORE-level run settings that risk report definitions of a party share: the base currency, the
 * market data of the run and the folders and log file of the package. The engine settings are not
 * here. They are a named configuration of their own, engine_settings, that the definition binds in
 * its own slot. A report definition binds this configuration through report_configurations, in the
 * slot of configuration type risk_report_config, as it binds its pricing engines. The analytics it
 * runs, and their parameters, are rows of configuration_analytic and
 * configuration_analytic_parameter. Portfolio and book scope live in separate temporal junction
 * tables keyed by this row.
 */
struct risk_report_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns this configuration. Set from the session that writes it, and
     * enforced by row level security, so a party sees only its own configurations.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Human-readable name of this configuration, unique per party. It is also the name of
     * the reporting configuration this row is registered as, which is how a report definition binds
     * it.
     */
    std::string name;

    /**
     * @brief What the configuration runs and for which reports.
     */
    std::string description;

    /**
     * @brief The reporting configuration this row is registered as. A report definition binds that
     * configuration, and so reaches these settings.
     */
    boost::uuids::uuid configuration_id;

    /**
     * @brief Reporting base currency (ISO 4217 code) that all results are converted to. Empty when
     * the run does not state one.
     */
    std::optional<std::string> base_currency;

    /**
     * @brief Market data convention: live, eod or date.
     */
    std::string market_data_type = "eod";

    /**
     * @brief As-of date (ISO 8601) used when market_data_type is date. An empty string is the
     * not-set sentinel: the mapper writes NULL for it, which is what the CHECK requires while
     * market_data_type stays eod.
     */
    std::string market_data_date;

    /**
     * @brief The folder the run reads its input from, relative to the run.
     */
    std::string input_path = "Input";

    /**
     * @brief The folder the run reads market data from, when it differs. Empty when the run does
     * not state it.
     */
    std::optional<std::string> input_path_market;

    /**
     * @brief The folder the run reads the portfolio from, when it differs. Empty when the run does
     * not state it.
     */
    std::optional<std::string> input_path_portfolio;

    /**
     * @brief The folder the run writes its results to, relative to the run.
     */
    std::string output_path = "Output";

    /**
     * @brief The name of the log file.
     */
    std::string log_file = "log.txt";

    /**
     * @brief Whether the portfolio is read from sub directories, as ORE spells it: true or false.
     * Empty when the run does not state it.
     */
    std::optional<std::string> portfolio_recurse_into_sub_directories;

    /**
     * @brief Username of the person who last modified this risk report config.
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
    friend bool operator==(const risk_report_config&, const risk_report_config&) = default;
};

/**
 * @brief Dispatch-key identifier for risk_report_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const risk_report_config&) {
    return "ores.reporting.risk_report_config";
}

}

#endif
