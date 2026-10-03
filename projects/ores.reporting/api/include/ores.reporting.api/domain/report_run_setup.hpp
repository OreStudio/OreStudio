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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_RUN_SETUP_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_RUN_SETUP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The ORE run document's Setup block for a report definition.
 *
 * The Setup block of the ORE run document, as typed columns: the dates the run
 * works to, the paths it reads and writes, the flags it runs under, and the
 * configuration files it names. The corpus uses forty of these parameters across
 * all four hundred and sixteen shipped run documents, and asofDate, inputPath,
 * outputPath and logFile appear in every one.
 *
 * Each row is owned by exactly one report_definition (1:1, enforced by the
 * unique index on report_definition_id). It replaces the part of
 * risk_report_config that held a handful of these parameters as columns mixed in
 * with the analytic switches; the switches become rows in report_analytic, and
 * what is left of the run's configuration is here.
 *
 * Values are held as ORE spells them rather than converted, because the run
 * document is written back to ORE and the flags are not all booleans: the
 * fixing and cashflow flags are Y or N, the model flags are true or
 * false, and accrualDate may be the literal ASOF. Converting them would
 * mean inventing a canonical form ORE does not have.
 */
struct report_run_setup final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this setup.
     */
    boost::uuids::uuid id;

    /**
     * @brief The report definition this setup belongs to. Unique per tenant on active records, so a
     * definition has at most one live setup.
     */
    boost::uuids::uuid report_definition_id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The date the run values the portfolio at, as ORE spells it.
     */
    std::optional<std::string> asof_date;

    /**
     * @brief The date accrued interest is measured to, or the literal ASOF.
     */
    std::optional<std::string> accrual_date;

    /**
     * @brief Directory the run reads its inputs from.
     */
    std::optional<std::string> input_path;

    /**
     * @brief Directory the run reads market data from, when it differs from the input path.
     */
    std::optional<std::string> input_path_market;

    /**
     * @brief Directory the run reads the portfolio from, when it differs from the input path.
     */
    std::optional<std::string> input_path_portfolio;

    /**
     * @brief Directory the run writes its output to.
     */
    std::optional<std::string> output_path;

    /**
     * @brief File the run writes its log to.
     */
    std::optional<std::string> log_file;

    /**
     * @brief Bit mask selecting which log categories the run writes.
     */
    std::optional<int> log_mask;

    /**
     * @brief Number of threads the run may use.
     */
    std::optional<int> n_threads;

    /**
     * @brief How the run observes fixings, as ORE spells it.
     */
    std::optional<std::string> observation_model;

    /**
     * @brief Currency the run reports its results in.
     */
    std::optional<std::string> base_currency;

    /**
     * @brief Calendar the run's date arithmetic uses.
     */
    std::optional<std::string> date_calendar;

    /**
     * @brief Business day convention the run's date arithmetic uses, as ORE spells it.
     */
    std::optional<std::string> date_convention;

    /**
     * @brief Latest fixing the run will read, as ORE spells it.
     */
    std::optional<std::string> fixing_cutoff;

    /**
     * @brief Whether the run carries on after an analytic fails, as ORE spells it.
     */
    std::optional<std::string> continue_on_error;

    /**
     * @brief Whether the run builds trades that failed to build, as ORE spells it.
     */
    std::optional<std::string> build_failed_trades;

    /**
     * @brief Whether the run implies today's fixings from the market, as ORE spells it.
     */
    std::optional<std::string> imply_todays_fixings;

    /**
     * @brief Days of fixing lag the run ignores.
     */
    std::optional<int> ignore_fixing_lag;

    /**
     * @brief Whether the run's cashflow report includes today's flows, as ORE spells it.
     */
    std::optional<std::string> include_todays_cash_flows;

    /**
     * @brief Whether the run counts events on the reference date, as ORE spells it.
     */
    std::optional<std::string> include_reference_date_events;

    /**
     * @brief Whether the run builds market data lazily, as ORE spells it.
     */
    std::optional<std::string> lazy_market_building;

    /**
     * @brief Whether the run enriches index fixings, as ORE spells it.
     */
    std::optional<std::string> enrich_index_fixings;

    /**
     * @brief Whether the run uses the analytics block, as ORE spells it.
     */
    std::optional<std::string> use_analytics;

    /**
     * @brief Whether the run writes a comment header into its CSV reports, as ORE spells it.
     */
    std::optional<std::string> csv_comment_report_header;

    /**
     * @brief Whether an unmapped input falls back to the identity mapping, as ORE spells it.
     */
    std::optional<std::string> default_mapping_to_identity;

    /**
     * @brief Whether the run reads portfolios from subdirectories too, as ORE spells it.
     */
    std::optional<std::string> portfolio_recurse_into_sub_directories;

    /**
     * @brief Curve configuration the run reads.
     */
    std::optional<std::string> curve_config_file;

    /**
     * @brief Conventions document the run reads.
     */
    std::optional<std::string> conventions_file;

    /**
     * @brief Market configuration the run reads.
     */
    std::optional<std::string> market_config_file;

    /**
     * @brief Pricing engines the run reads.
     */
    std::optional<std::string> pricing_engines_file;

    /**
     * @brief Pricing engines the run reads for its scenario analytic.
     */
    std::optional<std::string> pricing_engines_file_scenario;

    /**
     * @brief Portfolio the run reads.
     */
    std::optional<std::string> portfolio_file;

    /**
     * @brief Market data the run reads.
     */
    std::optional<std::string> market_data_file;

    /**
     * @brief Mapping the run applies to its market data.
     */
    std::optional<std::string> market_data_mapping_file;

    /**
     * @brief Fixing data the run reads.
     */
    std::optional<std::string> fixing_data_file;

    /**
     * @brief Mapping the run applies to its fixing data.
     */
    std::optional<std::string> fixing_data_mapping_file;

    /**
     * @brief Calendar adjustments the run reads.
     */
    std::optional<std::string> calendar_adjustment;

    /**
     * @brief Currency configuration the run reads.
     */
    std::optional<std::string> currency_configuration;

    /**
     * @brief Reference data the run reads.
     */
    std::optional<std::string> reference_data_file;

    /**
     * @brief Counterparties the run reads.
     */
    std::optional<std::string> counterparty_file;

    /**
     * @brief Script library the run reads.
     */
    std::optional<std::string> script_library;

    /**
     * @brief IBOR fallback configuration the run reads.
     */
    std::optional<std::string> ibor_fallback_config;

    /**
     * @brief Whether the run collects the additional results its configuration asks for, as ORE
     * spells it.
     */
    std::optional<std::string> additional_results;

    /**
     * @brief Username of the person who last modified this report run setup.
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
    friend bool operator==(const report_run_setup&, const report_run_setup&) = default;
};

/**
 * @brief Dispatch-key identifier for report_run_setup, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_run_setup&) {
    return "ores.reporting.report_run_setup";
}

}

#endif
