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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_RUN_SETUP_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_RUN_SETUP_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_run_setup.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_run_setup_key {
    boost::uuids::uuid id;
};

struct report_run_setup_write {
    boost::uuids::uuid id;
    boost::uuids::uuid report_definition_id;
    std::optional<std::string> asof_date;
    std::optional<std::string> accrual_date;
    std::optional<std::string> input_path;
    std::optional<std::string> input_path_market;
    std::optional<std::string> input_path_portfolio;
    std::optional<std::string> output_path;
    std::optional<std::string> log_file;
    std::optional<int> log_mask;
    std::optional<int> n_threads;
    std::optional<std::string> observation_model;
    std::optional<std::string> base_currency;
    std::optional<std::string> date_calendar;
    std::optional<std::string> date_convention;
    std::optional<std::string> fixing_cutoff;
    std::optional<std::string> continue_on_error;
    std::optional<std::string> build_failed_trades;
    std::optional<std::string> imply_todays_fixings;
    std::optional<int> ignore_fixing_lag;
    std::optional<std::string> include_todays_cash_flows;
    std::optional<std::string> include_reference_date_events;
    std::optional<std::string> lazy_market_building;
    std::optional<std::string> enrich_index_fixings;
    std::optional<std::string> use_analytics;
    std::optional<std::string> csv_comment_report_header;
    std::optional<std::string> default_mapping_to_identity;
    std::optional<std::string> portfolio_recurse_into_sub_directories;
    std::optional<std::string> curve_config_file;
    std::optional<std::string> conventions_file;
    std::optional<std::string> market_config_file;
    std::optional<std::string> pricing_engines_file;
    std::optional<std::string> pricing_engines_file_scenario;
    std::optional<std::string> portfolio_file;
    std::optional<std::string> market_data_file;
    std::optional<std::string> market_data_mapping_file;
    std::optional<std::string> fixing_data_file;
    std::optional<std::string> fixing_data_mapping_file;
    std::optional<std::string> calendar_adjustment;
    std::optional<std::string> currency_configuration;
    std::optional<std::string> reference_data_file;
    std::optional<std::string> counterparty_file;
    std::optional<std::string> script_library;
    std::optional<std::string> ibor_fallback_config;
    std::optional<std::string> additional_results;
};

struct report_run_setup_change {
    report_run_setup_write write;
    ores::utility::domain::precondition precondition;
};

struct report_run_setup_removal {
    report_run_setup_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_run_setup_lookup {
    report_run_setup_key key;
    std::optional<ores::reporting::domain::report_run_setup> report_run_setup;
};

struct report_run_setups_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct report_run_setup_event {
    boost::uuids::uuid event_id;
    report_run_setup_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct report_run_setup_version_key {
    report_run_setup_key report_run_setup;
    std::uint32_t version;
};

struct report_run_setup_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_report_run_setups_request {
    using response_type = struct list_report_run_setups_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_run_setups_filter> filter;
};

struct list_report_run_setups_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_run_setup> setups;
    std::uint64_t total;
};

struct get_report_run_setup_request {
    using response_type = struct get_report_run_setup_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_run_setup_key key;
};

struct get_report_run_setup_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_run_setup> report_run_setup;
};

struct get_many_report_run_setups_request {
    using response_type = struct get_many_report_run_setups_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_run_setup_key> keys;
};

struct get_many_report_run_setups_response {
    ores::utility::domain::result result;
    std::vector<report_run_setup_lookup> entries;
};

struct put_report_run_setup_request {
    using response_type = struct put_report_run_setup_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_run_setup_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_run_setup_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_run_setup> report_run_setup;
};

struct put_many_report_run_setups_request {
    using response_type = struct put_many_report_run_setups_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_run_setup_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_run_setups_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_run_setup> setups;
};

struct delete_report_run_setup_request {
    using response_type = struct delete_report_run_setup_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_run_setup_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_run_setup_response {
    ores::utility::domain::result result;
};

struct delete_many_report_run_setups_request {
    using response_type = struct delete_many_report_run_setups_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_run_setup_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_run_setups_response {
    ores::utility::domain::result result;
};

struct list_report_run_setup_versions_request {
    using response_type = struct list_report_run_setup_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_run_setup_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_run_setup_versions_filter> filter;
};

struct list_report_run_setup_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_run_setup> versions;
    std::uint64_t total;
};

struct get_report_run_setup_version_request {
    using response_type = struct get_report_run_setup_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_run_setups_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_run_setup_version_key key;
};

struct get_report_run_setup_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_run_setup> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_run_setup_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_run_setups_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_run_setups_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_run_setups_events.deleted";
}

}

#endif
