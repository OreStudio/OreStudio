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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_OPERATIONS_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_OPERATIONS_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct trigger_report_instance_request {
    using response_type = struct trigger_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.trigger_report_instance";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid report_definition_id;
    boost::uuids::uuid tenant_id;
    std::int64_t job_instance_id = 0;
    std::string kind = "production";
};

struct trigger_report_instance_response {
    ores::utility::domain::result result;
    std::string report_instance_id;
};

/**
 * @brief Reads one run's health word and the state it was derived from.
 *
 * The run is named rather than scoped to the caller's party: the answer is
 * scoped by row-level security on the tables, as the web's read of the same
 * runs is.
 */
struct get_run_health_request {
    using response_type = struct get_run_health_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.get_run_health";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The run to state the health of. */
    boost::uuids::uuid report_instance_id;
};

struct get_run_health_response {
    ores::utility::domain::result result;
    /**
     * @brief The name of the run's lifecycle state, as the store holds it.
     *
     * Empty when the run has no state, or when the session cannot read the
     * state. The word is then empty too, so a caller is told what the word was
     * derived from rather than left guessing.
     */
    std::string state_name;
    /**
     * @brief One of =failed=, =completed with errors=, =running= or
     * =completed clean=, or empty when the state has no word.
     *
     * =never run= is not a word here: a run that never started has no run to
     * ask about.
     */
    std::string health;
};

/**
 * @brief Records the date a run priced at, and why it moved when it did.
 *
 * The packaging step is the only place that knows both dates: the run document
 * asks for one, and the gathered market data carries another. The step states
 * the date it used on the run itself, so the screen that shows the run can say
 * which date it priced at without parsing a message. The run is named rather
 * than scoped to the caller's party: the write is scoped by row-level security
 * on the table, as the run reads are.
 */
struct record_run_pricing_request {
    using response_type = struct record_run_pricing_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.record_run_pricing";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The run the date was priced for. */
    boost::uuids::uuid report_instance_id;
    /**
     * @brief The date the run priced at, as an ISO 8601 date (YYYY-MM-DD).
     *
     * A date and not a timestamp: the engine prices one day, and the time of
     * day the packaging step ran is not part of the answer.
     */
    std::string priced_at;
    /**
     * @brief Why the run priced at that date, in the step's own words.
     *
     * Empty when the run priced at the date it asked for.
     */
    std::string price_note;
};

struct record_run_pricing_response {
    ores::utility::domain::result result;
};

struct schedule_report_definitions_request {
    using response_type = struct schedule_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.schedule_report_definitions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> ids;
};

struct schedule_report_definitions_response {
    bool success = false;
    std::string message;
    int scheduled_count = 0;
    std::vector<std::string> failed_ids;
};

struct unschedule_report_definitions_request {
    using response_type = struct unschedule_report_definitions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.unschedule_report_definitions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> ids;
};

struct unschedule_report_definitions_response {
    bool success = false;
    std::string message;
    int unscheduled_count = 0;
    std::vector<std::string> failed_ids;
};

/**
 * @brief The workflow step that publishes a DQ-cleared report definitions bundle.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply, so it states a subject and no response. Its body is the DQ artefact
 * the server-side function knows how to expand, which is why it declares no
 * fields. It is declared here, and referenced rather than spelled out, because
 * the SQL function name the handler derives from it depends on the spelling.
 */
struct publish_report_definitions_from_dq_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.publish_report_definitions_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes a DQ-cleared risk report configs bundle.
 *
 * The same trigger shape as publish_report_definitions_from_dq_request, and
 * declared the same way: the SQL function name the handler derives from the
 * subject depends on the spelling.
 */
struct publish_risk_report_configs_from_dq_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.publish_risk_report_configs_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes a DQ-cleared engine settings bundle.
 *
 * The same trigger shape as publish_risk_report_configs_from_dq_request, and
 * declared the same way: the SQL function name the handler derives from the
 * subject depends on the spelling.
 */
struct publish_engine_settings_from_dq_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.publish_engine_settings_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes the DQ-cleared ORE parameter value domains.
 *
 * The same trigger shape as publish_engine_settings_from_dq_request, and
 * declared the same way: the SQL function name the handler derives from the
 * subject depends on the spelling.
 */
struct publish_ore_parameter_value_domains_from_dq_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.publish_ore_parameter_value_domains_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes the DQ-cleared ORE parameter documentation.
 *
 * The same trigger shape as publish_engine_settings_from_dq_request. The
 * value domains publish first, because the dataset depends on theirs.
 */
struct publish_ore_parameters_from_dq_request {
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.publish_ore_parameters_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct gather_trades_request {
    using response_type = struct gather_trades_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.gather_trades";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string correlation_id;
};

struct gather_trades_result {
    bool success = false;
    std::string message;
    int trade_count = 0;
    std::string storage_key;
    /**
     * @brief The version of each trade the export wrote, as a JSON object keyed by
     * trade id. Empty when the export wrote none.
     *
     * The trades themselves stay in object storage, so only this manifest travels
     * with the run. A command line cannot fill a map, and the run carries the
     * manifest to the bundle unchanged, so the JSON text is what both messages
     * state.
     */
    std::string trade_versions;
};

struct gather_market_data_request {
    using response_type = struct gather_market_data_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.gather_market_data";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string correlation_id;
};

struct gather_market_data_result {
    bool success = false;
    std::string message;
    int series_count = 0;
    std::string storage_key;
    /**
     * @brief Where the gather step put the fixings body.
     *
     * The engine reads fixings through a file its run document names, and the
     * export writes them apart from the market data, so the run carries the key
     * the packaging step fetches it from.
     */
    std::string fixings_storage_key;
};

struct assemble_bundle_request {
    using response_type = struct assemble_bundle_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.assemble_bundle";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string trades_storage_key;
    std::string market_data_storage_key;
    /**
     * @brief Where the gather step put the fixings body.
     *
     * The engine reads fixings through a file its run document names, and the
     * export writes them apart from the market data, so the run carries the key
     * the packaging step fetches it from.
     */
    std::string fixings_storage_key;
    int trade_count = 0;
    /**
     * @brief The version of each trade the run read, as the JSON object the gather
     * step received. Empty when the run read no trades.
     *
     * The bundle stores it in its own column, because an error the engine raises for
     * a trade names the version of the trade the engine was given.
     */
    std::string trade_versions = "";
    int series_count = 0;
};

struct assemble_bundle_result {
    bool success = false;
    std::string message;
    std::string bundle_id;
};

struct prepare_ore_package_request {
    using response_type = struct prepare_ore_package_result;
    static constexpr std::string_view nats_subject = "ore.v1.ops.prepare_ore_package";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string definition_id;
    std::string bundle_id;
    std::string tenant_id;
    std::string party_id;
    std::string run_grant_id;
    std::string correlation_id;
    std::string trades_storage_key;
    std::string market_data_storage_key;
    /**
     * @brief Where the run's fixings body is, so the package can place it where
     * the engine reads it.
     */
    std::string fixings_storage_key;
};

struct prepare_ore_package_result {
    bool success = false;
    std::string message;
    std::vector<std::string> tarball_uris;
};

struct submit_compute_request {
    using response_type = struct submit_compute_result;
    static constexpr std::string_view nats_subject = "compute.v1.ops.submit_compute";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string tenant_id;
    std::string party_id;
    std::string run_grant_id;
    std::string correlation_id;
    std::vector<std::string> tarball_uris;
};

struct submit_compute_result {
    bool success = false;
    std::string message;
    std::string batch_id;
};

struct collect_compute_results_request {
    using response_type = struct collect_compute_results_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.collect_compute_results";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string batch_id;
};

struct collect_compute_results_result {
    bool success = false;
    std::string message;
};

struct post_process_results_request {
    using response_type = struct post_process_results_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.post_process_results";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string batch_id;
};

struct post_process_results_result {
    bool success = false;
    std::string message;
    int rows_loaded = 0;
};

struct finalise_report_request {
    using response_type = struct finalise_report_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.finalise_report";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
};

struct finalise_report_result {
    bool success = false;
    std::string message;
};

struct fail_report_request {
    using response_type = struct fail_report_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.fail_report";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string error_message;
};

struct fail_report_result {
    bool success = false;
    std::string message;
};

struct report_execution_request {
    std::string report_instance_id;
    std::string definition_id;
    std::string tenant_id;
    std::string party_id;
    std::string run_grant_id;
    std::string correlation_id;
    std::string pre_processing;
    std::string prepared_input_key;
    std::string post_processing;
    std::optional<std::string> kind;
};

struct resolve_prepared_input_request {
    using response_type = struct prepare_ore_package_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.resolve_prepared_input";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string prepared_input_key;
};

struct ignore_compute_results_request {
    using response_type = struct collect_compute_results_result;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.ignore_compute_results";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string report_instance_id;
    std::string tenant_id;
    std::string correlation_id;
    std::string batch_id;
};

struct definition_next_fire {
    boost::uuids::uuid definition_id;
    std::optional<std::string> next_fire_at;
};

struct get_next_fire_request {
    using response_type = struct get_next_fire_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.get_next_fire";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> definition_ids;
};

struct get_next_fire_response {
    ores::utility::domain::result result;
    std::vector<definition_next_fire> schedules;
};

}

#endif
