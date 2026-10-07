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
};

struct trigger_report_instance_response {
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
    int trade_count = 0;
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

}

#endif
