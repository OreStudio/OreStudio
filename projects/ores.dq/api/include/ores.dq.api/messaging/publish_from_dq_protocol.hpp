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
#ifndef ORES_DQ_API_MESSAGING_PUBLISH_FROM_DQ_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_PUBLISH_FROM_DQ_PROTOCOL_HPP

#include <cstdint>
#include <string>

namespace ores::dq::messaging {

/**
 * @brief Command payload sent by the DQ workflow engine to a target service's
 *        publish-from-dq NATS subject.
 *
 * The receiving handler calls a SECURITY DEFINER SQL function that reads the
 * DQ artefact table for dataset_id and writes to the target service's tables.
 */
struct publish_from_dq_command {
    /**
     * @brief UUID of the DQ dataset to publish.
     */
    std::string dataset_id;
    /**
     * @brief UUID of the target tenant.
     */
    std::string tenant_id;
    /**
     * @brief How the target writes the rows: upsert, replace_all or insert_only.
     */
    std::string mode;
    /**
     * @brief Extra per-artefact parameters, as JSON. May be an empty object.
     */
    std::string params_json;
};

/**
 * @brief Result returned by the target service's publish-from-dq handler.
 *
 * Serialised as JSON and passed to wf->complete() so the workflow engine can
 * record counts and propagate them to the bundle publish result.
 */
struct publish_from_dq_result {
    /**
     * @brief Whether the publication completed.
     */
    bool success = false;
    /**
     * @brief Why it failed, when it did.
     */
    std::string error_message;
    /**
     * @brief How many rows the publication inserted.
     */
    std::uint64_t records_inserted = 0;
    /**
     * @brief How many rows the publication updated.
     */
    std::uint64_t records_updated = 0;
    /**
     * @brief How many rows the publication left alone.
     */
    std::uint64_t records_skipped = 0;
    /**
     * @brief How many rows the publication removed.
     */
    std::uint64_t records_deleted = 0;
};

/**
 * @brief The workflow step that publishes the DQ-cleared ip2country.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is ores_dq_ip2country_publish_fn.
 */
struct publish_ip2country_from_dq_request {
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_ip2country_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes the DQ-cleared coding schemes.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is ores_dq_coding_schemes_publish_fn.
 */
struct publish_coding_schemes_from_dq_request {
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_coding_schemes_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes the DQ-cleared badge severities.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is ores_dq_badge_severities_publish_fn.
 */
struct publish_badge_severities_from_dq_request {
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_badge_severities_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes the DQ-cleared badge definitions.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is ores_dq_badge_definitions_publish_fn.
 */
struct publish_badge_definitions_from_dq_request {
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_badge_definitions_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes the DQ-cleared code domains.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is ores_dq_code_domains_publish_fn.
 */
struct publish_code_domains_from_dq_request {
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_code_domains_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The workflow step that publishes the DQ-cleared badge mappings.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is ores_dq_badge_mappings_publish_fn.
 */
struct publish_badge_mappings_from_dq_request {
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_badge_mappings_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

}

#endif
