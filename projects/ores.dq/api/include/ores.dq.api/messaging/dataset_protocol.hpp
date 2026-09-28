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
#ifndef ORES_DQ_API_MESSAGING_DATASET_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATASET_PROTOCOL_HPP

#include "ores.dq.api/domain/dataset.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct dataset_key {
    std::string code;
};

struct dataset_write {
    boost::uuids::uuid id;
    std::string code;
    std::optional<std::string> catalog_name;
    std::string subject_area_name;
    std::string domain_name;
    std::optional<std::string> coding_scheme_code;
    std::string origin_code;
    std::string nature_code;
    std::string treatment_code;
    std::optional<boost::uuids::uuid> methodology_id;
    std::string name;
    std::string description;
    std::string source_system_id;
    std::string business_context;
    std::optional<boost::uuids::uuid> upstream_derivation_id;
    int lineage_depth;
    std::chrono::system_clock::time_point as_of_date;
    std::chrono::system_clock::time_point ingestion_timestamp;
    std::optional<std::string> license_info;
    std::string artefact_type;
};

struct dataset_change {
    dataset_write write;
    ores::utility::domain::precondition precondition;
};

struct dataset_removal {
    dataset_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct dataset_lookup {
    dataset_key key;
    std::optional<ores::dq::domain::dataset> dataset;
};

struct dataset_event {
    boost::uuids::uuid event_id;
    dataset_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct dataset_version_key {
    dataset_key dataset;
    std::uint32_t version;
};

struct dataset_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_datasets_request {
    using response_type = struct list_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.list";
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
};

struct list_datasets_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset> datasets;
    std::uint64_t total;
};

struct get_dataset_request {
    using response_type = struct get_dataset_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_key key;
};

struct get_dataset_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset> dataset;
};

struct get_many_datasets_request {
    using response_type = struct get_many_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_key> keys;
};

struct get_many_datasets_response {
    ores::utility::domain::result result;
    std::vector<dataset_lookup> entries;
};

struct put_dataset_request {
    using response_type = struct put_dataset_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_change change;
    ores::utility::domain::change_intent intent;
};

struct put_dataset_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset> dataset;
};

struct put_many_datasets_request {
    using response_type = struct put_many_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_datasets_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset> datasets;
};

struct delete_dataset_request {
    using response_type = struct delete_dataset_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_dataset_response {
    ores::utility::domain::result result;
};

struct delete_many_datasets_request {
    using response_type = struct delete_many_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<dataset_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_datasets_response {
    ores::utility::domain::result result;
};

struct list_dataset_versions_request {
    using response_type = struct list_dataset_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<dataset_versions_filter> filter;
};

struct list_dataset_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::dataset> versions;
    std::uint64_t total;
};

struct get_dataset_version_request {
    using response_type = struct get_dataset_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.datasets_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    dataset_version_key key;
};

struct get_dataset_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::dataset> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace dataset_event_subjects {
inline constexpr std::string_view created = "dq.v1.datasets_events.created";
inline constexpr std::string_view updated = "dq.v1.datasets_events.updated";
inline constexpr std::string_view deleted = "dq.v1.datasets_events.deleted";
}

}

#endif
