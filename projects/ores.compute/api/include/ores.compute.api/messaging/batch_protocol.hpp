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
#ifndef ORES_COMPUTE_API_MESSAGING_BATCH_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_BATCH_PROTOCOL_HPP

#include "ores.compute.api/domain/batch.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct batch_key {
    std::string external_ref;
};

struct batch_write {
    boost::uuids::uuid id;
    std::string external_ref;
    std::string status;
};

struct batch_change {
    batch_write write;
    ores::utility::domain::precondition precondition;
};

struct batch_removal {
    batch_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct batch_lookup {
    batch_key key;
    std::optional<ores::compute::domain::batch> batch;
};

struct batch_event {
    boost::uuids::uuid event_id;
    batch_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct batch_version_key {
    batch_key batch;
    std::uint32_t version;
};

struct batch_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_batches_request {
    using response_type = struct list_batches_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.list";
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

struct list_batches_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::batch> batches;
    std::uint64_t total;
};

struct get_batch_request {
    using response_type = struct get_batch_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    batch_key key;
};

struct get_batch_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::batch> batch;
};

struct get_many_batches_request {
    using response_type = struct get_many_batches_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<batch_key> keys;
};

struct get_many_batches_response {
    ores::utility::domain::result result;
    std::vector<batch_lookup> entries;
};

struct put_batch_request {
    using response_type = struct put_batch_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    batch_change change;
    ores::utility::domain::change_intent intent;
};

struct put_batch_response {
    ores::utility::domain::result result;
    ores::compute::domain::batch batch;
};

struct put_many_batches_request {
    using response_type = struct put_many_batches_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<batch_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_batches_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::batch> batches;
};

struct delete_batch_request {
    using response_type = struct delete_batch_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    batch_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_batch_response {
    ores::utility::domain::result result;
};

struct delete_many_batches_request {
    using response_type = struct delete_many_batches_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<batch_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_batches_response {
    ores::utility::domain::result result;
};

struct list_batch_versions_request {
    using response_type = struct list_batch_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    batch_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<batch_versions_filter> filter;
};

struct list_batch_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::batch> versions;
    std::uint64_t total;
};

struct get_batch_version_request {
    using response_type = struct get_batch_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.batches_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    batch_version_key key;
};

struct get_batch_version_response {
    ores::utility::domain::result result;
    ores::compute::domain::batch version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace batch_event_subjects {
inline constexpr std::string_view created = "compute.v1.batches_events.created";
inline constexpr std::string_view updated = "compute.v1.batches_events.updated";
inline constexpr std::string_view deleted = "compute.v1.batches_events.deleted";
}

}

#endif
