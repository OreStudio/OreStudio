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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct tenor_key {
    std::string code;
};

struct tenor_write {
    std::string code;
    std::string display_name;
    std::string description;
    int sort_order;
    std::string kind;
    std::string unit;
    std::optional<int> multiplier;
};

struct tenor_change {
    tenor_write write;
    ores::utility::domain::precondition precondition;
};

struct tenor_removal {
    tenor_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenor_lookup {
    tenor_key key;
    std::optional<ores::refdata::domain::tenor> tenor;
};

struct tenor_event {
    boost::uuids::uuid event_id;
    tenor_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tenor_version_key {
    tenor_key tenor;
    std::uint32_t version;
};

struct tenor_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenors_request {
    using response_type = struct list_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.list";
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

struct list_tenors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor> tenors;
    std::uint64_t total;
};

struct get_tenor_request {
    using response_type = struct get_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_key key;
};

struct get_tenor_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor> tenor;
};

struct get_many_tenors_request {
    using response_type = struct get_many_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_key> keys;
};

struct get_many_tenors_response {
    ores::utility::domain::result result;
    std::vector<tenor_lookup> entries;
};

struct put_tenor_request {
    using response_type = struct put_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenor_response {
    ores::utility::domain::result result;
    ores::refdata::domain::tenor tenor;
};

struct put_many_tenors_request {
    using response_type = struct put_many_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenors_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor> tenors;
};

struct delete_tenor_request {
    using response_type = struct delete_tenor_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenor_response {
    ores::utility::domain::result result;
};

struct delete_many_tenors_request {
    using response_type = struct delete_many_tenors_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenors_response {
    ores::utility::domain::result result;
};

struct list_tenor_versions_request {
    using response_type = struct list_tenor_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_versions_filter> filter;
};

struct list_tenor_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor> versions;
    std::uint64_t total;
};

struct get_tenor_version_request {
    using response_type = struct get_tenor_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenors_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_version_key key;
};

struct get_tenor_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::tenor version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tenor_event_subjects {
inline constexpr std::string_view created = "refdata.v1.tenors_events.created";
inline constexpr std::string_view updated = "refdata.v1.tenors_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.tenors_events.deleted";
}

}

#endif
