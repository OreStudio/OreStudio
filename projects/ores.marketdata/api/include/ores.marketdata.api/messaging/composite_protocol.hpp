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
#ifndef ORES_MARKETDATA_API_MESSAGING_COMPOSITE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_COMPOSITE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/composite.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

struct composite_key {
    std::string name;
};

struct composite_write {
    boost::uuids::uuid id;
    std::string name;
    std::string provider_code;
    std::string oresmd_uri;
    std::string rule_code;
    std::string description;
};

struct composite_change {
    composite_write write;
    ores::utility::domain::precondition precondition;
};

struct composite_removal {
    composite_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct composite_lookup {
    composite_key key;
    std::optional<ores::marketdata::domain::composite> composite;
};

struct composites_filter {
    std::optional<std::string> provider_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;
    std::optional<std::string> search;
};

struct composite_event {
    boost::uuids::uuid event_id;
    composite_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct composite_version_key {
    composite_key composite;
    std::uint32_t version;
};

struct composite_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_composites_request {
    using response_type = struct list_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.list";
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
    std::optional<composites_filter> filter;
    std::optional<std::string> as_of;
};

struct list_composites_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite> composites;
    std::uint64_t total;
};

struct get_composite_request {
    using response_type = struct get_composite_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_key key;
};

struct get_composite_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite> composite;
};

struct get_many_composites_request {
    using response_type = struct get_many_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_key> keys;
};

struct get_many_composites_response {
    ores::utility::domain::result result;
    std::vector<composite_lookup> entries;
};

struct put_composite_request {
    using response_type = struct put_composite_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_change change;
    ores::utility::domain::change_intent intent;
};

struct put_composite_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite> composite;
};

struct put_many_composites_request {
    using response_type = struct put_many_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_composites_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite> composites;
};

struct delete_composite_request {
    using response_type = struct delete_composite_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_composite_response {
    ores::utility::domain::result result;
};

struct delete_many_composites_request {
    using response_type = struct delete_many_composites_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<composite_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_composites_response {
    ores::utility::domain::result result;
};

struct list_composite_versions_request {
    using response_type = struct list_composite_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<composite_versions_filter> filter;
};

struct list_composite_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::composite> versions;
    std::uint64_t total;
};

struct get_composite_version_request {
    using response_type = struct get_composite_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.composites_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    composite_version_key key;
};

struct get_composite_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::composite> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace composite_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.composites_events.created";
inline constexpr std::string_view updated = "marketdata.v1.composites_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.composites_events.deleted";
}

}

#endif
