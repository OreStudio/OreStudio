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
#ifndef ORES_REFDATA_API_MESSAGING_LEGAL_ENTITY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_LEGAL_ENTITY_PROTOCOL_HPP

#include "ores.refdata.api/domain/legal_entity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct legal_entity_key {
    std::string lei;
};

struct legal_entity_write {
    std::string lei;
    std::string legal_name;
    std::string jurisdiction;
    std::string entity_status;
    std::string entity_category;
    int display_order;
};

struct legal_entity_change {
    legal_entity_write write;
    ores::utility::domain::precondition precondition;
};

struct legal_entity_removal {
    legal_entity_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct legal_entity_lookup {
    legal_entity_key key;
    std::optional<ores::refdata::domain::legal_entity> legal_entity;
};

struct legal_entities_filter {
    std::optional<std::vector<std::string>> lei_one_of;
};

struct legal_entity_event {
    boost::uuids::uuid event_id;
    legal_entity_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct legal_entity_version_key {
    legal_entity_key legal_entity;
    std::uint32_t version;
};

struct legal_entity_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_legal_entities_request {
    using response_type = struct list_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.list";
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
    std::optional<legal_entities_filter> filter;
    std::optional<std::string> as_of;
};

struct list_legal_entities_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::legal_entity> legal_entities;
    std::uint64_t total;
};

struct get_legal_entity_request {
    using response_type = struct get_legal_entity_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_key key;
};

struct get_legal_entity_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::legal_entity> legal_entity;
};

struct get_many_legal_entities_request {
    using response_type = struct get_many_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<legal_entity_key> keys;
};

struct get_many_legal_entities_response {
    ores::utility::domain::result result;
    std::vector<legal_entity_lookup> entries;
};

struct put_legal_entity_request {
    using response_type = struct put_legal_entity_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_change change;
    ores::utility::domain::change_intent intent;
};

struct put_legal_entity_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::legal_entity> legal_entity;
};

struct put_many_legal_entities_request {
    using response_type = struct put_many_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<legal_entity_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_legal_entities_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::legal_entity> legal_entities;
};

struct delete_legal_entity_request {
    using response_type = struct delete_legal_entity_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_legal_entity_response {
    ores::utility::domain::result result;
};

struct delete_many_legal_entities_request {
    using response_type = struct delete_many_legal_entities_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<legal_entity_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_legal_entities_response {
    ores::utility::domain::result result;
};

struct list_legal_entity_versions_request {
    using response_type = struct list_legal_entity_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<legal_entity_versions_filter> filter;
};

struct list_legal_entity_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::legal_entity> versions;
    std::uint64_t total;
};

struct get_legal_entity_version_request {
    using response_type = struct get_legal_entity_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.legal_entities_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    legal_entity_version_key key;
};

struct get_legal_entity_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::legal_entity> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace legal_entity_event_subjects {
inline constexpr std::string_view created = "refdata.v1.legal_entities_events.created";
inline constexpr std::string_view updated = "refdata.v1.legal_entities_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.legal_entities_events.deleted";
}

}

#endif
