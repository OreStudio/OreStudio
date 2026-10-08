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
#ifndef ORES_TRADING_API_MESSAGING_STRUCTURE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_STRUCTURE_PROTOCOL_HPP

#include "ores.trading.api/domain/structure.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct structure_key {
    boost::uuids::uuid id;
};

struct structure_write {
    boost::uuids::uuid id;
    boost::uuids::uuid counterparty_id;
    std::string kind;
    std::string template_code;
    boost::uuids::uuid parent_structure_id;
};

struct structure_change {
    structure_write write;
    ores::utility::domain::precondition precondition;
};

struct structure_removal {
    structure_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct structure_lookup {
    structure_key key;
    std::optional<ores::trading::domain::structure> structure;
};

struct structures_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct structure_event {
    boost::uuids::uuid event_id;
    structure_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct structure_version_key {
    structure_key structure;
    std::uint32_t version;
};

struct structure_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_structures_request {
    using response_type = struct list_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.list";
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
    std::optional<structures_filter> filter;
    std::optional<std::string> as_of;
};

struct list_structures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure> structures;
    std::uint64_t total;
};

struct get_structure_request {
    using response_type = struct get_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_key key;
};

struct get_structure_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure> structure;
};

struct get_many_structures_request {
    using response_type = struct get_many_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_key> keys;
};

struct get_many_structures_response {
    ores::utility::domain::result result;
    std::vector<structure_lookup> entries;
};

struct put_structure_request {
    using response_type = struct put_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_change change;
    ores::utility::domain::change_intent intent;
};

struct put_structure_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure> structure;
};

struct put_many_structures_request {
    using response_type = struct put_many_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_structures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure> structures;
};

struct delete_structure_request {
    using response_type = struct delete_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_structure_response {
    ores::utility::domain::result result;
};

struct delete_many_structures_request {
    using response_type = struct delete_many_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_structures_response {
    ores::utility::domain::result result;
};

struct list_structure_versions_request {
    using response_type = struct list_structure_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<structure_versions_filter> filter;
};

struct list_structure_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure> versions;
    std::uint64_t total;
};

struct get_structure_version_request {
    using response_type = struct get_structure_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.structures_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_version_key key;
};

struct get_structure_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace structure_event_subjects {
inline constexpr std::string_view created = "trading.v1.structures_events.created";
inline constexpr std::string_view updated = "trading.v1.structures_events.updated";
inline constexpr std::string_view deleted = "trading.v1.structures_events.deleted";
}

}

#endif
