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
#ifndef ORES_TRADING_API_MESSAGING_STRUCTURE_KIND_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_STRUCTURE_KIND_PROTOCOL_HPP

#include "ores.trading.api/domain/structure_kind.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct structure_kind_key {
    std::string code;
};

struct structure_kind_write {
    std::string code;
    std::string description;
    bool confirms_as_whole;
};

struct structure_kind_change {
    structure_kind_write write;
    ores::utility::domain::precondition precondition;
};

struct structure_kind_removal {
    structure_kind_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct structure_kind_lookup {
    structure_kind_key key;
    std::optional<ores::trading::domain::structure_kind> structure_kind;
};

struct structure_kinds_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct structure_kind_event {
    boost::uuids::uuid event_id;
    structure_kind_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct structure_kind_version_key {
    structure_kind_key structure_kind;
    std::uint32_t version;
};

struct structure_kind_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_structure_kinds_request {
    using response_type = struct list_structure_kinds_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds.list";
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
    std::optional<structure_kinds_filter> filter;
    std::optional<std::string> as_of;
};

struct list_structure_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_kind> structure_kinds;
    std::uint64_t total;
};

struct get_structure_kind_request {
    using response_type = struct get_structure_kind_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_kind_key key;
};

struct get_structure_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_kind> structure_kind;
};

struct get_many_structure_kinds_request {
    using response_type = struct get_many_structure_kinds_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_kind_key> keys;
};

struct get_many_structure_kinds_response {
    ores::utility::domain::result result;
    std::vector<structure_kind_lookup> entries;
};

struct put_structure_kind_request {
    using response_type = struct put_structure_kind_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_kind_change change;
    ores::utility::domain::change_intent intent;
};

struct put_structure_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_kind> structure_kind;
};

struct put_many_structure_kinds_request {
    using response_type = struct put_many_structure_kinds_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_kind_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_structure_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_kind> structure_kinds;
};

struct delete_structure_kind_request {
    using response_type = struct delete_structure_kind_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_kind_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_structure_kind_response {
    ores::utility::domain::result result;
};

struct delete_many_structure_kinds_request {
    using response_type = struct delete_many_structure_kinds_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_kind_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_structure_kinds_response {
    ores::utility::domain::result result;
};

struct list_structure_kind_versions_request {
    using response_type = struct list_structure_kind_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_kind_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<structure_kind_versions_filter> filter;
};

struct list_structure_kind_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_kind> versions;
    std::uint64_t total;
};

struct get_structure_kind_version_request {
    using response_type = struct get_structure_kind_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_kinds_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_kind_version_key key;
};

struct get_structure_kind_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_kind> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace structure_kind_event_subjects {
inline constexpr std::string_view created = "trading.v1.structure_kinds_events.created";
inline constexpr std::string_view updated = "trading.v1.structure_kinds_events.updated";
inline constexpr std::string_view deleted = "trading.v1.structure_kinds_events.deleted";
}

}

#endif
