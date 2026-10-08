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
#ifndef ORES_TRADING_API_MESSAGING_STRUCTURE_TEMPLATE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_STRUCTURE_TEMPLATE_PROTOCOL_HPP

#include "ores.trading.api/domain/structure_template.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct structure_template_key {
    std::string code;
};

struct structure_template_write {
    std::string code;
    std::string description;
    std::string kind;
};

struct structure_template_change {
    structure_template_write write;
    ores::utility::domain::precondition precondition;
};

struct structure_template_removal {
    structure_template_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct structure_template_lookup {
    structure_template_key key;
    std::optional<ores::trading::domain::structure_template> structure_template;
};

struct structure_templates_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct structure_template_event {
    boost::uuids::uuid event_id;
    structure_template_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct structure_template_version_key {
    structure_template_key structure_template;
    std::uint32_t version;
};

struct structure_template_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_structure_templates_request {
    using response_type = struct list_structure_templates_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates.list";
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
    std::optional<structure_templates_filter> filter;
    std::optional<std::string> as_of;
};

struct list_structure_templates_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_template> structure_templates;
    std::uint64_t total;
};

struct get_structure_template_request {
    using response_type = struct get_structure_template_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_template_key key;
};

struct get_structure_template_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_template> structure_template;
};

struct get_many_structure_templates_request {
    using response_type = struct get_many_structure_templates_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_template_key> keys;
};

struct get_many_structure_templates_response {
    ores::utility::domain::result result;
    std::vector<structure_template_lookup> entries;
};

struct put_structure_template_request {
    using response_type = struct put_structure_template_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_template_change change;
    ores::utility::domain::change_intent intent;
};

struct put_structure_template_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_template> structure_template;
};

struct put_many_structure_templates_request {
    using response_type = struct put_many_structure_templates_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_template_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_structure_templates_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_template> structure_templates;
};

struct delete_structure_template_request {
    using response_type = struct delete_structure_template_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_template_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_structure_template_response {
    ores::utility::domain::result result;
};

struct delete_many_structure_templates_request {
    using response_type = struct delete_many_structure_templates_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<structure_template_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_structure_templates_response {
    ores::utility::domain::result result;
};

struct list_structure_template_versions_request {
    using response_type = struct list_structure_template_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_template_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<structure_template_versions_filter> filter;
};

struct list_structure_template_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::structure_template> versions;
    std::uint64_t total;
};

struct get_structure_template_version_request {
    using response_type = struct get_structure_template_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.structure_templates_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    structure_template_version_key key;
};

struct get_structure_template_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::structure_template> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace structure_template_event_subjects {
inline constexpr std::string_view created = "trading.v1.structure_templates_events.created";
inline constexpr std::string_view updated = "trading.v1.structure_templates_events.updated";
inline constexpr std::string_view deleted = "trading.v1.structure_templates_events.deleted";
}

}

#endif
