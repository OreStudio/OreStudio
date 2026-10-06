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
#ifndef ORES_REFDATA_API_MESSAGING_CURVE_SECTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURVE_SECTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/curve_section.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct curve_section_key {
    std::string code;
};

struct curve_section_write {
    std::string code;
    std::string entry_element;
    std::string description;
};

struct curve_section_change {
    curve_section_write write;
    ores::utility::domain::precondition precondition;
};

struct curve_section_removal {
    curve_section_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct curve_section_lookup {
    curve_section_key key;
    std::optional<ores::refdata::domain::curve_section> curve_section;
};

struct curve_sections_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct curve_section_event {
    boost::uuids::uuid event_id;
    curve_section_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_section_version_key {
    curve_section_key curve_section;
    std::uint32_t version;
};

struct curve_section_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_sections_request {
    using response_type = struct list_curve_sections_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections.list";
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
    std::optional<curve_sections_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_sections_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_section> sections;
    std::uint64_t total;
};

struct get_curve_section_request {
    using response_type = struct get_curve_section_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_section_key key;
};

struct get_curve_section_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_section> curve_section;
};

struct get_many_curve_sections_request {
    using response_type = struct get_many_curve_sections_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_section_key> keys;
};

struct get_many_curve_sections_response {
    ores::utility::domain::result result;
    std::vector<curve_section_lookup> entries;
};

struct put_curve_section_request {
    using response_type = struct put_curve_section_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_section_change change;
    ores::utility::domain::change_intent intent;
};

struct put_curve_section_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_section> curve_section;
};

struct put_many_curve_sections_request {
    using response_type = struct put_many_curve_sections_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_section_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_curve_sections_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_section> sections;
};

struct delete_curve_section_request {
    using response_type = struct delete_curve_section_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_section_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_curve_section_response {
    ores::utility::domain::result result;
};

struct delete_many_curve_sections_request {
    using response_type = struct delete_many_curve_sections_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_section_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_curve_sections_response {
    ores::utility::domain::result result;
};

struct list_curve_section_versions_request {
    using response_type = struct list_curve_section_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_section_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_section_versions_filter> filter;
};

struct list_curve_section_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::curve_section> versions;
    std::uint64_t total;
};

struct get_curve_section_version_request {
    using response_type = struct get_curve_section_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.curve_sections_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_section_version_key key;
};

struct get_curve_section_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::curve_section> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_section_event_subjects {
inline constexpr std::string_view created = "refdata.v1.curve_sections_events.created";
inline constexpr std::string_view updated = "refdata.v1.curve_sections_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.curve_sections_events.deleted";
}

}

#endif
