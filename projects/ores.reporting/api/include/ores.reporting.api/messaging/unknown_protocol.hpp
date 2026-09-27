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
#ifndef ORES_REPORTING_API_MESSAGING__PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING__PROTOCOL_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/domain/protocol.hpp"
#include "ores.reporting.api/domain/.hpp"

namespace ores::reporting::messaging {

struct _key {
    std::string value;
};

struct _write {
    boost::uuids::uuid id;
    boost::uuids::uuid configuration_id;
    boost::uuids::uuid parameter_definition_id;
    std::string value;
    int position;
};

struct _change {
    _write write;
    ores::utility::domain::precondition precondition;
};

struct _removal {
    _key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct _lookup {
    _key key;
    std::optional<ores::reporting::domain::> ;
};

struct s_filter {
    std::optional<boost::uuids::uuid> configuration_id;
    std::optional<boost::uuids::uuid> parameter_definition_id;
};

struct _event {
    boost::uuids::uuid event_id;
    _key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct _version_key {
    _key ;
    std::uint32_t version;
};

struct _versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_s_request {
    using response_type = struct list_s_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.list";
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
    std::optional<s_filter> filter;
};

struct list_s_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::> parameter_values;
    std::uint64_t total;
};

struct get__request {
    using response_type = struct get__response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    _key key;
};

struct get__response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::> ;
};

struct get_many_s_request {
    using response_type = struct get_many_s_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<_key> keys;
};

struct get_many_s_response {
    ores::utility::domain::result result;
    std::vector<_lookup> entries;
};

struct put__request {
    using response_type = struct put__response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    _change change;
    ores::utility::domain::change_intent intent;
};

struct put__response {
    ores::utility::domain::result result;
    ores::reporting::domain:: ;
};

struct put_many_s_request {
    using response_type = struct put_many_s_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_s_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::> parameter_values;
};

struct delete__request {
    using response_type = struct delete__response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    _removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete__response {
    ores::utility::domain::result result;
};

struct delete_many_s_request {
    using response_type = struct delete_many_s_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_s_response {
    ores::utility::domain::result result;
};

struct list_by_configuration_id_s_request {
    using response_type = struct list_by_configuration_id_s_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.list_by_configuration_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid configuration_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<s_filter> filter;
};

struct list_by_configuration_id_s_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::> parameter_values;
    std::uint64_t total;
};

struct list_by_parameter_definition_id_s_request {
    using response_type = struct list_by_parameter_definition_id_s_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s.list_by_parameter_definition_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid parameter_definition_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<s_filter> filter;
};

struct list_by_parameter_definition_id_s_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::> parameter_values;
    std::uint64_t total;
};

struct list__versions_request {
    using response_type = struct list__versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    _key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<_versions_filter> filter;
};

struct list__versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::> versions;
    std::uint64_t total;
};

struct get__version_request {
    using response_type = struct get__version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.s_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    _version_key key;
};

struct get__version_response {
    ores::utility::domain::result result;
    ores::reporting::domain:: version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace _event_subjects {
inline constexpr std::string_view created = "reporting.v1.s_events.created";
inline constexpr std::string_view updated = "reporting.v1.s_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.s_events.deleted";
}

}

#endif
