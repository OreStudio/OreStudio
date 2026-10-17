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
#ifndef ORES_REFDATA_API_MESSAGING_CONVENTIONS_IMPORT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CONVENTIONS_IMPORT_PROTOCOL_HPP

#include "ores.refdata.api/domain/conventions_import.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct conventions_import_key {
    boost::uuids::uuid id;
};

struct conventions_import_write {
    boost::uuids::uuid id;
    std::optional<std::string> source;
    std::string state;
    std::optional<int> families_expected;
    std::chrono::system_clock::time_point expires_at;
};

struct conventions_import_change {
    conventions_import_write write;
    ores::utility::domain::precondition precondition;
};

struct conventions_import_removal {
    conventions_import_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct conventions_import_lookup {
    conventions_import_key key;
    std::optional<ores::refdata::domain::conventions_import> conventions_import;
};

struct conventions_imports_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct conventions_import_event {
    boost::uuids::uuid event_id;
    conventions_import_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct conventions_import_version_key {
    conventions_import_key conventions_import;
    std::uint32_t version;
};

struct conventions_import_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_conventions_imports_request {
    using response_type = struct list_conventions_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports.list";
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
    std::optional<conventions_imports_filter> filter;
    std::optional<std::string> as_of;
};

struct list_conventions_imports_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::conventions_import> conventions_imports;
    std::uint64_t total;
};

struct get_conventions_import_request {
    using response_type = struct get_conventions_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    conventions_import_key key;
};

struct get_conventions_import_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::conventions_import> conventions_import;
};

struct get_many_conventions_imports_request {
    using response_type = struct get_many_conventions_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<conventions_import_key> keys;
};

struct get_many_conventions_imports_response {
    ores::utility::domain::result result;
    std::vector<conventions_import_lookup> entries;
};

struct put_conventions_import_request {
    using response_type = struct put_conventions_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    conventions_import_change change;
    ores::utility::domain::change_intent intent;
};

struct put_conventions_import_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::conventions_import> conventions_import;
};

struct put_many_conventions_imports_request {
    using response_type = struct put_many_conventions_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<conventions_import_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_conventions_imports_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::conventions_import> conventions_imports;
};

struct delete_conventions_import_request {
    using response_type = struct delete_conventions_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    conventions_import_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_conventions_import_response {
    ores::utility::domain::result result;
};

struct delete_many_conventions_imports_request {
    using response_type = struct delete_many_conventions_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<conventions_import_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_conventions_imports_response {
    ores::utility::domain::result result;
};

struct list_conventions_import_versions_request {
    using response_type = struct list_conventions_import_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    conventions_import_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<conventions_import_versions_filter> filter;
};

struct list_conventions_import_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::conventions_import> versions;
    std::uint64_t total;
};

struct get_conventions_import_version_request {
    using response_type = struct get_conventions_import_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.conventions_imports_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    conventions_import_version_key key;
};

struct get_conventions_import_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::conventions_import> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace conventions_import_event_subjects {
inline constexpr std::string_view created = "refdata.v1.conventions_imports_events.created";
inline constexpr std::string_view updated = "refdata.v1.conventions_imports_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.conventions_imports_events.deleted";
}

}

#endif
