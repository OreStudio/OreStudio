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
#ifndef ORES_REFDATA_API_MESSAGING_CONVENTION_KIND_TERM_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CONVENTION_KIND_TERM_PROTOCOL_HPP

#include "ores.refdata.api/domain/convention_kind_term.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct convention_kind_term_key {
    std::string code;
};

struct convention_kind_term_write {
    std::string code;
    std::string kind_code;
    std::string entity_name;
    std::string column_name;
    std::string schema_name;
    std::string xpath;
    int position;
};

struct convention_kind_term_change {
    convention_kind_term_write write;
    ores::utility::domain::precondition precondition;
};

struct convention_kind_term_removal {
    convention_kind_term_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct convention_kind_term_lookup {
    convention_kind_term_key key;
    std::optional<ores::refdata::domain::convention_kind_term> convention_kind_term;
};

struct convention_kind_terms_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct convention_kind_term_event {
    boost::uuids::uuid event_id;
    convention_kind_term_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct convention_kind_term_version_key {
    convention_kind_term_key convention_kind_term;
    std::uint32_t version;
};

struct convention_kind_term_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_convention_kind_terms_request {
    using response_type = struct list_convention_kind_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.convention_kind_terms.list";
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
    std::optional<convention_kind_terms_filter> filter;
    std::optional<std::string> as_of;
};

struct list_convention_kind_terms_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::convention_kind_term> terms;
    std::uint64_t total;
};

struct get_convention_kind_term_request {
    using response_type = struct get_convention_kind_term_response;
    static constexpr std::string_view nats_subject = "refdata.v1.convention_kind_terms.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    convention_kind_term_key key;
};

struct get_convention_kind_term_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::convention_kind_term> convention_kind_term;
};

struct get_many_convention_kind_terms_request {
    using response_type = struct get_many_convention_kind_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.convention_kind_terms.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<convention_kind_term_key> keys;
};

struct get_many_convention_kind_terms_response {
    ores::utility::domain::result result;
    std::vector<convention_kind_term_lookup> entries;
};

struct put_convention_kind_term_request {
    using response_type = struct put_convention_kind_term_response;
    static constexpr std::string_view nats_subject = "refdata.v1.convention_kind_terms.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    convention_kind_term_change change;
    ores::utility::domain::change_intent intent;
};

struct put_convention_kind_term_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::convention_kind_term> convention_kind_term;
};

struct put_many_convention_kind_terms_request {
    using response_type = struct put_many_convention_kind_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.convention_kind_terms.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<convention_kind_term_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_convention_kind_terms_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::convention_kind_term> terms;
};

struct delete_convention_kind_term_request {
    using response_type = struct delete_convention_kind_term_response;
    static constexpr std::string_view nats_subject = "refdata.v1.convention_kind_terms.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    convention_kind_term_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_convention_kind_term_response {
    ores::utility::domain::result result;
};

struct delete_many_convention_kind_terms_request {
    using response_type = struct delete_many_convention_kind_terms_response;
    static constexpr std::string_view nats_subject = "refdata.v1.convention_kind_terms.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<convention_kind_term_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_convention_kind_terms_response {
    ores::utility::domain::result result;
};

struct list_convention_kind_term_versions_request {
    using response_type = struct list_convention_kind_term_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.convention_kind_terms_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    convention_kind_term_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<convention_kind_term_versions_filter> filter;
};

struct list_convention_kind_term_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::convention_kind_term> versions;
    std::uint64_t total;
};

struct get_convention_kind_term_version_request {
    using response_type = struct get_convention_kind_term_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.convention_kind_terms_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    convention_kind_term_version_key key;
};

struct get_convention_kind_term_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::convention_kind_term> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace convention_kind_term_event_subjects {
inline constexpr std::string_view created = "refdata.v1.convention_kind_terms_events.created";
inline constexpr std::string_view updated = "refdata.v1.convention_kind_terms_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.convention_kind_terms_events.deleted";
}

}

#endif
