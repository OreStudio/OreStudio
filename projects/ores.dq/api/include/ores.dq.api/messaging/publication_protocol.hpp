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
#ifndef ORES_DQ_API_MESSAGING_PUBLICATION_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_PUBLICATION_PROTOCOL_HPP

#include "ores.dq.api/domain/publication.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct publication_key {
    boost::uuids::uuid id;
};

struct publication_write {
    boost::uuids::uuid id;
    boost::uuids::uuid dataset_id;
    std::string dataset_code;
    std::string mode;
    std::string target_table;
    std::int64_t records_inserted;
    std::int64_t records_updated;
    std::int64_t records_skipped;
    std::int64_t records_deleted;
    std::string published_by;
    std::chrono::system_clock::time_point published_at;
};

struct publication_change {
    publication_write write;
    ores::utility::domain::precondition precondition;
};

struct publication_removal {
    publication_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct publication_lookup {
    publication_key key;
    std::optional<ores::dq::domain::publication> publication;
};

struct publication_event {
    boost::uuids::uuid event_id;
    publication_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_publications_request {
    using response_type = struct list_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.list";
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

struct list_publications_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::publication> publications;
    std::uint64_t total;
};

struct get_publication_request {
    using response_type = struct get_publication_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    publication_key key;
};

struct get_publication_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::publication> publication;
};

struct get_many_publications_request {
    using response_type = struct get_many_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<publication_key> keys;
};

struct get_many_publications_response {
    ores::utility::domain::result result;
    std::vector<publication_lookup> entries;
};

struct put_publication_request {
    using response_type = struct put_publication_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    publication_change change;
    ores::utility::domain::change_intent intent;
};

struct put_publication_response {
    ores::utility::domain::result result;
    ores::dq::domain::publication publication;
};

struct put_many_publications_request {
    using response_type = struct put_many_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<publication_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_publications_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::publication> publications;
};

struct delete_publication_request {
    using response_type = struct delete_publication_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    publication_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_publication_response {
    ores::utility::domain::result result;
};

struct delete_many_publications_request {
    using response_type = struct delete_many_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<publication_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_publications_response {
    ores::utility::domain::result result;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace publication_event_subjects {
inline constexpr std::string_view created = "dq.v1.publications_events.created";
inline constexpr std::string_view updated = "dq.v1.publications_events.updated";
inline constexpr std::string_view deleted = "dq.v1.publications_events.deleted";
}

}

#endif
