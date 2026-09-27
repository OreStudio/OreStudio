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
#ifndef ORES_ANALYTICS_API_MESSAGING_CREDIT_RATING_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_CREDIT_RATING_PROTOCOL_HPP

#include "ores.analytics.api/domain/credit_rating.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct credit_rating_key {
    std::string code;
};

struct credit_rating_write {
    std::string code;
    std::string name;
    int display_order;
};

struct credit_rating_change {
    credit_rating_write write;
    ores::utility::domain::precondition precondition;
};

struct credit_rating_removal {
    credit_rating_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct credit_rating_lookup {
    credit_rating_key key;
    std::optional<ores::analytics::domain::credit_rating> credit_rating;
};

struct credit_rating_event {
    boost::uuids::uuid event_id;
    credit_rating_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct credit_rating_version_key {
    credit_rating_key credit_rating;
    std::uint32_t version;
};

struct credit_rating_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_credit_ratings_request {
    using response_type = struct list_credit_ratings_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings.list";
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

struct list_credit_ratings_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::credit_rating> ratings;
    std::uint64_t total;
};

struct get_credit_rating_request {
    using response_type = struct get_credit_rating_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_rating_key key;
};

struct get_credit_rating_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::credit_rating> credit_rating;
};

struct get_many_credit_ratings_request {
    using response_type = struct get_many_credit_ratings_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_rating_key> keys;
};

struct get_many_credit_ratings_response {
    ores::utility::domain::result result;
    std::vector<credit_rating_lookup> entries;
};

struct put_credit_rating_request {
    using response_type = struct put_credit_rating_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_rating_change change;
    ores::utility::domain::change_intent intent;
};

struct put_credit_rating_response {
    ores::utility::domain::result result;
    ores::analytics::domain::credit_rating credit_rating;
};

struct put_many_credit_ratings_request {
    using response_type = struct put_many_credit_ratings_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_rating_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_credit_ratings_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::credit_rating> ratings;
};

struct delete_credit_rating_request {
    using response_type = struct delete_credit_rating_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_rating_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_credit_rating_response {
    ores::utility::domain::result result;
};

struct delete_many_credit_ratings_request {
    using response_type = struct delete_many_credit_ratings_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_rating_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_credit_ratings_response {
    ores::utility::domain::result result;
};

struct list_credit_rating_versions_request {
    using response_type = struct list_credit_rating_versions_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_rating_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<credit_rating_versions_filter> filter;
};

struct list_credit_rating_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::credit_rating> versions;
    std::uint64_t total;
};

struct get_credit_rating_version_request {
    using response_type = struct get_credit_rating_version_response;
    static constexpr std::string_view nats_subject = "analytics.v1.credit_ratings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_rating_version_key key;
};

struct get_credit_rating_version_response {
    ores::utility::domain::result result;
    ores::analytics::domain::credit_rating version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace credit_rating_event_subjects {
inline constexpr std::string_view created = "analytics.v1.credit_ratings_events.created";
inline constexpr std::string_view updated = "analytics.v1.credit_ratings_events.updated";
inline constexpr std::string_view deleted = "analytics.v1.credit_ratings_events.deleted";
}

}

#endif
