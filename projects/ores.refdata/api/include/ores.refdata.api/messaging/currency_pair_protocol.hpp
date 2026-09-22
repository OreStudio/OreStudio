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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency_pair.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct currency_pair_key {
    std::string pair_code;
};

struct currency_pair_write {
    std::string pair_code;
    std::string base_currency;
    std::string quote_currency;
    std::string classification;
};

struct currency_pair_change {
    currency_pair_write write;
    ores::utility::domain::precondition precondition;
};

struct currency_pair_removal {
    currency_pair_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct currency_pair_lookup {
    currency_pair_key key;
    std::optional<ores::refdata::domain::currency_pair> currency_pair;
};

struct currency_pair_event {
    boost::uuids::uuid event_id;
    currency_pair_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct currency_pair_version_key {
    currency_pair_key currency_pair;
    std::uint32_t version;
};

struct currency_pair_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_currency_pairs_request {
    using response_type = struct list_currency_pairs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs.list";
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

struct list_currency_pairs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair> pairs;
    std::uint64_t total;
};

struct get_currency_pair_request {
    using response_type = struct get_currency_pair_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_key key;
};

struct get_currency_pair_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::currency_pair> currency_pair;
};

struct get_many_currency_pairs_request {
    using response_type = struct get_many_currency_pairs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_key> keys;
};

struct get_many_currency_pairs_response {
    ores::utility::domain::result result;
    std::vector<currency_pair_lookup> entries;
};

struct put_currency_pair_request {
    using response_type = struct put_currency_pair_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_change change;
    ores::utility::domain::change_intent intent;
};

struct put_currency_pair_response {
    ores::utility::domain::result result;
    ores::refdata::domain::currency_pair currency_pair;
};

struct put_many_currency_pairs_request {
    using response_type = struct put_many_currency_pairs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_currency_pairs_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair> pairs;
};

struct delete_currency_pair_request {
    using response_type = struct delete_currency_pair_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_currency_pair_response {
    ores::utility::domain::result result;
};

struct delete_many_currency_pairs_request {
    using response_type = struct delete_many_currency_pairs_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<currency_pair_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_currency_pairs_response {
    ores::utility::domain::result result;
};

struct list_currency_pair_versions_request {
    using response_type = struct list_currency_pair_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<currency_pair_versions_filter> filter;
};

struct list_currency_pair_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::currency_pair> versions;
    std::uint64_t total;
};

struct get_currency_pair_version_request {
    using response_type = struct get_currency_pair_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.currency_pairs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    currency_pair_version_key key;
};

struct get_currency_pair_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::currency_pair version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace currency_pair_event_subjects {
inline constexpr std::string_view created = "refdata.v1.currency_pairs_events.created";
inline constexpr std::string_view updated = "refdata.v1.currency_pairs_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.currency_pairs_events.deleted";
}

}

#endif
