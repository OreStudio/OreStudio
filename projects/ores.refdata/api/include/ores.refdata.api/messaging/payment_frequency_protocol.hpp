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
#ifndef ORES_REFDATA_API_MESSAGING_PAYMENT_FREQUENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PAYMENT_FREQUENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/payment_frequency.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct payment_frequency_key {
    std::string code;
};

struct payment_frequency_write {
    std::string code;
    std::string name;
    std::string description;
    std::string period_unit;
    std::optional<int> period_multiplier;
    int display_order;
};

struct payment_frequency_change {
    payment_frequency_write write;
    ores::utility::domain::precondition precondition;
};

struct payment_frequency_removal {
    payment_frequency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct payment_frequency_lookup {
    payment_frequency_key key;
    std::optional<ores::refdata::domain::payment_frequency> payment_frequency;
};

struct payment_frequency_event {
    boost::uuids::uuid event_id;
    payment_frequency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct payment_frequency_version_key {
    payment_frequency_key payment_frequency;
    std::uint32_t version;
};

struct payment_frequency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_payment_frequencies_request {
    using response_type = struct list_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.list";
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

struct list_payment_frequencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::payment_frequency> payment_frequencies;
    std::uint64_t total;
};

struct get_payment_frequency_request {
    using response_type = struct get_payment_frequency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_key key;
};

struct get_payment_frequency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::payment_frequency> payment_frequency;
};

struct get_many_payment_frequencies_request {
    using response_type = struct get_many_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<payment_frequency_key> keys;
};

struct get_many_payment_frequencies_response {
    ores::utility::domain::result result;
    std::vector<payment_frequency_lookup> entries;
};

struct put_payment_frequency_request {
    using response_type = struct put_payment_frequency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_change change;
    ores::utility::domain::change_intent intent;
};

struct put_payment_frequency_response {
    ores::utility::domain::result result;
    ores::refdata::domain::payment_frequency payment_frequency;
};

struct put_many_payment_frequencies_request {
    using response_type = struct put_many_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<payment_frequency_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_payment_frequencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::payment_frequency> payment_frequencies;
};

struct delete_payment_frequency_request {
    using response_type = struct delete_payment_frequency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_payment_frequency_response {
    ores::utility::domain::result result;
};

struct delete_many_payment_frequencies_request {
    using response_type = struct delete_many_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<payment_frequency_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_payment_frequencies_response {
    ores::utility::domain::result result;
};

struct list_payment_frequency_versions_request {
    using response_type = struct list_payment_frequency_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<payment_frequency_versions_filter> filter;
};

struct list_payment_frequency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::payment_frequency> versions;
    std::uint64_t total;
};

struct get_payment_frequency_version_request {
    using response_type = struct get_payment_frequency_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_version_key key;
};

struct get_payment_frequency_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::payment_frequency version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace payment_frequency_event_subjects {
inline constexpr std::string_view created = "refdata.v1.payment_frequencies_events.created";
inline constexpr std::string_view updated = "refdata.v1.payment_frequencies_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.payment_frequencies_events.deleted";
}

}

#endif
