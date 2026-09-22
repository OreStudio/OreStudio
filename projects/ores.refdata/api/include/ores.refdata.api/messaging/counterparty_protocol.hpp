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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information.hpp"
#include "ores.refdata.api/domain/counterparty_identifier.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct counterparty_key {
    boost::uuids::uuid id;
};

struct counterparty_write {
    boost::uuids::uuid id;
    std::string short_code;
    std::string full_name;
    std::optional<std::string> transliterated_name;
    std::string party_type;
    std::optional<boost::uuids::uuid> parent_counterparty_id;
    std::string business_center_code;
    std::string status;
    std::optional<boost::uuids::uuid> image_id;
};

struct counterparty_change {
    counterparty_write write;
    ores::utility::domain::precondition precondition;
};

struct counterparty_removal {
    counterparty_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct counterparty_lookup {
    counterparty_key key;
    std::optional<ores::refdata::domain::counterparty> counterparty;
};

struct counterparty_event {
    boost::uuids::uuid event_id;
    counterparty_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct counterparty_version_key {
    counterparty_key counterparty;
    std::uint32_t version;
};

struct counterparty_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_counterparties_request {
    using response_type = struct list_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.list";
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

struct list_counterparties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty> counterparties;
    std::uint64_t total;
};

struct get_counterparty_request {
    using response_type = struct get_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_key key;
};

struct get_counterparty_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty> counterparty;
};

struct get_many_counterparties_request {
    using response_type = struct get_many_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_key> keys;
};

struct get_many_counterparties_response {
    ores::utility::domain::result result;
    std::vector<counterparty_lookup> entries;
};

struct put_counterparty_request {
    using response_type = struct put_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_change change;
    ores::utility::domain::change_intent intent;
};

struct put_counterparty_response {
    ores::utility::domain::result result;
    ores::refdata::domain::counterparty counterparty;
};

struct put_many_counterparties_request {
    using response_type = struct put_many_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_counterparties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty> counterparties;
};

struct delete_counterparty_request {
    using response_type = struct delete_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_counterparty_response {
    ores::utility::domain::result result;
};

struct delete_many_counterparties_request {
    using response_type = struct delete_many_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_counterparties_response {
    ores::utility::domain::result result;
};

struct list_counterparty_versions_request {
    using response_type = struct list_counterparty_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_versions_filter> filter;
};

struct list_counterparty_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty> versions;
    std::uint64_t total;
};

struct get_counterparty_version_request {
    using response_type = struct get_counterparty_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_version_key key;
};

struct get_counterparty_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::counterparty version;
};

/**
 * @brief Reads a counterparty as it stood at a specific version, together
 * with its identifiers and contact information as they stood during that
 * same version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
struct get_counterparty_composite_as_of_request {
    using response_type = struct get_counterparty_composite_as_of_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.composite_as_of";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string id;
    int version = 0;
};

struct get_counterparty_composite_as_of_response {
    bool success = false;
    std::string message;
    ores::refdata::domain::counterparty counterparty;
    std::vector<ores::refdata::domain::counterparty_identifier> identifiers;
    std::vector<ores::refdata::domain::counterparty_contact_information> contacts;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace counterparty_event_subjects {
inline constexpr std::string_view created = "refdata.v1.counterparties_events.created";
inline constexpr std::string_view updated = "refdata.v1.counterparties_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.counterparties_events.deleted";
}

}

#endif
