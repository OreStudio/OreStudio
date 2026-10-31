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
#ifndef ORES_REFDATA_API_MESSAGING_PARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/party.hpp"
#include "ores.refdata.api/domain/party_contact_information.hpp"
#include "ores.refdata.api/domain/party_identifier.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct party_key {
    std::string short_code;
};

struct party_write {
    boost::uuids::uuid id;
    std::string short_code;
    std::string full_name;
    std::string codename;
    std::optional<std::string> transliterated_name;
    std::string party_category;
    std::string party_type;
    std::optional<boost::uuids::uuid> parent_party_id;
    std::string business_center_code;
    std::string status;
    std::optional<boost::uuids::uuid> image_id;
    bool is_registration_default;
};

struct party_change {
    party_write write;
    ores::utility::domain::precondition precondition;
};

struct party_removal {
    party_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct party_lookup {
    party_key key;
    std::optional<ores::refdata::domain::party> party;
};

struct parties_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::string> search;
};

struct party_event {
    boost::uuids::uuid event_id;
    party_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct party_version_key {
    party_key party;
    std::uint32_t version;
};

struct party_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_parties_request {
    using response_type = struct list_parties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.list";
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
    std::optional<parties_filter> filter;
    std::optional<std::string> as_of;
};

struct list_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party> parties;
    std::uint64_t total;
};

struct get_party_request {
    using response_type = struct get_party_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_key key;
};

struct get_party_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party> party;
};

struct get_many_parties_request {
    using response_type = struct get_many_parties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_key> keys;
};

struct get_many_parties_response {
    ores::utility::domain::result result;
    std::vector<party_lookup> entries;
};

struct put_party_request {
    using response_type = struct put_party_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_change change;
    ores::utility::domain::change_intent intent;
};

struct put_party_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party> party;
};

struct put_many_parties_request {
    using response_type = struct put_many_parties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_parties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party> parties;
};

struct delete_party_request {
    using response_type = struct delete_party_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_party_response {
    ores::utility::domain::result result;
};

struct delete_many_parties_request {
    using response_type = struct delete_many_parties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<party_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_parties_response {
    ores::utility::domain::result result;
};

struct list_party_versions_request {
    using response_type = struct list_party_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<party_versions_filter> filter;
};

struct list_party_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::party> versions;
    std::uint64_t total;
};

struct get_party_version_request {
    using response_type = struct get_party_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    party_version_key key;
};

struct get_party_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::party> version;
};

/**
 * @brief Reads a party as it stood at a specific version, together with its
 * identifiers and contact information as they stood during that same
 * version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
struct get_party_composite_as_of_request {
    using response_type = struct get_party_composite_as_of_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.get_party_composite_as_of";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid id;
    int version = 0;
};

struct get_party_composite_as_of_response {
    bool success = false;
    std::string message;
    ores::refdata::domain::party party;
    std::vector<ores::refdata::domain::party_identifier> identifiers;
    std::vector<ores::refdata::domain::party_contact_information> contacts;
};

/**
 * @brief Writes a party and the children a confirmed review staged, in one
 * transaction. A refusal or a failure leaves none of them behind.
 */
struct put_party_composite_request {
    using response_type = struct put_party_composite_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.put_party_composite";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ores::utility::domain::change_intent intent;
    ores::refdata::domain::party party;
    std::vector<ores::refdata::domain::party_identifier> identifiers;
    std::vector<ores::refdata::domain::party_contact_information> contacts;
};

struct put_party_composite_response {
    ores::utility::domain::result result;
    ores::refdata::domain::party party;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace party_event_subjects {
inline constexpr std::string_view created = "refdata.v1.parties_events.created";
inline constexpr std::string_view updated = "refdata.v1.parties_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.parties_events.deleted";
}
/**
 * @brief The members of party_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_key&) {
    return std::tuple{
        utility::codec::field{"short_code", &party_key::short_code},
    };
}

/**
 * @brief The members of party_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_write&) {
    return std::tuple{
        utility::codec::field{"id", &party_write::id},
        utility::codec::field{"short_code", &party_write::short_code},
        utility::codec::field{"full_name", &party_write::full_name},
        utility::codec::field{"codename", &party_write::codename},
        utility::codec::field{"transliterated_name", &party_write::transliterated_name},
        utility::codec::field{"party_category", &party_write::party_category},
        utility::codec::field{"party_type", &party_write::party_type},
        utility::codec::field{"parent_party_id", &party_write::parent_party_id},
        utility::codec::field{"business_center_code", &party_write::business_center_code},
        utility::codec::field{"status", &party_write::status},
        utility::codec::field{"image_id", &party_write::image_id},
        utility::codec::field{"is_registration_default", &party_write::is_registration_default},
    };
}

/**
 * @brief The members of party_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_change&) {
    return std::tuple{
        utility::codec::field{"write", &party_change::write},
        utility::codec::field{"precondition", &party_change::precondition},
    };
}

/**
 * @brief The members of party_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_removal&) {
    return std::tuple{
        utility::codec::field{"key", &party_removal::key},
        utility::codec::field{"precondition", &party_removal::precondition},
    };
}

/**
 * @brief The members of party_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &party_lookup::key},
        utility::codec::field{"party", &party_lookup::party},
    };
}

/**
 * @brief The members of parties_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const parties_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &parties_filter::id_one_of},
        utility::codec::field{"search", &parties_filter::search},
    };
}

/**
 * @brief The members of party_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &party_event::event_id},
        utility::codec::field{"key", &party_event::key},
        utility::codec::field{"action", &party_event::action},
        utility::codec::field{"version", &party_event::version},
        utility::codec::field{"occurred_at", &party_event::occurred_at},
        utility::codec::field{"correlation_id", &party_event::correlation_id},
    };
}

/**
 * @brief The members of party_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_version_key&) {
    return std::tuple{
        utility::codec::field{"party", &party_version_key::party},
        utility::codec::field{"version", &party_version_key::version},
    };
}

/**
 * @brief The members of party_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const party_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &party_versions_filter::version},
        utility::codec::field{"from_version", &party_versions_filter::from_version},
        utility::codec::field{"to_version", &party_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_parties_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_parties_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_parties_request::offset},
        utility::codec::field{"limit", &list_parties_request::limit},
        utility::codec::field{"order", &list_parties_request::order},
        utility::codec::field{"filter", &list_parties_request::filter},
        utility::codec::field{"as_of", &list_parties_request::as_of},
    };
}

/**
 * @brief The members of list_parties_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_parties_response::result},
        utility::codec::field{"parties", &list_parties_response::parties},
        utility::codec::field{"total", &list_parties_response::total},
    };
}

/**
 * @brief The members of get_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_party_request::key},
    };
}

/**
 * @brief The members of get_party_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_party_response::result},
        utility::codec::field{"party", &get_party_response::party},
    };
}

/**
 * @brief The members of get_many_parties_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_parties_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_parties_request::keys},
    };
}

/**
 * @brief The members of get_many_parties_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_parties_response::result},
        utility::codec::field{"entries", &get_many_parties_response::entries},
    };
}

/**
 * @brief The members of put_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_party_request::change},
        utility::codec::field{"intent", &put_party_request::intent},
    };
}

/**
 * @brief The members of put_party_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_party_response::result},
        utility::codec::field{"party", &put_party_response::party},
    };
}

/**
 * @brief The members of put_many_parties_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_parties_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_parties_request::changes},
        utility::codec::field{"intent", &put_many_parties_request::intent},
    };
}

/**
 * @brief The members of put_many_parties_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_parties_response::result},
        utility::codec::field{"parties", &put_many_parties_response::parties},
    };
}

/**
 * @brief The members of delete_party_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_party_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_party_request::removal},
        utility::codec::field{"intent", &delete_party_request::intent},
    };
}

/**
 * @brief The members of delete_party_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_party_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_party_response::result},
    };
}

/**
 * @brief The members of delete_many_parties_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_parties_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_parties_request::removals},
        utility::codec::field{"intent", &delete_many_parties_request::intent},
    };
}

/**
 * @brief The members of delete_many_parties_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_parties_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_parties_response::result},
    };
}

/**
 * @brief The members of list_party_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_party_versions_request::key},
        utility::codec::field{"offset", &list_party_versions_request::offset},
        utility::codec::field{"limit", &list_party_versions_request::limit},
        utility::codec::field{"order", &list_party_versions_request::order},
        utility::codec::field{"filter", &list_party_versions_request::filter},
    };
}

/**
 * @brief The members of list_party_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_party_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_party_versions_response::result},
        utility::codec::field{"versions", &list_party_versions_response::versions},
        utility::codec::field{"total", &list_party_versions_response::total},
    };
}

/**
 * @brief The members of get_party_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_party_version_request::key},
    };
}

/**
 * @brief The members of get_party_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_party_version_response::result},
        utility::codec::field{"version", &get_party_version_response::version},
    };
}

/**
 * @brief The members of get_party_composite_as_of_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_composite_as_of_request&) {
    return std::tuple{
        utility::codec::field{"id", &get_party_composite_as_of_request::id},
        utility::codec::field{"version", &get_party_composite_as_of_request::version},
    };
}

/**
 * @brief The members of get_party_composite_as_of_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_party_composite_as_of_response&) {
    return std::tuple{
        utility::codec::field{"success", &get_party_composite_as_of_response::success},
        utility::codec::field{"message", &get_party_composite_as_of_response::message},
        utility::codec::field{"party", &get_party_composite_as_of_response::party},
        utility::codec::field{"identifiers", &get_party_composite_as_of_response::identifiers},
        utility::codec::field{"contacts", &get_party_composite_as_of_response::contacts},
    };
}

/**
 * @brief The members of put_party_composite_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_composite_request&) {
    return std::tuple{
        utility::codec::field{"intent", &put_party_composite_request::intent},
        utility::codec::field{"party", &put_party_composite_request::party},
        utility::codec::field{"identifiers", &put_party_composite_request::identifiers},
        utility::codec::field{"contacts", &put_party_composite_request::contacts},
    };
}

/**
 * @brief The members of put_party_composite_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_party_composite_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_party_composite_response::result},
        utility::codec::field{"party", &put_party_composite_response::party},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_key& v);
    static ores::refdata::messaging::party_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_key& v);
    static ores::refdata::messaging::party_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_write& v);
    static ores::refdata::messaging::party_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_write& v);
    static ores::refdata::messaging::party_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_change& v);
    static ores::refdata::messaging::party_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_change& v);
    static ores::refdata::messaging::party_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_removal& v);
    static ores::refdata::messaging::party_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_removal& v);
    static ores::refdata::messaging::party_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_lookup& v);
    static ores::refdata::messaging::party_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_lookup& v);
    static ores::refdata::messaging::party_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::parties_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::parties_filter& v);
    static ores::refdata::messaging::parties_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::parties_filter& v);
    static ores::refdata::messaging::parties_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_event& v);
    static ores::refdata::messaging::party_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_event& v);
    static ores::refdata::messaging::party_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_version_key& v);
    static ores::refdata::messaging::party_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_version_key& v);
    static ores::refdata::messaging::party_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::party_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::party_versions_filter& v);
    static ores::refdata::messaging::party_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::party_versions_filter& v);
    static ores::refdata::messaging::party_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_parties_request& v);
    static ores::refdata::messaging::list_parties_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_parties_request& v);
    static ores::refdata::messaging::list_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_parties_response& v);
    static ores::refdata::messaging::list_parties_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_parties_response& v);
    static ores::refdata::messaging::list_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_request& v);
    static ores::refdata::messaging::get_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_party_request& v);
    static ores::refdata::messaging::get_party_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_response& v);
    static ores::refdata::messaging::get_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_party_response& v);
    static ores::refdata::messaging::get_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_parties_request& v);
    static ores::refdata::messaging::get_many_parties_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_parties_request& v);
    static ores::refdata::messaging::get_many_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_parties_response& v);
    static ores::refdata::messaging::get_many_parties_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_parties_response& v);
    static ores::refdata::messaging::get_many_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_request& v);
    static ores::refdata::messaging::put_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_party_request& v);
    static ores::refdata::messaging::put_party_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_response& v);
    static ores::refdata::messaging::put_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_party_response& v);
    static ores::refdata::messaging::put_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_parties_request& v);
    static ores::refdata::messaging::put_many_parties_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_parties_request& v);
    static ores::refdata::messaging::put_many_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_parties_response& v);
    static ores::refdata::messaging::put_many_parties_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_parties_response& v);
    static ores::refdata::messaging::put_many_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_party_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_party_request& v);
    static ores::refdata::messaging::delete_party_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_party_request& v);
    static ores::refdata::messaging::delete_party_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_party_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_party_response& v);
    static ores::refdata::messaging::delete_party_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_party_response& v);
    static ores::refdata::messaging::delete_party_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_many_parties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_parties_request& v);
    static ores::refdata::messaging::delete_many_parties_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_parties_request& v);
    static ores::refdata::messaging::delete_many_parties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_many_parties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_parties_response& v);
    static ores::refdata::messaging::delete_many_parties_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_parties_response& v);
    static ores::refdata::messaging::delete_many_parties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_party_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_party_versions_request& v);
    static ores::refdata::messaging::list_party_versions_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_party_versions_request& v);
    static ores::refdata::messaging::list_party_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_party_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_party_versions_response& v);
    static ores::refdata::messaging::list_party_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_party_versions_response& v);
    static ores::refdata::messaging::list_party_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_version_request& v);
    static ores::refdata::messaging::get_party_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_party_version_request& v);
    static ores::refdata::messaging::get_party_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_party_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_party_version_response& v);
    static ores::refdata::messaging::get_party_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_party_version_response& v);
    static ores::refdata::messaging::get_party_version_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_party_composite_as_of_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_party_composite_as_of_request& v);
    static ores::refdata::messaging::get_party_composite_as_of_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_party_composite_as_of_request& v);
    static ores::refdata::messaging::get_party_composite_as_of_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_party_composite_as_of_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_party_composite_as_of_response& v);
    static ores::refdata::messaging::get_party_composite_as_of_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_party_composite_as_of_response& v);
    static ores::refdata::messaging::get_party_composite_as_of_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_composite_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_composite_request& v);
    static ores::refdata::messaging::put_party_composite_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_party_composite_request& v);
    static ores::refdata::messaging::put_party_composite_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_party_composite_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_party_composite_response& v);
    static ores::refdata::messaging::put_party_composite_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_party_composite_response& v);
    static ores::refdata::messaging::put_party_composite_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
