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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_CONTACT_INFORMATION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_CONTACT_INFORMATION_PROTOCOL_HPP

#include "ores.iam.api/domain/account_contact_information.hpp"
#include "ores.iam.api/export.hpp"
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

namespace ores::iam::messaging {

struct account_contact_information_key {
    std::string email;

    friend bool operator==(const account_contact_information_key&,
                           const account_contact_information_key&) = default;
};

struct account_contact_information_write {
    boost::uuids::uuid id;
    boost::uuids::uuid account_id;
    std::string street_line_1;
    std::string street_line_2;
    std::string city;
    std::string state;
    std::string country_code;
    std::string postal_code;
    std::string phone;
    std::string email;
    std::string web_page;

    friend bool operator==(const account_contact_information_write&,
                           const account_contact_information_write&) = default;
};

struct account_contact_information_change {
    account_contact_information_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const account_contact_information_change&,
                           const account_contact_information_change&) = default;
};

struct account_contact_information_removal {
    account_contact_information_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const account_contact_information_removal&,
                           const account_contact_information_removal&) = default;
};

struct account_contact_information_lookup {
    account_contact_information_key key;
    std::optional<ores::iam::domain::account_contact_information> account_contact_information;

    friend bool operator==(const account_contact_information_lookup&,
                           const account_contact_information_lookup&) = default;
};

struct account_contact_informations_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;

    friend bool operator==(const account_contact_informations_filter&,
                           const account_contact_informations_filter&) = default;
};

struct account_contact_information_event {
    boost::uuids::uuid event_id;
    account_contact_information_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const account_contact_information_event&,
                           const account_contact_information_event&) = default;
};

struct account_contact_information_version_key {
    account_contact_information_key account_contact_information;
    std::uint32_t version;

    friend bool operator==(const account_contact_information_version_key&,
                           const account_contact_information_version_key&) = default;
};

struct account_contact_information_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const account_contact_information_versions_filter&,
                           const account_contact_information_versions_filter&) = default;
};

struct list_account_contact_informations_request {
    using response_type = struct list_account_contact_informations_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.list";
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
    std::optional<account_contact_informations_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_account_contact_informations_request&,
                           const list_account_contact_informations_request&) = default;
};

struct list_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;
    std::uint64_t total;

    friend bool operator==(const list_account_contact_informations_response&,
                           const list_account_contact_informations_response&) = default;
};

struct get_account_contact_information_request {
    using response_type = struct get_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_contact_information_key key;

    friend bool operator==(const get_account_contact_information_request&,
                           const get_account_contact_information_request&) = default;
};

struct get_account_contact_information_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_contact_information> account_contact_information;

    friend bool operator==(const get_account_contact_information_response&,
                           const get_account_contact_information_response&) = default;
};

struct get_many_account_contact_informations_request {
    using response_type = struct get_many_account_contact_informations_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_contact_information_key> keys;

    friend bool operator==(const get_many_account_contact_informations_request&,
                           const get_many_account_contact_informations_request&) = default;
};

struct get_many_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<account_contact_information_lookup> entries;

    friend bool operator==(const get_many_account_contact_informations_response&,
                           const get_many_account_contact_informations_response&) = default;
};

struct put_account_contact_information_request {
    using response_type = struct put_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_contact_information_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_account_contact_information_request&,
                           const put_account_contact_information_request&) = default;
};

struct put_account_contact_information_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_contact_information> account_contact_information;

    friend bool operator==(const put_account_contact_information_response&,
                           const put_account_contact_information_response&) = default;
};

struct put_many_account_contact_informations_request {
    using response_type = struct put_many_account_contact_informations_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_contact_information_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_account_contact_informations_request&,
                           const put_many_account_contact_informations_request&) = default;
};

struct put_many_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;

    friend bool operator==(const put_many_account_contact_informations_response&,
                           const put_many_account_contact_informations_response&) = default;
};

struct delete_account_contact_information_request {
    using response_type = struct delete_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_contact_information_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_account_contact_information_request&,
                           const delete_account_contact_information_request&) = default;
};

struct delete_account_contact_information_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_account_contact_information_response&,
                           const delete_account_contact_information_response&) = default;
};

struct delete_many_account_contact_informations_request {
    using response_type = struct delete_many_account_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<account_contact_information_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_account_contact_informations_request&,
                           const delete_many_account_contact_informations_request&) = default;
};

struct delete_many_account_contact_informations_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_account_contact_informations_response&,
                           const delete_many_account_contact_informations_response&) = default;
};

struct list_by_account_id_account_contact_informations_request {
    using response_type = struct list_by_account_id_account_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations.list_by_account_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_contact_informations_filter> filter;

    friend bool
    operator==(const list_by_account_id_account_contact_informations_request&,
               const list_by_account_id_account_contact_informations_request&) = default;
};

struct list_by_account_id_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;
    std::uint64_t total;

    friend bool
    operator==(const list_by_account_id_account_contact_informations_response&,
               const list_by_account_id_account_contact_informations_response&) = default;
};

struct list_account_contact_information_versions_request {
    using response_type = struct list_account_contact_information_versions_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_contact_information_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_contact_information_versions_filter> filter;

    friend bool operator==(const list_account_contact_information_versions_request&,
                           const list_account_contact_information_versions_request&) = default;
};

struct list_account_contact_information_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> versions;
    std::uint64_t total;

    friend bool operator==(const list_account_contact_information_versions_response&,
                           const list_account_contact_information_versions_response&) = default;
};

struct get_account_contact_information_version_request {
    using response_type = struct get_account_contact_information_version_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    account_contact_information_version_key key;

    friend bool operator==(const get_account_contact_information_version_request&,
                           const get_account_contact_information_version_request&) = default;
};

struct get_account_contact_information_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_contact_information> version;

    friend bool operator==(const get_account_contact_information_version_response&,
                           const get_account_contact_information_version_response&) = default;
};

/**
 * @brief The workflow step that publishes a DQ-cleared contact-information
 * bundle.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply, so it states a subject and no response. Its body is the DQ artefact
 * the server-side function knows how to expand, which is why it declares no
 * fields.
 */
struct publish_account_contact_informations_from_dq_request {
    static constexpr std::string_view nats_subject =
        "iam.v1.ops.publish_account_contact_informations_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const publish_account_contact_informations_from_dq_request&,
                           const publish_account_contact_informations_from_dq_request&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace account_contact_information_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.account_contact_informations.created";
inline constexpr std::string_view updated = "iam.v1.events.account_contact_informations.updated";
inline constexpr std::string_view deleted = "iam.v1.events.account_contact_informations.deleted";
}
/**
 * @brief The members of account_contact_information_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_contact_information_key>) {
    return std::tuple{
        utility::codec::field{"email", &account_contact_information_key::email},
    };
}

/**
 * @brief The members of account_contact_information_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_contact_information_write>) {
    return std::tuple{
        utility::codec::field{"id", &account_contact_information_write::id},
        utility::codec::field{"account_id", &account_contact_information_write::account_id},
        utility::codec::field{"street_line_1", &account_contact_information_write::street_line_1},
        utility::codec::field{"street_line_2", &account_contact_information_write::street_line_2},
        utility::codec::field{"city", &account_contact_information_write::city},
        utility::codec::field{"state", &account_contact_information_write::state},
        utility::codec::field{"country_code", &account_contact_information_write::country_code},
        utility::codec::field{"postal_code", &account_contact_information_write::postal_code},
        utility::codec::field{"phone", &account_contact_information_write::phone},
        utility::codec::field{"email", &account_contact_information_write::email},
        utility::codec::field{"web_page", &account_contact_information_write::web_page},
    };
}

/**
 * @brief The members of account_contact_information_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_contact_information_change>) {
    return std::tuple{
        utility::codec::field{"write", &account_contact_information_change::write},
        utility::codec::field{"precondition", &account_contact_information_change::precondition},
    };
}

/**
 * @brief The members of account_contact_information_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_contact_information_removal>) {
    return std::tuple{
        utility::codec::field{"key", &account_contact_information_removal::key},
        utility::codec::field{"precondition", &account_contact_information_removal::precondition},
    };
}

/**
 * @brief The members of account_contact_information_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_contact_information_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &account_contact_information_lookup::key},
        utility::codec::field{"account_contact_information",
                              &account_contact_information_lookup::account_contact_information},
    };
}

/**
 * @brief The members of account_contact_informations_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_contact_informations_filter>) {
    return std::tuple{
        utility::codec::field{"account_id", &account_contact_informations_filter::account_id},
        utility::codec::field{"id_one_of", &account_contact_informations_filter::id_one_of},
        utility::codec::field{"account_id_one_of",
                              &account_contact_informations_filter::account_id_one_of},
    };
}

/**
 * @brief The members of account_contact_information_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<account_contact_information_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &account_contact_information_event::event_id},
        utility::codec::field{"key", &account_contact_information_event::key},
        utility::codec::field{"action", &account_contact_information_event::action},
        utility::codec::field{"version", &account_contact_information_event::version},
        utility::codec::field{"occurred_at", &account_contact_information_event::occurred_at},
        utility::codec::field{"correlation_id", &account_contact_information_event::correlation_id},
    };
}

/**
 * @brief The members of account_contact_information_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<account_contact_information_version_key>) {
    return std::tuple{
        utility::codec::field{
            "account_contact_information",
            &account_contact_information_version_key::account_contact_information},
        utility::codec::field{"version", &account_contact_information_version_key::version},
    };
}

/**
 * @brief The members of account_contact_information_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<account_contact_information_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &account_contact_information_versions_filter::version},
        utility::codec::field{"from_version",
                              &account_contact_information_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &account_contact_information_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_account_contact_informations_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_account_contact_informations_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_account_contact_informations_request::offset},
        utility::codec::field{"limit", &list_account_contact_informations_request::limit},
        utility::codec::field{"order", &list_account_contact_informations_request::order},
        utility::codec::field{"filter", &list_account_contact_informations_request::filter},
        utility::codec::field{"as_of", &list_account_contact_informations_request::as_of},
    };
}

/**
 * @brief The members of list_account_contact_informations_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_account_contact_informations_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_account_contact_informations_response::result},
        utility::codec::field{
            "account_contact_informations",
            &list_account_contact_informations_response::account_contact_informations},
        utility::codec::field{"total", &list_account_contact_informations_response::total},
    };
}

/**
 * @brief The members of get_account_contact_information_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_account_contact_information_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_account_contact_information_request::key},
    };
}

/**
 * @brief The members of get_account_contact_information_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_account_contact_information_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_account_contact_information_response::result},
        utility::codec::field{
            "account_contact_information",
            &get_account_contact_information_response::account_contact_information},
    };
}

/**
 * @brief The members of get_many_account_contact_informations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_account_contact_informations_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_account_contact_informations_request::keys},
    };
}

/**
 * @brief The members of get_many_account_contact_informations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_account_contact_informations_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_account_contact_informations_response::result},
        utility::codec::field{"entries", &get_many_account_contact_informations_response::entries},
    };
}

/**
 * @brief The members of put_account_contact_information_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_account_contact_information_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_account_contact_information_request::change},
        utility::codec::field{"intent", &put_account_contact_information_request::intent},
    };
}

/**
 * @brief The members of put_account_contact_information_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_account_contact_information_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_account_contact_information_response::result},
        utility::codec::field{
            "account_contact_information",
            &put_account_contact_information_response::account_contact_information},
    };
}

/**
 * @brief The members of put_many_account_contact_informations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_account_contact_informations_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_account_contact_informations_request::changes},
        utility::codec::field{"intent", &put_many_account_contact_informations_request::intent},
    };
}

/**
 * @brief The members of put_many_account_contact_informations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_account_contact_informations_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_account_contact_informations_response::result},
        utility::codec::field{
            "account_contact_informations",
            &put_many_account_contact_informations_response::account_contact_informations},
    };
}

/**
 * @brief The members of delete_account_contact_information_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_account_contact_information_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_account_contact_information_request::removal},
        utility::codec::field{"intent", &delete_account_contact_information_request::intent},
    };
}

/**
 * @brief The members of delete_account_contact_information_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_account_contact_information_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_account_contact_information_response::result},
    };
}

/**
 * @brief The members of delete_many_account_contact_informations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_account_contact_informations_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_account_contact_informations_request::removals},
        utility::codec::field{"intent", &delete_many_account_contact_informations_request::intent},
    };
}

/**
 * @brief The members of delete_many_account_contact_informations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_account_contact_informations_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_account_contact_informations_response::result},
    };
}

/**
 * @brief The members of list_by_account_id_account_contact_informations_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_account_contact_informations_request>) {
    return std::tuple{
        utility::codec::field{"account_id",
                              &list_by_account_id_account_contact_informations_request::account_id},
        utility::codec::field{"scope",
                              &list_by_account_id_account_contact_informations_request::scope},
        utility::codec::field{"offset",
                              &list_by_account_id_account_contact_informations_request::offset},
        utility::codec::field{"limit",
                              &list_by_account_id_account_contact_informations_request::limit},
        utility::codec::field{"order",
                              &list_by_account_id_account_contact_informations_request::order},
        utility::codec::field{"filter",
                              &list_by_account_id_account_contact_informations_request::filter},
    };
}

/**
 * @brief The members of list_by_account_id_account_contact_informations_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_account_contact_informations_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_account_id_account_contact_informations_response::result},
        utility::codec::field{"account_contact_informations",
                              &list_by_account_id_account_contact_informations_response::
                                  account_contact_informations},
        utility::codec::field{"total",
                              &list_by_account_id_account_contact_informations_response::total},
    };
}

/**
 * @brief The members of list_account_contact_information_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_account_contact_information_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_account_contact_information_versions_request::key},
        utility::codec::field{"offset", &list_account_contact_information_versions_request::offset},
        utility::codec::field{"limit", &list_account_contact_information_versions_request::limit},
        utility::codec::field{"order", &list_account_contact_information_versions_request::order},
        utility::codec::field{"filter", &list_account_contact_information_versions_request::filter},
    };
}

/**
 * @brief The members of list_account_contact_information_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_account_contact_information_versions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_account_contact_information_versions_response::result},
        utility::codec::field{"versions",
                              &list_account_contact_information_versions_response::versions},
        utility::codec::field{"total", &list_account_contact_information_versions_response::total},
    };
}

/**
 * @brief The members of get_account_contact_information_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_account_contact_information_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_account_contact_information_version_request::key},
    };
}

/**
 * @brief The members of get_account_contact_information_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_account_contact_information_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_account_contact_information_version_response::result},
        utility::codec::field{"version",
                              &get_account_contact_information_version_response::version},
    };
}

/**
 * @brief The members of publish_account_contact_informations_from_dq_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<publish_account_contact_informations_from_dq_request>) {
    return std::tuple{};
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_contact_information_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_contact_information_key& v);
    static ores::iam::messaging::account_contact_information_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_key& v);
    static ores::iam::messaging::account_contact_information_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_contact_information_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_contact_information_write& v);
    static ores::iam::messaging::account_contact_information_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_write& v);
    static ores::iam::messaging::account_contact_information_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_contact_information_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::account_contact_information_change& v);
    static ores::iam::messaging::account_contact_information_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_change& v);
    static ores::iam::messaging::account_contact_information_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_contact_information_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::account_contact_information_removal& v);
    static ores::iam::messaging::account_contact_information_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_removal& v);
    static ores::iam::messaging::account_contact_information_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_contact_information_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::account_contact_information_lookup& v);
    static ores::iam::messaging::account_contact_information_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_lookup& v);
    static ores::iam::messaging::account_contact_information_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_contact_informations_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::account_contact_informations_filter& v);
    static ores::iam::messaging::account_contact_informations_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_informations_filter& v);
    static ores::iam::messaging::account_contact_informations_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::account_contact_information_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::account_contact_information_event& v);
    static ores::iam::messaging::account_contact_information_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_event& v);
    static ores::iam::messaging::account_contact_information_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::account_contact_information_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::account_contact_information_version_key& v);
    static ores::iam::messaging::account_contact_information_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_version_key& v);
    static ores::iam::messaging::account_contact_information_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::account_contact_information_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::account_contact_information_versions_filter& v);
    static ores::iam::messaging::account_contact_information_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::account_contact_information_versions_filter& v);
    static ores::iam::messaging::account_contact_information_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_account_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_account_contact_informations_request& v);
    static ores::iam::messaging::list_account_contact_informations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_account_contact_informations_request& v);
    static ores::iam::messaging::list_account_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_account_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_account_contact_informations_response& v);
    static ores::iam::messaging::list_account_contact_informations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_account_contact_informations_response& v);
    static ores::iam::messaging::list_account_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_account_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_account_contact_information_request& v);
    static ores::iam::messaging::get_account_contact_information_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_account_contact_information_request& v);
    static ores::iam::messaging::get_account_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_account_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_account_contact_information_response& v);
    static ores::iam::messaging::get_account_contact_information_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_account_contact_information_response& v);
    static ores::iam::messaging::get_account_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_many_account_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_account_contact_informations_request& v);
    static ores::iam::messaging::get_many_account_contact_informations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_account_contact_informations_request& v);
    static ores::iam::messaging::get_many_account_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_many_account_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_account_contact_informations_response& v);
    static ores::iam::messaging::get_many_account_contact_informations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_account_contact_informations_response& v);
    static ores::iam::messaging::get_many_account_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_account_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_account_contact_information_request& v);
    static ores::iam::messaging::put_account_contact_information_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_account_contact_information_request& v);
    static ores::iam::messaging::put_account_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_account_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_account_contact_information_response& v);
    static ores::iam::messaging::put_account_contact_information_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_account_contact_information_response& v);
    static ores::iam::messaging::put_account_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_many_account_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_account_contact_informations_request& v);
    static ores::iam::messaging::put_many_account_contact_informations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_account_contact_informations_request& v);
    static ores::iam::messaging::put_many_account_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_many_account_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_account_contact_informations_response& v);
    static ores::iam::messaging::put_many_account_contact_informations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_account_contact_informations_response& v);
    static ores::iam::messaging::put_many_account_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_account_contact_information_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_account_contact_information_request& v);
    static ores::iam::messaging::delete_account_contact_information_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_account_contact_information_request& v);
    static ores::iam::messaging::delete_account_contact_information_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_account_contact_information_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_account_contact_information_response& v);
    static ores::iam::messaging::delete_account_contact_information_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_account_contact_information_response& v);
    static ores::iam::messaging::delete_account_contact_information_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_account_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_account_contact_informations_request& v);
    static ores::iam::messaging::delete_many_account_contact_informations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_account_contact_informations_request& v);
    static ores::iam::messaging::delete_many_account_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_account_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_account_contact_informations_response& v);
    static ores::iam::messaging::delete_many_account_contact_informations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_account_contact_informations_response& v);
    static ores::iam::messaging::delete_many_account_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_account_contact_informations_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::iam::messaging::list_by_account_id_account_contact_informations_request& v);
    static ores::iam::messaging::list_by_account_id_account_contact_informations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_account_id_account_contact_informations_request& v);
    static ores::iam::messaging::list_by_account_id_account_contact_informations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_account_contact_informations_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::iam::messaging::list_by_account_id_account_contact_informations_response& v);
    static ores::iam::messaging::list_by_account_id_account_contact_informations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_account_id_account_contact_informations_response& v);
    static ores::iam::messaging::list_by_account_id_account_contact_informations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_account_contact_information_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_account_contact_information_versions_request& v);
    static ores::iam::messaging::list_account_contact_information_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_account_contact_information_versions_request& v);
    static ores::iam::messaging::list_account_contact_information_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_account_contact_information_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_account_contact_information_versions_response& v);
    static ores::iam::messaging::list_account_contact_information_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_account_contact_information_versions_response& v);
    static ores::iam::messaging::list_account_contact_information_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_account_contact_information_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_account_contact_information_version_request& v);
    static ores::iam::messaging::get_account_contact_information_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_account_contact_information_version_request& v);
    static ores::iam::messaging::get_account_contact_information_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_account_contact_information_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_account_contact_information_version_response& v);
    static ores::iam::messaging::get_account_contact_information_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_account_contact_information_version_response& v);
    static ores::iam::messaging::get_account_contact_information_version_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::publish_account_contact_informations_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::publish_account_contact_informations_from_dq_request& v);
    static ores::iam::messaging::publish_account_contact_informations_from_dq_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::publish_account_contact_informations_from_dq_request& v);
    static ores::iam::messaging::publish_account_contact_informations_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
