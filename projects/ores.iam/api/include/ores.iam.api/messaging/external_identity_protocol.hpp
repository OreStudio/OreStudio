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
#ifndef ORES_IAM_API_MESSAGING_EXTERNAL_IDENTITY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_EXTERNAL_IDENTITY_PROTOCOL_HPP

#include "ores.iam.api/domain/external_identity_codec.hpp"
#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::iam::messaging {

struct external_identity_key {
    std::string email;

    friend bool operator==(const external_identity_key&, const external_identity_key&) = default;
};

struct external_identity_write {
    boost::uuids::uuid id;
    std::string issuer;
    std::string subject;
    boost::uuids::uuid account_id;
    boost::uuids::uuid identity_provider_id;
    std::string email;
    bool email_verified{};
    std::chrono::system_clock::time_point linked_at;
    std::chrono::system_clock::time_point last_used_at;

    friend bool operator==(const external_identity_write&,
                           const external_identity_write&) = default;
};

struct external_identity_change {
    external_identity_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const external_identity_change&,
                           const external_identity_change&) = default;
};

struct external_identity_removal {
    external_identity_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const external_identity_removal&,
                           const external_identity_removal&) = default;
};

struct external_identity_lookup {
    external_identity_key key;
    std::optional<ores::iam::domain::external_identity> external_identity;

    friend bool operator==(const external_identity_lookup&,
                           const external_identity_lookup&) = default;
};

struct external_identities_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<boost::uuids::uuid> identity_provider_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> identity_provider_id_one_of;

    friend bool operator==(const external_identities_filter&,
                           const external_identities_filter&) = default;
};

struct external_identity_event {
    boost::uuids::uuid event_id;
    external_identity_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const external_identity_event&,
                           const external_identity_event&) = default;
};

struct external_identity_version_key {
    external_identity_key external_identity;
    std::uint32_t version{};

    friend bool operator==(const external_identity_version_key&,
                           const external_identity_version_key&) = default;
};

struct external_identity_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const external_identity_versions_filter&,
                           const external_identity_versions_filter&) = default;
};

struct list_external_identities_request {
    using response_type = struct list_external_identities_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities.list";
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
    std::optional<external_identities_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_external_identities_request&,
                           const list_external_identities_request&) = default;
};

struct list_external_identities_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::external_identity> links;
    std::uint64_t total{};

    friend bool operator==(const list_external_identities_response&,
                           const list_external_identities_response&) = default;
};

struct get_external_identity_request {
    using response_type = struct get_external_identity_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    external_identity_key key;

    friend bool operator==(const get_external_identity_request&,
                           const get_external_identity_request&) = default;
};

struct get_external_identity_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::external_identity> external_identity;

    friend bool operator==(const get_external_identity_response&,
                           const get_external_identity_response&) = default;
};

struct get_many_external_identities_request {
    using response_type = struct get_many_external_identities_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<external_identity_key> keys;

    friend bool operator==(const get_many_external_identities_request&,
                           const get_many_external_identities_request&) = default;
};

struct get_many_external_identities_response {
    ores::utility::domain::result result;
    std::vector<external_identity_lookup> entries;

    friend bool operator==(const get_many_external_identities_response&,
                           const get_many_external_identities_response&) = default;
};

struct put_external_identity_request {
    using response_type = struct put_external_identity_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    external_identity_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_external_identity_request&,
                           const put_external_identity_request&) = default;
};

struct put_external_identity_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::external_identity> external_identity;

    friend bool operator==(const put_external_identity_response&,
                           const put_external_identity_response&) = default;
};

struct put_many_external_identities_request {
    using response_type = struct put_many_external_identities_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<external_identity_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_external_identities_request&,
                           const put_many_external_identities_request&) = default;
};

struct put_many_external_identities_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::external_identity> links;

    friend bool operator==(const put_many_external_identities_response&,
                           const put_many_external_identities_response&) = default;
};

struct delete_external_identity_request {
    using response_type = struct delete_external_identity_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    external_identity_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_external_identity_request&,
                           const delete_external_identity_request&) = default;
};

struct delete_external_identity_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_external_identity_response&,
                           const delete_external_identity_response&) = default;
};

struct delete_many_external_identities_request {
    using response_type = struct delete_many_external_identities_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<external_identity_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_external_identities_request&,
                           const delete_many_external_identities_request&) = default;
};

struct delete_many_external_identities_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_external_identities_response&,
                           const delete_many_external_identities_response&) = default;
};

struct list_by_account_id_external_identities_request {
    using response_type = struct list_by_account_id_external_identities_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.external_identities.list_by_account_id";
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
    std::optional<external_identities_filter> filter;

    friend bool operator==(const list_by_account_id_external_identities_request&,
                           const list_by_account_id_external_identities_request&) = default;
};

struct list_by_account_id_external_identities_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::external_identity> links;
    std::uint64_t total{};

    friend bool operator==(const list_by_account_id_external_identities_response&,
                           const list_by_account_id_external_identities_response&) = default;
};

struct list_by_identity_provider_id_external_identities_request {
    using response_type = struct list_by_identity_provider_id_external_identities_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.external_identities.list_by_identity_provider_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid identity_provider_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<external_identities_filter> filter;

    friend bool
    operator==(const list_by_identity_provider_id_external_identities_request&,
               const list_by_identity_provider_id_external_identities_request&) = default;
};

struct list_by_identity_provider_id_external_identities_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::external_identity> links;
    std::uint64_t total{};

    friend bool
    operator==(const list_by_identity_provider_id_external_identities_response&,
               const list_by_identity_provider_id_external_identities_response&) = default;
};

struct list_external_identity_versions_request {
    using response_type = struct list_external_identity_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    external_identity_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<external_identity_versions_filter> filter;

    friend bool operator==(const list_external_identity_versions_request&,
                           const list_external_identity_versions_request&) = default;
};

struct list_external_identity_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::external_identity> versions;
    std::uint64_t total{};

    friend bool operator==(const list_external_identity_versions_response&,
                           const list_external_identity_versions_response&) = default;
};

struct get_external_identity_version_request {
    using response_type = struct get_external_identity_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.external_identities_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    external_identity_version_key key;

    friend bool operator==(const get_external_identity_version_request&,
                           const get_external_identity_version_request&) = default;
};

struct get_external_identity_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::external_identity> version;

    friend bool operator==(const get_external_identity_version_response&,
                           const get_external_identity_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace external_identity_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.external_identities.created";
inline constexpr std::string_view updated = "iam.v1.events.external_identities.updated";
inline constexpr std::string_view deleted = "iam.v1.events.external_identities.deleted";
}
/**
 * @brief The members of external_identity_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"email", &external_identity_key::email},
    };
}

/**
 * @brief The members of external_identity_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &external_identity_write::id},
        utility::codec::field{"issuer", &external_identity_write::issuer},
        utility::codec::field{"subject", &external_identity_write::subject},
        utility::codec::field{"account_id", &external_identity_write::account_id},
        utility::codec::field{"identity_provider_id",
                              &external_identity_write::identity_provider_id},
        utility::codec::field{"email", &external_identity_write::email},
        utility::codec::field{"email_verified", &external_identity_write::email_verified},
        utility::codec::field{"linked_at", &external_identity_write::linked_at},
        utility::codec::field{"last_used_at", &external_identity_write::last_used_at},
    };
}

/**
 * @brief The members of external_identity_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &external_identity_change::write},
        utility::codec::field{"precondition", &external_identity_change::precondition},
    };
}

/**
 * @brief The members of external_identity_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &external_identity_removal::key},
        utility::codec::field{"precondition", &external_identity_removal::precondition},
    };
}

/**
 * @brief The members of external_identity_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &external_identity_lookup::key},
        utility::codec::field{"external_identity", &external_identity_lookup::external_identity},
    };
}

/**
 * @brief The members of external_identities_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identities_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"account_id", &external_identities_filter::account_id},
        utility::codec::field{"identity_provider_id",
                              &external_identities_filter::identity_provider_id},
        utility::codec::field{"id_one_of", &external_identities_filter::id_one_of},
        utility::codec::field{"account_id_one_of", &external_identities_filter::account_id_one_of},
        utility::codec::field{"identity_provider_id_one_of",
                              &external_identities_filter::identity_provider_id_one_of},
    };
}

/**
 * @brief The members of external_identity_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &external_identity_event::event_id},
        utility::codec::field{"key", &external_identity_event::key},
        utility::codec::field{"action", &external_identity_event::action},
        utility::codec::field{"version", &external_identity_event::version},
        utility::codec::field{"occurred_at", &external_identity_event::occurred_at},
        utility::codec::field{"correlation_id", &external_identity_event::correlation_id},
    };
}

/**
 * @brief The members of external_identity_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"external_identity",
                              &external_identity_version_key::external_identity},
        utility::codec::field{"version", &external_identity_version_key::version},
    };
}

/**
 * @brief The members of external_identity_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<external_identity_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &external_identity_versions_filter::version},
        utility::codec::field{"from_version", &external_identity_versions_filter::from_version},
        utility::codec::field{"to_version", &external_identity_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_external_identities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_external_identities_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_external_identities_request::offset},
        utility::codec::field{"limit", &list_external_identities_request::limit},
        utility::codec::field{"order", &list_external_identities_request::order},
        utility::codec::field{"filter", &list_external_identities_request::filter},
        utility::codec::field{"as_of", &list_external_identities_request::as_of},
    };
}

/**
 * @brief The members of list_external_identities_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_external_identities_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_external_identities_response::result},
        utility::codec::field{"links", &list_external_identities_response::links},
        utility::codec::field{"total", &list_external_identities_response::total},
    };
}

/**
 * @brief The members of get_external_identity_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_external_identity_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_external_identity_request::key},
    };
}

/**
 * @brief The members of get_external_identity_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_external_identity_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_external_identity_response::result},
        utility::codec::field{"external_identity",
                              &get_external_identity_response::external_identity},
    };
}

/**
 * @brief The members of get_many_external_identities_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_external_identities_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_external_identities_request::keys},
    };
}

/**
 * @brief The members of get_many_external_identities_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_external_identities_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_external_identities_response::result},
        utility::codec::field{"entries", &get_many_external_identities_response::entries},
    };
}

/**
 * @brief The members of put_external_identity_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_external_identity_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_external_identity_request::change},
        utility::codec::field{"intent", &put_external_identity_request::intent},
    };
}

/**
 * @brief The members of put_external_identity_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_external_identity_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_external_identity_response::result},
        utility::codec::field{"external_identity",
                              &put_external_identity_response::external_identity},
    };
}

/**
 * @brief The members of put_many_external_identities_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_external_identities_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_external_identities_request::changes},
        utility::codec::field{"intent", &put_many_external_identities_request::intent},
    };
}

/**
 * @brief The members of put_many_external_identities_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_external_identities_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_external_identities_response::result},
        utility::codec::field{"links", &put_many_external_identities_response::links},
    };
}

/**
 * @brief The members of delete_external_identity_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_external_identity_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_external_identity_request::removal},
        utility::codec::field{"intent", &delete_external_identity_request::intent},
    };
}

/**
 * @brief The members of delete_external_identity_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_external_identity_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_external_identity_response::result},
    };
}

/**
 * @brief The members of delete_many_external_identities_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_external_identities_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_external_identities_request::removals},
        utility::codec::field{"intent", &delete_many_external_identities_request::intent},
    };
}

/**
 * @brief The members of delete_many_external_identities_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_external_identities_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_external_identities_response::result},
    };
}

/**
 * @brief The members of list_by_account_id_external_identities_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_external_identities_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"account_id",
                              &list_by_account_id_external_identities_request::account_id},
        utility::codec::field{"scope", &list_by_account_id_external_identities_request::scope},
        utility::codec::field{"offset", &list_by_account_id_external_identities_request::offset},
        utility::codec::field{"limit", &list_by_account_id_external_identities_request::limit},
        utility::codec::field{"order", &list_by_account_id_external_identities_request::order},
        utility::codec::field{"filter", &list_by_account_id_external_identities_request::filter},
    };
}

/**
 * @brief The members of list_by_account_id_external_identities_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_account_id_external_identities_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_account_id_external_identities_response::result},
        utility::codec::field{"links", &list_by_account_id_external_identities_response::links},
        utility::codec::field{"total", &list_by_account_id_external_identities_response::total},
    };
}

/**
 * @brief The members of list_by_identity_provider_id_external_identities_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_identity_provider_id_external_identities_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{
            "identity_provider_id",
            &list_by_identity_provider_id_external_identities_request::identity_provider_id},
        utility::codec::field{"scope",
                              &list_by_identity_provider_id_external_identities_request::scope},
        utility::codec::field{"offset",
                              &list_by_identity_provider_id_external_identities_request::offset},
        utility::codec::field{"limit",
                              &list_by_identity_provider_id_external_identities_request::limit},
        utility::codec::field{"order",
                              &list_by_identity_provider_id_external_identities_request::order},
        utility::codec::field{"filter",
                              &list_by_identity_provider_id_external_identities_request::filter},
    };
}

/**
 * @brief The members of list_by_identity_provider_id_external_identities_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_identity_provider_id_external_identities_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result",
                              &list_by_identity_provider_id_external_identities_response::result},
        utility::codec::field{"links",
                              &list_by_identity_provider_id_external_identities_response::links},
        utility::codec::field{"total",
                              &list_by_identity_provider_id_external_identities_response::total},
    };
}

/**
 * @brief The members of list_external_identity_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_external_identity_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_external_identity_versions_request::key},
        utility::codec::field{"offset", &list_external_identity_versions_request::offset},
        utility::codec::field{"limit", &list_external_identity_versions_request::limit},
        utility::codec::field{"order", &list_external_identity_versions_request::order},
        utility::codec::field{"filter", &list_external_identity_versions_request::filter},
    };
}

/**
 * @brief The members of list_external_identity_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_external_identity_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_external_identity_versions_response::result},
        utility::codec::field{"versions", &list_external_identity_versions_response::versions},
        utility::codec::field{"total", &list_external_identity_versions_response::total},
    };
}

/**
 * @brief The members of get_external_identity_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_external_identity_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_external_identity_version_request::key},
    };
}

/**
 * @brief The members of get_external_identity_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_external_identity_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_external_identity_version_response::result},
        utility::codec::field{"version", &get_external_identity_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_key& v);
    static ores::iam::messaging::external_identity_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identity_key& v);
    static ores::iam::messaging::external_identity_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_write& v);
    static ores::iam::messaging::external_identity_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identity_write& v);
    static ores::iam::messaging::external_identity_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_change& v);
    static ores::iam::messaging::external_identity_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identity_change& v);
    static ores::iam::messaging::external_identity_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_removal& v);
    static ores::iam::messaging::external_identity_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identity_removal& v);
    static ores::iam::messaging::external_identity_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_lookup& v);
    static ores::iam::messaging::external_identity_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identity_lookup& v);
    static ores::iam::messaging::external_identity_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identities_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identities_filter& v);
    static ores::iam::messaging::external_identities_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identities_filter& v);
    static ores::iam::messaging::external_identities_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_event& v);
    static ores::iam::messaging::external_identity_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identity_event& v);
    static ores::iam::messaging::external_identity_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_version_key& v);
    static ores::iam::messaging::external_identity_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::external_identity_version_key& v);
    static ores::iam::messaging::external_identity_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::external_identity_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::external_identity_versions_filter& v);
    static ores::iam::messaging::external_identity_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::external_identity_versions_filter& v);
    static ores::iam::messaging::external_identity_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_external_identities_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_external_identities_request& v);
    static ores::iam::messaging::list_external_identities_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_external_identities_request& v);
    static ores::iam::messaging::list_external_identities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_external_identities_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_external_identities_response& v);
    static ores::iam::messaging::list_external_identities_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_external_identities_response& v);
    static ores::iam::messaging::list_external_identities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_external_identity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_external_identity_request& v);
    static ores::iam::messaging::get_external_identity_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_external_identity_request& v);
    static ores::iam::messaging::get_external_identity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_external_identity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_external_identity_response& v);
    static ores::iam::messaging::get_external_identity_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_external_identity_response& v);
    static ores::iam::messaging::get_external_identity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_external_identities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_external_identities_request& v);
    static ores::iam::messaging::get_many_external_identities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_external_identities_request& v);
    static ores::iam::messaging::get_many_external_identities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_many_external_identities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_external_identities_response& v);
    static ores::iam::messaging::get_many_external_identities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_external_identities_response& v);
    static ores::iam::messaging::get_many_external_identities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_external_identity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_external_identity_request& v);
    static ores::iam::messaging::put_external_identity_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_external_identity_request& v);
    static ores::iam::messaging::put_external_identity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_external_identity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_external_identity_response& v);
    static ores::iam::messaging::put_external_identity_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_external_identity_response& v);
    static ores::iam::messaging::put_external_identity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_external_identities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_external_identities_request& v);
    static ores::iam::messaging::put_many_external_identities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_external_identities_request& v);
    static ores::iam::messaging::put_many_external_identities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_many_external_identities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_external_identities_response& v);
    static ores::iam::messaging::put_many_external_identities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_external_identities_response& v);
    static ores::iam::messaging::put_many_external_identities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_external_identity_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_external_identity_request& v);
    static ores::iam::messaging::delete_external_identity_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_external_identity_request& v);
    static ores::iam::messaging::delete_external_identity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_external_identity_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_external_identity_response& v);
    static ores::iam::messaging::delete_external_identity_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_external_identity_response& v);
    static ores::iam::messaging::delete_external_identity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_external_identities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_external_identities_request& v);
    static ores::iam::messaging::delete_many_external_identities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_external_identities_request& v);
    static ores::iam::messaging::delete_many_external_identities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_external_identities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_external_identities_response& v);
    static ores::iam::messaging::delete_many_external_identities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_external_identities_response& v);
    static ores::iam::messaging::delete_many_external_identities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_external_identities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_external_identities_request& v);
    static ores::iam::messaging::list_by_account_id_external_identities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_external_identities_request& v);
    static ores::iam::messaging::list_by_account_id_external_identities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_external_identities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_external_identities_response& v);
    static ores::iam::messaging::list_by_account_id_external_identities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_external_identities_response& v);
    static ores::iam::messaging::list_by_account_id_external_identities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_request& v);
    static ores::iam::messaging::list_by_identity_provider_id_external_identities_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_request& v);
    static ores::iam::messaging::list_by_identity_provider_id_external_identities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_identity_provider_id_external_identities_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_response& v);
    static ores::iam::messaging::list_by_identity_provider_id_external_identities_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_identity_provider_id_external_identities_response& v);
    static ores::iam::messaging::list_by_identity_provider_id_external_identities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_external_identity_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_external_identity_versions_request& v);
    static ores::iam::messaging::list_external_identity_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_external_identity_versions_request& v);
    static ores::iam::messaging::list_external_identity_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_external_identity_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_external_identity_versions_response& v);
    static ores::iam::messaging::list_external_identity_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_external_identity_versions_response& v);
    static ores::iam::messaging::list_external_identity_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_external_identity_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_external_identity_version_request& v);
    static ores::iam::messaging::get_external_identity_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_external_identity_version_request& v);
    static ores::iam::messaging::get_external_identity_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_external_identity_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_external_identity_version_response& v);
    static ores::iam::messaging::get_external_identity_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_external_identity_version_response& v);
    static ores::iam::messaging::get_external_identity_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
