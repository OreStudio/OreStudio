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
#ifndef ORES_IAM_API_MESSAGING_IDENTITY_PROVIDER_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_IDENTITY_PROVIDER_PROTOCOL_HPP

#include "ores.iam.api/domain/identity_provider_codec.hpp"
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

struct identity_provider_key {
    std::string code;

    friend bool operator==(const identity_provider_key&, const identity_provider_key&) = default;
};

struct identity_provider_write {
    boost::uuids::uuid id;
    std::string code;
    std::string display_name;
    std::string issuer;
    std::string client_id;
    std::string client_secret_ref;
    std::string scopes;
    std::string allowed_email_domains;
    bool is_enabled{};

    friend bool operator==(const identity_provider_write&,
                           const identity_provider_write&) = default;
};

struct identity_provider_change {
    identity_provider_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const identity_provider_change&,
                           const identity_provider_change&) = default;
};

struct identity_provider_removal {
    identity_provider_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const identity_provider_removal&,
                           const identity_provider_removal&) = default;
};

struct identity_provider_lookup {
    identity_provider_key key;
    std::optional<ores::iam::domain::identity_provider> identity_provider;

    friend bool operator==(const identity_provider_lookup&,
                           const identity_provider_lookup&) = default;
};

struct identity_providers_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::string> search;

    friend bool operator==(const identity_providers_filter&,
                           const identity_providers_filter&) = default;
};

struct identity_provider_event {
    boost::uuids::uuid event_id;
    identity_provider_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const identity_provider_event&,
                           const identity_provider_event&) = default;
};

struct identity_provider_version_key {
    identity_provider_key identity_provider;
    std::uint32_t version{};

    friend bool operator==(const identity_provider_version_key&,
                           const identity_provider_version_key&) = default;
};

struct identity_provider_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const identity_provider_versions_filter&,
                           const identity_provider_versions_filter&) = default;
};

struct list_identity_providers_request {
    using response_type = struct list_identity_providers_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers.list";
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
    std::optional<identity_providers_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_identity_providers_request&,
                           const list_identity_providers_request&) = default;
};

struct list_identity_providers_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::identity_provider> providers;
    std::uint64_t total{};

    friend bool operator==(const list_identity_providers_response&,
                           const list_identity_providers_response&) = default;
};

struct get_identity_provider_request {
    using response_type = struct get_identity_provider_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    identity_provider_key key;

    friend bool operator==(const get_identity_provider_request&,
                           const get_identity_provider_request&) = default;
};

struct get_identity_provider_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::identity_provider> identity_provider;

    friend bool operator==(const get_identity_provider_response&,
                           const get_identity_provider_response&) = default;
};

struct get_many_identity_providers_request {
    using response_type = struct get_many_identity_providers_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<identity_provider_key> keys;

    friend bool operator==(const get_many_identity_providers_request&,
                           const get_many_identity_providers_request&) = default;
};

struct get_many_identity_providers_response {
    ores::utility::domain::result result;
    std::vector<identity_provider_lookup> entries;

    friend bool operator==(const get_many_identity_providers_response&,
                           const get_many_identity_providers_response&) = default;
};

struct put_identity_provider_request {
    using response_type = struct put_identity_provider_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    identity_provider_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_identity_provider_request&,
                           const put_identity_provider_request&) = default;
};

struct put_identity_provider_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::identity_provider> identity_provider;

    friend bool operator==(const put_identity_provider_response&,
                           const put_identity_provider_response&) = default;
};

struct put_many_identity_providers_request {
    using response_type = struct put_many_identity_providers_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<identity_provider_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_identity_providers_request&,
                           const put_many_identity_providers_request&) = default;
};

struct put_many_identity_providers_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::identity_provider> providers;

    friend bool operator==(const put_many_identity_providers_response&,
                           const put_many_identity_providers_response&) = default;
};

struct delete_identity_provider_request {
    using response_type = struct delete_identity_provider_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    identity_provider_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_identity_provider_request&,
                           const delete_identity_provider_request&) = default;
};

struct delete_identity_provider_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_identity_provider_response&,
                           const delete_identity_provider_response&) = default;
};

struct delete_many_identity_providers_request {
    using response_type = struct delete_many_identity_providers_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<identity_provider_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_identity_providers_request&,
                           const delete_many_identity_providers_request&) = default;
};

struct delete_many_identity_providers_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_identity_providers_response&,
                           const delete_many_identity_providers_response&) = default;
};

struct list_identity_provider_versions_request {
    using response_type = struct list_identity_provider_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    identity_provider_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<identity_provider_versions_filter> filter;

    friend bool operator==(const list_identity_provider_versions_request&,
                           const list_identity_provider_versions_request&) = default;
};

struct list_identity_provider_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::identity_provider> versions;
    std::uint64_t total{};

    friend bool operator==(const list_identity_provider_versions_response&,
                           const list_identity_provider_versions_response&) = default;
};

struct get_identity_provider_version_request {
    using response_type = struct get_identity_provider_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.identity_providers_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    identity_provider_version_key key;

    friend bool operator==(const get_identity_provider_version_request&,
                           const get_identity_provider_version_request&) = default;
};

struct get_identity_provider_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::identity_provider> version;

    friend bool operator==(const get_identity_provider_version_response&,
                           const get_identity_provider_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace identity_provider_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.identity_providers.created";
inline constexpr std::string_view updated = "iam.v1.events.identity_providers.updated";
inline constexpr std::string_view deleted = "iam.v1.events.identity_providers.deleted";
}
/**
 * @brief The members of identity_provider_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &identity_provider_key::code},
    };
}

/**
 * @brief The members of identity_provider_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &identity_provider_write::id},
        utility::codec::field{"code", &identity_provider_write::code},
        utility::codec::field{"display_name", &identity_provider_write::display_name},
        utility::codec::field{"issuer", &identity_provider_write::issuer},
        utility::codec::field{"client_id", &identity_provider_write::client_id},
        utility::codec::field{"client_secret_ref", &identity_provider_write::client_secret_ref},
        utility::codec::field{"scopes", &identity_provider_write::scopes},
        utility::codec::field{"allowed_email_domains",
                              &identity_provider_write::allowed_email_domains},
        utility::codec::field{"is_enabled", &identity_provider_write::is_enabled},
    };
}

/**
 * @brief The members of identity_provider_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &identity_provider_change::write},
        utility::codec::field{"precondition", &identity_provider_change::precondition},
    };
}

/**
 * @brief The members of identity_provider_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &identity_provider_removal::key},
        utility::codec::field{"precondition", &identity_provider_removal::precondition},
    };
}

/**
 * @brief The members of identity_provider_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &identity_provider_lookup::key},
        utility::codec::field{"identity_provider", &identity_provider_lookup::identity_provider},
    };
}

/**
 * @brief The members of identity_providers_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_providers_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &identity_providers_filter::id_one_of},
        utility::codec::field{"search", &identity_providers_filter::search},
    };
}

/**
 * @brief The members of identity_provider_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &identity_provider_event::event_id},
        utility::codec::field{"key", &identity_provider_event::key},
        utility::codec::field{"action", &identity_provider_event::action},
        utility::codec::field{"version", &identity_provider_event::version},
        utility::codec::field{"occurred_at", &identity_provider_event::occurred_at},
        utility::codec::field{"correlation_id", &identity_provider_event::correlation_id},
    };
}

/**
 * @brief The members of identity_provider_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"identity_provider",
                              &identity_provider_version_key::identity_provider},
        utility::codec::field{"version", &identity_provider_version_key::version},
    };
}

/**
 * @brief The members of identity_provider_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<identity_provider_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &identity_provider_versions_filter::version},
        utility::codec::field{"from_version", &identity_provider_versions_filter::from_version},
        utility::codec::field{"to_version", &identity_provider_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_identity_providers_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_identity_providers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_identity_providers_request::offset},
        utility::codec::field{"limit", &list_identity_providers_request::limit},
        utility::codec::field{"order", &list_identity_providers_request::order},
        utility::codec::field{"filter", &list_identity_providers_request::filter},
        utility::codec::field{"as_of", &list_identity_providers_request::as_of},
    };
}

/**
 * @brief The members of list_identity_providers_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_identity_providers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_identity_providers_response::result},
        utility::codec::field{"providers", &list_identity_providers_response::providers},
        utility::codec::field{"total", &list_identity_providers_response::total},
    };
}

/**
 * @brief The members of get_identity_provider_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_identity_provider_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_identity_provider_request::key},
    };
}

/**
 * @brief The members of get_identity_provider_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_identity_provider_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_identity_provider_response::result},
        utility::codec::field{"identity_provider",
                              &get_identity_provider_response::identity_provider},
    };
}

/**
 * @brief The members of get_many_identity_providers_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_identity_providers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_identity_providers_request::keys},
    };
}

/**
 * @brief The members of get_many_identity_providers_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_identity_providers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_identity_providers_response::result},
        utility::codec::field{"entries", &get_many_identity_providers_response::entries},
    };
}

/**
 * @brief The members of put_identity_provider_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_identity_provider_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_identity_provider_request::change},
        utility::codec::field{"intent", &put_identity_provider_request::intent},
    };
}

/**
 * @brief The members of put_identity_provider_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_identity_provider_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_identity_provider_response::result},
        utility::codec::field{"identity_provider",
                              &put_identity_provider_response::identity_provider},
    };
}

/**
 * @brief The members of put_many_identity_providers_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_identity_providers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_identity_providers_request::changes},
        utility::codec::field{"intent", &put_many_identity_providers_request::intent},
    };
}

/**
 * @brief The members of put_many_identity_providers_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_identity_providers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_identity_providers_response::result},
        utility::codec::field{"providers", &put_many_identity_providers_response::providers},
    };
}

/**
 * @brief The members of delete_identity_provider_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_identity_provider_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_identity_provider_request::removal},
        utility::codec::field{"intent", &delete_identity_provider_request::intent},
    };
}

/**
 * @brief The members of delete_identity_provider_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_identity_provider_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_identity_provider_response::result},
    };
}

/**
 * @brief The members of delete_many_identity_providers_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_identity_providers_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_identity_providers_request::removals},
        utility::codec::field{"intent", &delete_many_identity_providers_request::intent},
    };
}

/**
 * @brief The members of delete_many_identity_providers_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_identity_providers_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_identity_providers_response::result},
    };
}

/**
 * @brief The members of list_identity_provider_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_identity_provider_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_identity_provider_versions_request::key},
        utility::codec::field{"offset", &list_identity_provider_versions_request::offset},
        utility::codec::field{"limit", &list_identity_provider_versions_request::limit},
        utility::codec::field{"order", &list_identity_provider_versions_request::order},
        utility::codec::field{"filter", &list_identity_provider_versions_request::filter},
    };
}

/**
 * @brief The members of list_identity_provider_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_identity_provider_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_identity_provider_versions_response::result},
        utility::codec::field{"versions", &list_identity_provider_versions_response::versions},
        utility::codec::field{"total", &list_identity_provider_versions_response::total},
    };
}

/**
 * @brief The members of get_identity_provider_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_identity_provider_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_identity_provider_version_request::key},
    };
}

/**
 * @brief The members of get_identity_provider_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_identity_provider_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_identity_provider_version_response::result},
        utility::codec::field{"version", &get_identity_provider_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_key& v);
    static ores::iam::messaging::identity_provider_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_provider_key& v);
    static ores::iam::messaging::identity_provider_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_write& v);
    static ores::iam::messaging::identity_provider_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_provider_write& v);
    static ores::iam::messaging::identity_provider_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_change& v);
    static ores::iam::messaging::identity_provider_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_provider_change& v);
    static ores::iam::messaging::identity_provider_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_removal& v);
    static ores::iam::messaging::identity_provider_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_provider_removal& v);
    static ores::iam::messaging::identity_provider_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_lookup& v);
    static ores::iam::messaging::identity_provider_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_provider_lookup& v);
    static ores::iam::messaging::identity_provider_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_providers_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_providers_filter& v);
    static ores::iam::messaging::identity_providers_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_providers_filter& v);
    static ores::iam::messaging::identity_providers_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_event& v);
    static ores::iam::messaging::identity_provider_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_provider_event& v);
    static ores::iam::messaging::identity_provider_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_version_key& v);
    static ores::iam::messaging::identity_provider_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::identity_provider_version_key& v);
    static ores::iam::messaging::identity_provider_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::identity_provider_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::identity_provider_versions_filter& v);
    static ores::iam::messaging::identity_provider_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::identity_provider_versions_filter& v);
    static ores::iam::messaging::identity_provider_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_identity_providers_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_identity_providers_request& v);
    static ores::iam::messaging::list_identity_providers_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_identity_providers_request& v);
    static ores::iam::messaging::list_identity_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_identity_providers_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_identity_providers_response& v);
    static ores::iam::messaging::list_identity_providers_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_identity_providers_response& v);
    static ores::iam::messaging::list_identity_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_identity_provider_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_identity_provider_request& v);
    static ores::iam::messaging::get_identity_provider_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_identity_provider_request& v);
    static ores::iam::messaging::get_identity_provider_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_identity_provider_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_identity_provider_response& v);
    static ores::iam::messaging::get_identity_provider_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_identity_provider_response& v);
    static ores::iam::messaging::get_identity_provider_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_identity_providers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_identity_providers_request& v);
    static ores::iam::messaging::get_many_identity_providers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_identity_providers_request& v);
    static ores::iam::messaging::get_many_identity_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_identity_providers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_identity_providers_response& v);
    static ores::iam::messaging::get_many_identity_providers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_identity_providers_response& v);
    static ores::iam::messaging::get_many_identity_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_identity_provider_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_identity_provider_request& v);
    static ores::iam::messaging::put_identity_provider_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_identity_provider_request& v);
    static ores::iam::messaging::put_identity_provider_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_identity_provider_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_identity_provider_response& v);
    static ores::iam::messaging::put_identity_provider_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_identity_provider_response& v);
    static ores::iam::messaging::put_identity_provider_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_identity_providers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_identity_providers_request& v);
    static ores::iam::messaging::put_many_identity_providers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_identity_providers_request& v);
    static ores::iam::messaging::put_many_identity_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_identity_providers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_identity_providers_response& v);
    static ores::iam::messaging::put_many_identity_providers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_identity_providers_response& v);
    static ores::iam::messaging::put_many_identity_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_identity_provider_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_identity_provider_request& v);
    static ores::iam::messaging::delete_identity_provider_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_identity_provider_request& v);
    static ores::iam::messaging::delete_identity_provider_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_identity_provider_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_identity_provider_response& v);
    static ores::iam::messaging::delete_identity_provider_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_identity_provider_response& v);
    static ores::iam::messaging::delete_identity_provider_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_identity_providers_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_identity_providers_request& v);
    static ores::iam::messaging::delete_many_identity_providers_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_identity_providers_request& v);
    static ores::iam::messaging::delete_many_identity_providers_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_identity_providers_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_identity_providers_response& v);
    static ores::iam::messaging::delete_many_identity_providers_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_identity_providers_response& v);
    static ores::iam::messaging::delete_many_identity_providers_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_identity_provider_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_identity_provider_versions_request& v);
    static ores::iam::messaging::list_identity_provider_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_identity_provider_versions_request& v);
    static ores::iam::messaging::list_identity_provider_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_identity_provider_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_identity_provider_versions_response& v);
    static ores::iam::messaging::list_identity_provider_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_identity_provider_versions_response& v);
    static ores::iam::messaging::list_identity_provider_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_identity_provider_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_identity_provider_version_request& v);
    static ores::iam::messaging::get_identity_provider_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_identity_provider_version_request& v);
    static ores::iam::messaging::get_identity_provider_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_identity_provider_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_identity_provider_version_response& v);
    static ores::iam::messaging::get_identity_provider_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_identity_provider_version_response& v);
    static ores::iam::messaging::get_identity_provider_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
