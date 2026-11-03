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
#ifndef ORES_IAM_API_MESSAGING_ROLE_GRANT_REQUEST_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_GRANT_REQUEST_PROTOCOL_HPP

#include "ores.iam.api/domain/role_grant_request.hpp"
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

struct role_grant_request_key {
    boost::uuids::uuid request_id;

    friend bool operator==(const role_grant_request_key&, const role_grant_request_key&) = default;
};

struct role_grant_request_write {
    boost::uuids::uuid request_id;
    boost::uuids::uuid account_id;

    friend bool operator==(const role_grant_request_write&,
                           const role_grant_request_write&) = default;
};

struct role_grant_request_change {
    role_grant_request_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const role_grant_request_change&,
                           const role_grant_request_change&) = default;
};

struct role_grant_request_removal {
    role_grant_request_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const role_grant_request_removal&,
                           const role_grant_request_removal&) = default;
};

struct role_grant_request_lookup {
    role_grant_request_key key;
    std::optional<ores::iam::domain::role_grant_request> role_grant_request;

    friend bool operator==(const role_grant_request_lookup&,
                           const role_grant_request_lookup&) = default;
};

struct role_grant_requests_filter {
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> request_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;

    friend bool operator==(const role_grant_requests_filter&,
                           const role_grant_requests_filter&) = default;
};

struct role_grant_request_event {
    boost::uuids::uuid event_id;
    role_grant_request_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const role_grant_request_event&,
                           const role_grant_request_event&) = default;
};

struct role_grant_request_version_key {
    role_grant_request_key role_grant_request;
    std::uint32_t version;

    friend bool operator==(const role_grant_request_version_key&,
                           const role_grant_request_version_key&) = default;
};

struct role_grant_request_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const role_grant_request_versions_filter&,
                           const role_grant_request_versions_filter&) = default;
};

struct list_role_grant_requests_request {
    using response_type = struct list_role_grant_requests_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests.list";
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
    std::optional<role_grant_requests_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_role_grant_requests_request&,
                           const list_role_grant_requests_request&) = default;
};

struct list_role_grant_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_grant_request> role_grant_requests;
    std::uint64_t total;

    friend bool operator==(const list_role_grant_requests_response&,
                           const list_role_grant_requests_response&) = default;
};

struct get_role_grant_request_request {
    using response_type = struct get_role_grant_request_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_key key;

    friend bool operator==(const get_role_grant_request_request&,
                           const get_role_grant_request_request&) = default;
};

struct get_role_grant_request_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_grant_request> role_grant_request;

    friend bool operator==(const get_role_grant_request_response&,
                           const get_role_grant_request_response&) = default;
};

struct get_many_role_grant_requests_request {
    using response_type = struct get_many_role_grant_requests_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_grant_request_key> keys;

    friend bool operator==(const get_many_role_grant_requests_request&,
                           const get_many_role_grant_requests_request&) = default;
};

struct get_many_role_grant_requests_response {
    ores::utility::domain::result result;
    std::vector<role_grant_request_lookup> entries;

    friend bool operator==(const get_many_role_grant_requests_response&,
                           const get_many_role_grant_requests_response&) = default;
};

struct put_role_grant_request_request {
    using response_type = struct put_role_grant_request_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_role_grant_request_request&,
                           const put_role_grant_request_request&) = default;
};

struct put_role_grant_request_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_grant_request> role_grant_request;

    friend bool operator==(const put_role_grant_request_response&,
                           const put_role_grant_request_response&) = default;
};

struct put_many_role_grant_requests_request {
    using response_type = struct put_many_role_grant_requests_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_grant_request_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_role_grant_requests_request&,
                           const put_many_role_grant_requests_request&) = default;
};

struct put_many_role_grant_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_grant_request> role_grant_requests;

    friend bool operator==(const put_many_role_grant_requests_response&,
                           const put_many_role_grant_requests_response&) = default;
};

struct delete_role_grant_request_request {
    using response_type = struct delete_role_grant_request_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_role_grant_request_request&,
                           const delete_role_grant_request_request&) = default;
};

struct delete_role_grant_request_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_role_grant_request_response&,
                           const delete_role_grant_request_response&) = default;
};

struct delete_many_role_grant_requests_request {
    using response_type = struct delete_many_role_grant_requests_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_grant_request_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_role_grant_requests_request&,
                           const delete_many_role_grant_requests_request&) = default;
};

struct delete_many_role_grant_requests_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_role_grant_requests_response&,
                           const delete_many_role_grant_requests_response&) = default;
};

struct list_by_account_id_role_grant_requests_request {
    using response_type = struct list_by_account_id_role_grant_requests_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.role_grant_requests.list_by_account_id";
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
    std::optional<role_grant_requests_filter> filter;

    friend bool operator==(const list_by_account_id_role_grant_requests_request&,
                           const list_by_account_id_role_grant_requests_request&) = default;
};

struct list_by_account_id_role_grant_requests_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_grant_request> role_grant_requests;
    std::uint64_t total;

    friend bool operator==(const list_by_account_id_role_grant_requests_response&,
                           const list_by_account_id_role_grant_requests_response&) = default;
};

struct list_role_grant_request_versions_request {
    using response_type = struct list_role_grant_request_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_grant_request_versions_filter> filter;

    friend bool operator==(const list_role_grant_request_versions_request&,
                           const list_role_grant_request_versions_request&) = default;
};

struct list_role_grant_request_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role_grant_request> versions;
    std::uint64_t total;

    friend bool operator==(const list_role_grant_request_versions_response&,
                           const list_role_grant_request_versions_response&) = default;
};

struct get_role_grant_request_version_request {
    using response_type = struct get_role_grant_request_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.role_grant_requests_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_grant_request_version_key key;

    friend bool operator==(const get_role_grant_request_version_request&,
                           const get_role_grant_request_version_request&) = default;
};

struct get_role_grant_request_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role_grant_request> version;

    friend bool operator==(const get_role_grant_request_version_response&,
                           const get_role_grant_request_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace role_grant_request_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.role_grant_requests.created";
inline constexpr std::string_view updated = "iam.v1.events.role_grant_requests.updated";
inline constexpr std::string_view deleted = "iam.v1.events.role_grant_requests.deleted";
}
/**
 * @brief The members of role_grant_request_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_key&) {
    return std::tuple{
        utility::codec::field{"request_id", &role_grant_request_key::request_id},
    };
}

/**
 * @brief The members of role_grant_request_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_write&) {
    return std::tuple{
        utility::codec::field{"request_id", &role_grant_request_write::request_id},
        utility::codec::field{"account_id", &role_grant_request_write::account_id},
    };
}

/**
 * @brief The members of role_grant_request_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_change&) {
    return std::tuple{
        utility::codec::field{"write", &role_grant_request_change::write},
        utility::codec::field{"precondition", &role_grant_request_change::precondition},
    };
}

/**
 * @brief The members of role_grant_request_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_removal&) {
    return std::tuple{
        utility::codec::field{"key", &role_grant_request_removal::key},
        utility::codec::field{"precondition", &role_grant_request_removal::precondition},
    };
}

/**
 * @brief The members of role_grant_request_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &role_grant_request_lookup::key},
        utility::codec::field{"role_grant_request", &role_grant_request_lookup::role_grant_request},
    };
}

/**
 * @brief The members of role_grant_requests_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_requests_filter&) {
    return std::tuple{
        utility::codec::field{"account_id", &role_grant_requests_filter::account_id},
        utility::codec::field{"request_id_one_of", &role_grant_requests_filter::request_id_one_of},
        utility::codec::field{"account_id_one_of", &role_grant_requests_filter::account_id_one_of},
    };
}

/**
 * @brief The members of role_grant_request_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &role_grant_request_event::event_id},
        utility::codec::field{"key", &role_grant_request_event::key},
        utility::codec::field{"action", &role_grant_request_event::action},
        utility::codec::field{"version", &role_grant_request_event::version},
        utility::codec::field{"occurred_at", &role_grant_request_event::occurred_at},
        utility::codec::field{"correlation_id", &role_grant_request_event::correlation_id},
    };
}

/**
 * @brief The members of role_grant_request_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_version_key&) {
    return std::tuple{
        utility::codec::field{"role_grant_request",
                              &role_grant_request_version_key::role_grant_request},
        utility::codec::field{"version", &role_grant_request_version_key::version},
    };
}

/**
 * @brief The members of role_grant_request_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const role_grant_request_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &role_grant_request_versions_filter::version},
        utility::codec::field{"from_version", &role_grant_request_versions_filter::from_version},
        utility::codec::field{"to_version", &role_grant_request_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_role_grant_requests_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_role_grant_requests_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_role_grant_requests_request::offset},
        utility::codec::field{"limit", &list_role_grant_requests_request::limit},
        utility::codec::field{"order", &list_role_grant_requests_request::order},
        utility::codec::field{"filter", &list_role_grant_requests_request::filter},
        utility::codec::field{"as_of", &list_role_grant_requests_request::as_of},
    };
}

/**
 * @brief The members of list_role_grant_requests_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_role_grant_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_role_grant_requests_response::result},
        utility::codec::field{"role_grant_requests",
                              &list_role_grant_requests_response::role_grant_requests},
        utility::codec::field{"total", &list_role_grant_requests_response::total},
    };
}

/**
 * @brief The members of get_role_grant_request_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_role_grant_request_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_role_grant_request_request::key},
    };
}

/**
 * @brief The members of get_role_grant_request_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_role_grant_request_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_role_grant_request_response::result},
        utility::codec::field{"role_grant_request",
                              &get_role_grant_request_response::role_grant_request},
    };
}

/**
 * @brief The members of get_many_role_grant_requests_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_role_grant_requests_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_role_grant_requests_request::keys},
    };
}

/**
 * @brief The members of get_many_role_grant_requests_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_role_grant_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_role_grant_requests_response::result},
        utility::codec::field{"entries", &get_many_role_grant_requests_response::entries},
    };
}

/**
 * @brief The members of put_role_grant_request_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_role_grant_request_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_role_grant_request_request::change},
        utility::codec::field{"intent", &put_role_grant_request_request::intent},
    };
}

/**
 * @brief The members of put_role_grant_request_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_role_grant_request_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_role_grant_request_response::result},
        utility::codec::field{"role_grant_request",
                              &put_role_grant_request_response::role_grant_request},
    };
}

/**
 * @brief The members of put_many_role_grant_requests_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_role_grant_requests_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_role_grant_requests_request::changes},
        utility::codec::field{"intent", &put_many_role_grant_requests_request::intent},
    };
}

/**
 * @brief The members of put_many_role_grant_requests_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_role_grant_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_role_grant_requests_response::result},
        utility::codec::field{"role_grant_requests",
                              &put_many_role_grant_requests_response::role_grant_requests},
    };
}

/**
 * @brief The members of delete_role_grant_request_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_role_grant_request_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_role_grant_request_request::removal},
        utility::codec::field{"intent", &delete_role_grant_request_request::intent},
    };
}

/**
 * @brief The members of delete_role_grant_request_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_role_grant_request_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_role_grant_request_response::result},
    };
}

/**
 * @brief The members of delete_many_role_grant_requests_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_role_grant_requests_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_role_grant_requests_request::removals},
        utility::codec::field{"intent", &delete_many_role_grant_requests_request::intent},
    };
}

/**
 * @brief The members of delete_many_role_grant_requests_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_role_grant_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_role_grant_requests_response::result},
    };
}

/**
 * @brief The members of list_by_account_id_role_grant_requests_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_account_id_role_grant_requests_request&) {
    return std::tuple{
        utility::codec::field{"account_id",
                              &list_by_account_id_role_grant_requests_request::account_id},
        utility::codec::field{"scope", &list_by_account_id_role_grant_requests_request::scope},
        utility::codec::field{"offset", &list_by_account_id_role_grant_requests_request::offset},
        utility::codec::field{"limit", &list_by_account_id_role_grant_requests_request::limit},
        utility::codec::field{"order", &list_by_account_id_role_grant_requests_request::order},
        utility::codec::field{"filter", &list_by_account_id_role_grant_requests_request::filter},
    };
}

/**
 * @brief The members of list_by_account_id_role_grant_requests_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_account_id_role_grant_requests_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_account_id_role_grant_requests_response::result},
        utility::codec::field{
            "role_grant_requests",
            &list_by_account_id_role_grant_requests_response::role_grant_requests},
        utility::codec::field{"total", &list_by_account_id_role_grant_requests_response::total},
    };
}

/**
 * @brief The members of list_role_grant_request_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_role_grant_request_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_role_grant_request_versions_request::key},
        utility::codec::field{"offset", &list_role_grant_request_versions_request::offset},
        utility::codec::field{"limit", &list_role_grant_request_versions_request::limit},
        utility::codec::field{"order", &list_role_grant_request_versions_request::order},
        utility::codec::field{"filter", &list_role_grant_request_versions_request::filter},
    };
}

/**
 * @brief The members of list_role_grant_request_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_role_grant_request_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_role_grant_request_versions_response::result},
        utility::codec::field{"versions", &list_role_grant_request_versions_response::versions},
        utility::codec::field{"total", &list_role_grant_request_versions_response::total},
    };
}

/**
 * @brief The members of get_role_grant_request_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_role_grant_request_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_role_grant_request_version_request::key},
    };
}

/**
 * @brief The members of get_role_grant_request_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_role_grant_request_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_role_grant_request_version_response::result},
        utility::codec::field{"version", &get_role_grant_request_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_request_key& v);
    static ores::iam::messaging::role_grant_request_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_request_key& v);
    static ores::iam::messaging::role_grant_request_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_request_write& v);
    static ores::iam::messaging::role_grant_request_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_request_write& v);
    static ores::iam::messaging::role_grant_request_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_request_change& v);
    static ores::iam::messaging::role_grant_request_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_request_change& v);
    static ores::iam::messaging::role_grant_request_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_request_removal& v);
    static ores::iam::messaging::role_grant_request_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_request_removal& v);
    static ores::iam::messaging::role_grant_request_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_request_lookup& v);
    static ores::iam::messaging::role_grant_request_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_request_lookup& v);
    static ores::iam::messaging::role_grant_request_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_requests_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_requests_filter& v);
    static ores::iam::messaging::role_grant_requests_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_requests_filter& v);
    static ores::iam::messaging::role_grant_requests_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_request_event& v);
    static ores::iam::messaging::role_grant_request_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_request_event& v);
    static ores::iam::messaging::role_grant_request_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::role_grant_request_version_key& v);
    static ores::iam::messaging::role_grant_request_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::role_grant_request_version_key& v);
    static ores::iam::messaging::role_grant_request_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::role_grant_request_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::role_grant_request_versions_filter& v);
    static ores::iam::messaging::role_grant_request_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::role_grant_request_versions_filter& v);
    static ores::iam::messaging::role_grant_request_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_grant_requests_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_grant_requests_request& v);
    static ores::iam::messaging::list_role_grant_requests_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_role_grant_requests_request& v);
    static ores::iam::messaging::list_role_grant_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_role_grant_requests_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_role_grant_requests_response& v);
    static ores::iam::messaging::list_role_grant_requests_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_role_grant_requests_response& v);
    static ores::iam::messaging::list_role_grant_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_grant_request_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_grant_request_request& v);
    static ores::iam::messaging::get_role_grant_request_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_role_grant_request_request& v);
    static ores::iam::messaging::get_role_grant_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_role_grant_request_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_role_grant_request_response& v);
    static ores::iam::messaging::get_role_grant_request_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_role_grant_request_response& v);
    static ores::iam::messaging::get_role_grant_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_many_role_grant_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_role_grant_requests_request& v);
    static ores::iam::messaging::get_many_role_grant_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_role_grant_requests_request& v);
    static ores::iam::messaging::get_many_role_grant_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_many_role_grant_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_role_grant_requests_response& v);
    static ores::iam::messaging::get_many_role_grant_requests_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_role_grant_requests_response& v);
    static ores::iam::messaging::get_many_role_grant_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_grant_request_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_grant_request_request& v);
    static ores::iam::messaging::put_role_grant_request_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::put_role_grant_request_request& v);
    static ores::iam::messaging::put_role_grant_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_role_grant_request_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::put_role_grant_request_response& v);
    static ores::iam::messaging::put_role_grant_request_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_role_grant_request_response& v);
    static ores::iam::messaging::put_role_grant_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_many_role_grant_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_role_grant_requests_request& v);
    static ores::iam::messaging::put_many_role_grant_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_role_grant_requests_request& v);
    static ores::iam::messaging::put_many_role_grant_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_many_role_grant_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_role_grant_requests_response& v);
    static ores::iam::messaging::put_many_role_grant_requests_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_role_grant_requests_response& v);
    static ores::iam::messaging::put_many_role_grant_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_grant_request_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::delete_role_grant_request_request& v);
    static ores::iam::messaging::delete_role_grant_request_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_role_grant_request_request& v);
    static ores::iam::messaging::delete_role_grant_request_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::delete_role_grant_request_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_role_grant_request_response& v);
    static ores::iam::messaging::delete_role_grant_request_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_role_grant_request_response& v);
    static ores::iam::messaging::delete_role_grant_request_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_role_grant_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_role_grant_requests_request& v);
    static ores::iam::messaging::delete_many_role_grant_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_role_grant_requests_request& v);
    static ores::iam::messaging::delete_many_role_grant_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_role_grant_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_role_grant_requests_response& v);
    static ores::iam::messaging::delete_many_role_grant_requests_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_role_grant_requests_response& v);
    static ores::iam::messaging::delete_many_role_grant_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_role_grant_requests_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_role_grant_requests_request& v);
    static ores::iam::messaging::list_by_account_id_role_grant_requests_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_role_grant_requests_request& v);
    static ores::iam::messaging::list_by_account_id_role_grant_requests_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_account_id_role_grant_requests_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_by_account_id_role_grant_requests_response& v);
    static ores::iam::messaging::list_by_account_id_role_grant_requests_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_by_account_id_role_grant_requests_response& v);
    static ores::iam::messaging::list_by_account_id_role_grant_requests_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_role_grant_request_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_role_grant_request_versions_request& v);
    static ores::iam::messaging::list_role_grant_request_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_role_grant_request_versions_request& v);
    static ores::iam::messaging::list_role_grant_request_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_role_grant_request_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_role_grant_request_versions_response& v);
    static ores::iam::messaging::list_role_grant_request_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_role_grant_request_versions_response& v);
    static ores::iam::messaging::list_role_grant_request_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_role_grant_request_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_role_grant_request_version_request& v);
    static ores::iam::messaging::get_role_grant_request_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_role_grant_request_version_request& v);
    static ores::iam::messaging::get_role_grant_request_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_role_grant_request_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_role_grant_request_version_response& v);
    static ores::iam::messaging::get_role_grant_request_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_role_grant_request_version_response& v);
    static ores::iam::messaging::get_role_grant_request_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
