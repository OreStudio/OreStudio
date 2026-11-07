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
#ifndef ORES_INBOX_API_MESSAGING_APPROVAL_PART_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_APPROVAL_PART_PROTOCOL_HPP

#include "ores.inbox.api/domain/approval_part_codec.hpp"
#include "ores.inbox.api/export.hpp"
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

namespace ores::inbox::messaging {

struct approval_part_key {
    std::string code;

    friend bool operator==(const approval_part_key&, const approval_part_key&) = default;
};

struct approval_part_write {
    std::string code;
    std::string name;
    std::string description;
    std::string decide_permission_code;
    int answer_order;
    int display_order;

    friend bool operator==(const approval_part_write&, const approval_part_write&) = default;
};

struct approval_part_change {
    approval_part_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const approval_part_change&, const approval_part_change&) = default;
};

struct approval_part_removal {
    approval_part_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const approval_part_removal&, const approval_part_removal&) = default;
};

struct approval_part_lookup {
    approval_part_key key;
    std::optional<ores::inbox::domain::approval_part> approval_part;

    friend bool operator==(const approval_part_lookup&, const approval_part_lookup&) = default;
};

struct approval_parts_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const approval_parts_filter&, const approval_parts_filter&) = default;
};

struct approval_part_event {
    boost::uuids::uuid event_id;
    approval_part_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const approval_part_event&, const approval_part_event&) = default;
};

struct approval_part_version_key {
    approval_part_key approval_part;
    std::uint32_t version;

    friend bool operator==(const approval_part_version_key&,
                           const approval_part_version_key&) = default;
};

struct approval_part_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const approval_part_versions_filter&,
                           const approval_part_versions_filter&) = default;
};

struct list_approval_parts_request {
    using response_type = struct list_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.list";
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
    std::optional<approval_parts_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_approval_parts_request&,
                           const list_approval_parts_request&) = default;
};

struct list_approval_parts_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_part> parts;
    std::uint64_t total;

    friend bool operator==(const list_approval_parts_response&,
                           const list_approval_parts_response&) = default;
};

struct get_approval_part_request {
    using response_type = struct get_approval_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_key key;

    friend bool operator==(const get_approval_part_request&,
                           const get_approval_part_request&) = default;
};

struct get_approval_part_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_part> approval_part;

    friend bool operator==(const get_approval_part_response&,
                           const get_approval_part_response&) = default;
};

struct get_many_approval_parts_request {
    using response_type = struct get_many_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_part_key> keys;

    friend bool operator==(const get_many_approval_parts_request&,
                           const get_many_approval_parts_request&) = default;
};

struct get_many_approval_parts_response {
    ores::utility::domain::result result;
    std::vector<approval_part_lookup> entries;

    friend bool operator==(const get_many_approval_parts_response&,
                           const get_many_approval_parts_response&) = default;
};

struct put_approval_part_request {
    using response_type = struct put_approval_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_approval_part_request&,
                           const put_approval_part_request&) = default;
};

struct put_approval_part_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_part> approval_part;

    friend bool operator==(const put_approval_part_response&,
                           const put_approval_part_response&) = default;
};

struct put_many_approval_parts_request {
    using response_type = struct put_many_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_part_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_approval_parts_request&,
                           const put_many_approval_parts_request&) = default;
};

struct put_many_approval_parts_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_part> parts;

    friend bool operator==(const put_many_approval_parts_response&,
                           const put_many_approval_parts_response&) = default;
};

struct delete_approval_part_request {
    using response_type = struct delete_approval_part_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_approval_part_request&,
                           const delete_approval_part_request&) = default;
};

struct delete_approval_part_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_approval_part_response&,
                           const delete_approval_part_response&) = default;
};

struct delete_many_approval_parts_request {
    using response_type = struct delete_many_approval_parts_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_part_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_approval_parts_request&,
                           const delete_many_approval_parts_request&) = default;
};

struct delete_many_approval_parts_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_approval_parts_response&,
                           const delete_many_approval_parts_response&) = default;
};

struct list_approval_part_versions_request {
    using response_type = struct list_approval_part_versions_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<approval_part_versions_filter> filter;

    friend bool operator==(const list_approval_part_versions_request&,
                           const list_approval_part_versions_request&) = default;
};

struct list_approval_part_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_part> versions;
    std::uint64_t total;

    friend bool operator==(const list_approval_part_versions_response&,
                           const list_approval_part_versions_response&) = default;
};

struct get_approval_part_version_request {
    using response_type = struct get_approval_part_version_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_parts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_part_version_key key;

    friend bool operator==(const get_approval_part_version_request&,
                           const get_approval_part_version_request&) = default;
};

struct get_approval_part_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_part> version;

    friend bool operator==(const get_approval_part_version_response&,
                           const get_approval_part_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace approval_part_event_subjects {
inline constexpr std::string_view created = "inbox.v1.events.approval_parts.created";
inline constexpr std::string_view updated = "inbox.v1.events.approval_parts.updated";
inline constexpr std::string_view deleted = "inbox.v1.events.approval_parts.deleted";
}
/**
 * @brief The members of approval_part_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_key>) {
    return std::tuple{
        utility::codec::field{"code", &approval_part_key::code},
    };
}

/**
 * @brief The members of approval_part_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_write>) {
    return std::tuple{
        utility::codec::field{"code", &approval_part_write::code},
        utility::codec::field{"name", &approval_part_write::name},
        utility::codec::field{"description", &approval_part_write::description},
        utility::codec::field{"decide_permission_code",
                              &approval_part_write::decide_permission_code},
        utility::codec::field{"answer_order", &approval_part_write::answer_order},
        utility::codec::field{"display_order", &approval_part_write::display_order},
    };
}

/**
 * @brief The members of approval_part_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_change>) {
    return std::tuple{
        utility::codec::field{"write", &approval_part_change::write},
        utility::codec::field{"precondition", &approval_part_change::precondition},
    };
}

/**
 * @brief The members of approval_part_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_removal>) {
    return std::tuple{
        utility::codec::field{"key", &approval_part_removal::key},
        utility::codec::field{"precondition", &approval_part_removal::precondition},
    };
}

/**
 * @brief The members of approval_part_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &approval_part_lookup::key},
        utility::codec::field{"approval_part", &approval_part_lookup::approval_part},
    };
}

/**
 * @brief The members of approval_parts_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_parts_filter>) {
    return std::tuple{
        utility::codec::field{"code_one_of", &approval_parts_filter::code_one_of},
    };
}

/**
 * @brief The members of approval_part_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &approval_part_event::event_id},
        utility::codec::field{"key", &approval_part_event::key},
        utility::codec::field{"action", &approval_part_event::action},
        utility::codec::field{"version", &approval_part_event::version},
        utility::codec::field{"occurred_at", &approval_part_event::occurred_at},
        utility::codec::field{"correlation_id", &approval_part_event::correlation_id},
    };
}

/**
 * @brief The members of approval_part_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_version_key>) {
    return std::tuple{
        utility::codec::field{"approval_part", &approval_part_version_key::approval_part},
        utility::codec::field{"version", &approval_part_version_key::version},
    };
}

/**
 * @brief The members of approval_part_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_part_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &approval_part_versions_filter::version},
        utility::codec::field{"from_version", &approval_part_versions_filter::from_version},
        utility::codec::field{"to_version", &approval_part_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_approval_parts_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_approval_parts_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_approval_parts_request::offset},
        utility::codec::field{"limit", &list_approval_parts_request::limit},
        utility::codec::field{"order", &list_approval_parts_request::order},
        utility::codec::field{"filter", &list_approval_parts_request::filter},
        utility::codec::field{"as_of", &list_approval_parts_request::as_of},
    };
}

/**
 * @brief The members of list_approval_parts_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_approval_parts_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_approval_parts_response::result},
        utility::codec::field{"parts", &list_approval_parts_response::parts},
        utility::codec::field{"total", &list_approval_parts_response::total},
    };
}

/**
 * @brief The members of get_approval_part_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_part_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_approval_part_request::key},
    };
}

/**
 * @brief The members of get_approval_part_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_part_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_approval_part_response::result},
        utility::codec::field{"approval_part", &get_approval_part_response::approval_part},
    };
}

/**
 * @brief The members of get_many_approval_parts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_approval_parts_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_approval_parts_request::keys},
    };
}

/**
 * @brief The members of get_many_approval_parts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_approval_parts_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_approval_parts_response::result},
        utility::codec::field{"entries", &get_many_approval_parts_response::entries},
    };
}

/**
 * @brief The members of put_approval_part_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_approval_part_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_approval_part_request::change},
        utility::codec::field{"intent", &put_approval_part_request::intent},
    };
}

/**
 * @brief The members of put_approval_part_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_approval_part_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_approval_part_response::result},
        utility::codec::field{"approval_part", &put_approval_part_response::approval_part},
    };
}

/**
 * @brief The members of put_many_approval_parts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_approval_parts_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_approval_parts_request::changes},
        utility::codec::field{"intent", &put_many_approval_parts_request::intent},
    };
}

/**
 * @brief The members of put_many_approval_parts_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_approval_parts_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_approval_parts_response::result},
        utility::codec::field{"parts", &put_many_approval_parts_response::parts},
    };
}

/**
 * @brief The members of delete_approval_part_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_approval_part_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_approval_part_request::removal},
        utility::codec::field{"intent", &delete_approval_part_request::intent},
    };
}

/**
 * @brief The members of delete_approval_part_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_approval_part_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_approval_part_response::result},
    };
}

/**
 * @brief The members of delete_many_approval_parts_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_approval_parts_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_approval_parts_request::removals},
        utility::codec::field{"intent", &delete_many_approval_parts_request::intent},
    };
}

/**
 * @brief The members of delete_many_approval_parts_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_approval_parts_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_approval_parts_response::result},
    };
}

/**
 * @brief The members of list_approval_part_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_approval_part_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_approval_part_versions_request::key},
        utility::codec::field{"offset", &list_approval_part_versions_request::offset},
        utility::codec::field{"limit", &list_approval_part_versions_request::limit},
        utility::codec::field{"order", &list_approval_part_versions_request::order},
        utility::codec::field{"filter", &list_approval_part_versions_request::filter},
    };
}

/**
 * @brief The members of list_approval_part_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_approval_part_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_approval_part_versions_response::result},
        utility::codec::field{"versions", &list_approval_part_versions_response::versions},
        utility::codec::field{"total", &list_approval_part_versions_response::total},
    };
}

/**
 * @brief The members of get_approval_part_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_part_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_approval_part_version_request::key},
    };
}

/**
 * @brief The members of get_approval_part_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_approval_part_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_approval_part_version_response::result},
        utility::codec::field{"version", &get_approval_part_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_key& v);
    static ores::inbox::messaging::approval_part_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_key& v);
    static ores::inbox::messaging::approval_part_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_write& v);
    static ores::inbox::messaging::approval_part_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_write& v);
    static ores::inbox::messaging::approval_part_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_change& v);
    static ores::inbox::messaging::approval_part_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_change& v);
    static ores::inbox::messaging::approval_part_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_removal& v);
    static ores::inbox::messaging::approval_part_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_removal& v);
    static ores::inbox::messaging::approval_part_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_lookup& v);
    static ores::inbox::messaging::approval_part_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_lookup& v);
    static ores::inbox::messaging::approval_part_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_parts_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_parts_filter& v);
    static ores::inbox::messaging::approval_parts_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_parts_filter& v);
    static ores::inbox::messaging::approval_parts_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_event& v);
    static ores::inbox::messaging::approval_part_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_event& v);
    static ores::inbox::messaging::approval_part_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_version_key& v);
    static ores::inbox::messaging::approval_part_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::approval_part_version_key& v);
    static ores::inbox::messaging::approval_part_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::approval_part_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::approval_part_versions_filter& v);
    static ores::inbox::messaging::approval_part_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::approval_part_versions_filter& v);
    static ores::inbox::messaging::approval_part_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_approval_parts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_approval_parts_request& v);
    static ores::inbox::messaging::list_approval_parts_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::list_approval_parts_request& v);
    static ores::inbox::messaging::list_approval_parts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::list_approval_parts_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::list_approval_parts_response& v);
    static ores::inbox::messaging::list_approval_parts_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::list_approval_parts_response& v);
    static ores::inbox::messaging::list_approval_parts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_part_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_part_request& v);
    static ores::inbox::messaging::get_approval_part_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_approval_part_request& v);
    static ores::inbox::messaging::get_approval_part_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_approval_part_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_approval_part_response& v);
    static ores::inbox::messaging::get_approval_part_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::get_approval_part_response& v);
    static ores::inbox::messaging::get_approval_part_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_many_approval_parts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::get_many_approval_parts_request& v);
    static ores::inbox::messaging::get_many_approval_parts_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_approval_parts_request& v);
    static ores::inbox::messaging::get_many_approval_parts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::get_many_approval_parts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_approval_parts_response& v);
    static ores::inbox::messaging::get_many_approval_parts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_approval_parts_response& v);
    static ores::inbox::messaging::get_many_approval_parts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_approval_part_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_approval_part_request& v);
    static ores::inbox::messaging::put_approval_part_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::put_approval_part_request& v);
    static ores::inbox::messaging::put_approval_part_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_approval_part_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_approval_part_response& v);
    static ores::inbox::messaging::put_approval_part_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::put_approval_part_response& v);
    static ores::inbox::messaging::put_approval_part_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_many_approval_parts_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::put_many_approval_parts_request& v);
    static ores::inbox::messaging::put_many_approval_parts_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_approval_parts_request& v);
    static ores::inbox::messaging::put_many_approval_parts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::put_many_approval_parts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::put_many_approval_parts_response& v);
    static ores::inbox::messaging::put_many_approval_parts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::put_many_approval_parts_response& v);
    static ores::inbox::messaging::put_many_approval_parts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delete_approval_part_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delete_approval_part_request& v);
    static ores::inbox::messaging::delete_approval_part_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::delete_approval_part_request& v);
    static ores::inbox::messaging::delete_approval_part_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delete_approval_part_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delete_approval_part_response& v);
    static ores::inbox::messaging::delete_approval_part_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_approval_part_response& v);
    static ores::inbox::messaging::delete_approval_part_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_approval_parts_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_approval_parts_request& v);
    static ores::inbox::messaging::delete_many_approval_parts_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_approval_parts_request& v);
    static ores::inbox::messaging::delete_many_approval_parts_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::delete_many_approval_parts_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::delete_many_approval_parts_response& v);
    static ores::inbox::messaging::delete_many_approval_parts_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delete_many_approval_parts_response& v);
    static ores::inbox::messaging::delete_many_approval_parts_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_approval_part_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_approval_part_versions_request& v);
    static ores::inbox::messaging::list_approval_part_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_approval_part_versions_request& v);
    static ores::inbox::messaging::list_approval_part_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_approval_part_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_approval_part_versions_response& v);
    static ores::inbox::messaging::list_approval_part_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_approval_part_versions_response& v);
    static ores::inbox::messaging::list_approval_part_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_approval_part_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_approval_part_version_request& v);
    static ores::inbox::messaging::get_approval_part_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_approval_part_version_request& v);
    static ores::inbox::messaging::get_approval_part_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_approval_part_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_approval_part_version_response& v);
    static ores::inbox::messaging::get_approval_part_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_approval_part_version_response& v);
    static ores::inbox::messaging::get_approval_part_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
