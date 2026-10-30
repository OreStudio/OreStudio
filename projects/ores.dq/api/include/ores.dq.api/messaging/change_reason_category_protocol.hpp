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
#ifndef ORES_DQ_API_MESSAGING_CHANGE_REASON_CATEGORY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_CHANGE_REASON_CATEGORY_PROTOCOL_HPP

#include "ores.dq.api/domain/change_reason_category.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct change_reason_category_key {
    std::string code;
};

struct change_reason_category_write {
    std::string code;
    std::string description;
};

struct change_reason_category_change {
    change_reason_category_write write;
    ores::utility::domain::precondition precondition;
};

struct change_reason_category_removal {
    change_reason_category_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct change_reason_category_lookup {
    change_reason_category_key key;
    std::optional<ores::dq::domain::change_reason_category> change_reason_category;
};

struct change_reason_categories_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct change_reason_category_event {
    boost::uuids::uuid event_id;
    change_reason_category_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct change_reason_category_version_key {
    change_reason_category_key change_reason_category;
    std::uint32_t version;
};

struct change_reason_category_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_change_reason_categories_request {
    using response_type = struct list_change_reason_categories_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories.list";
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
    std::optional<change_reason_categories_filter> filter;
    std::optional<std::string> as_of;
};

struct list_change_reason_categories_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::change_reason_category> categories;
    std::uint64_t total;
};

struct get_change_reason_category_request {
    using response_type = struct get_change_reason_category_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    change_reason_category_key key;
};

struct get_change_reason_category_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::change_reason_category> change_reason_category;
};

struct get_many_change_reason_categories_request {
    using response_type = struct get_many_change_reason_categories_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<change_reason_category_key> keys;
};

struct get_many_change_reason_categories_response {
    ores::utility::domain::result result;
    std::vector<change_reason_category_lookup> entries;
};

struct put_change_reason_category_request {
    using response_type = struct put_change_reason_category_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    change_reason_category_change change;
    ores::utility::domain::change_intent intent;
};

struct put_change_reason_category_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::change_reason_category> change_reason_category;
};

struct put_many_change_reason_categories_request {
    using response_type = struct put_many_change_reason_categories_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<change_reason_category_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_change_reason_categories_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::change_reason_category> categories;
};

struct delete_change_reason_category_request {
    using response_type = struct delete_change_reason_category_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    change_reason_category_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_change_reason_category_response {
    ores::utility::domain::result result;
};

struct delete_many_change_reason_categories_request {
    using response_type = struct delete_many_change_reason_categories_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<change_reason_category_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_change_reason_categories_response {
    ores::utility::domain::result result;
};

struct list_change_reason_category_versions_request {
    using response_type = struct list_change_reason_category_versions_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    change_reason_category_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<change_reason_category_versions_filter> filter;
};

struct list_change_reason_category_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::change_reason_category> versions;
    std::uint64_t total;
};

struct get_change_reason_category_version_request {
    using response_type = struct get_change_reason_category_version_response;
    static constexpr std::string_view nats_subject = "dq.v1.change_reason_categories_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    change_reason_category_version_key key;
};

struct get_change_reason_category_version_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::change_reason_category> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace change_reason_category_event_subjects {
inline constexpr std::string_view created = "dq.v1.change_reason_categories_events.created";
inline constexpr std::string_view updated = "dq.v1.change_reason_categories_events.updated";
inline constexpr std::string_view deleted = "dq.v1.change_reason_categories_events.deleted";
}
/**
 * @brief The members of change_reason_category_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_key&) {
    return std::tuple{
        utility::codec::field{"code", &change_reason_category_key::code},
    };
}

/**
 * @brief The members of change_reason_category_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_write&) {
    return std::tuple{
        utility::codec::field{"code", &change_reason_category_write::code},
        utility::codec::field{"description", &change_reason_category_write::description},
    };
}

/**
 * @brief The members of change_reason_category_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_change&) {
    return std::tuple{
        utility::codec::field{"write", &change_reason_category_change::write},
        utility::codec::field{"precondition", &change_reason_category_change::precondition},
    };
}

/**
 * @brief The members of change_reason_category_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_removal&) {
    return std::tuple{
        utility::codec::field{"key", &change_reason_category_removal::key},
        utility::codec::field{"precondition", &change_reason_category_removal::precondition},
    };
}

/**
 * @brief The members of change_reason_category_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &change_reason_category_lookup::key},
        utility::codec::field{"change_reason_category",
                              &change_reason_category_lookup::change_reason_category},
    };
}

/**
 * @brief The members of change_reason_categories_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_categories_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &change_reason_categories_filter::code_one_of},
    };
}

/**
 * @brief The members of change_reason_category_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &change_reason_category_event::event_id},
        utility::codec::field{"key", &change_reason_category_event::key},
        utility::codec::field{"action", &change_reason_category_event::action},
        utility::codec::field{"version", &change_reason_category_event::version},
        utility::codec::field{"occurred_at", &change_reason_category_event::occurred_at},
        utility::codec::field{"correlation_id", &change_reason_category_event::correlation_id},
    };
}

/**
 * @brief The members of change_reason_category_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_version_key&) {
    return std::tuple{
        utility::codec::field{"change_reason_category",
                              &change_reason_category_version_key::change_reason_category},
        utility::codec::field{"version", &change_reason_category_version_key::version},
    };
}

/**
 * @brief The members of change_reason_category_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const change_reason_category_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &change_reason_category_versions_filter::version},
        utility::codec::field{"from_version",
                              &change_reason_category_versions_filter::from_version},
        utility::codec::field{"to_version", &change_reason_category_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_change_reason_categories_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_change_reason_categories_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_change_reason_categories_request::offset},
        utility::codec::field{"limit", &list_change_reason_categories_request::limit},
        utility::codec::field{"order", &list_change_reason_categories_request::order},
        utility::codec::field{"filter", &list_change_reason_categories_request::filter},
        utility::codec::field{"as_of", &list_change_reason_categories_request::as_of},
    };
}

/**
 * @brief The members of list_change_reason_categories_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_change_reason_categories_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_change_reason_categories_response::result},
        utility::codec::field{"categories", &list_change_reason_categories_response::categories},
        utility::codec::field{"total", &list_change_reason_categories_response::total},
    };
}

/**
 * @brief The members of get_change_reason_category_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_change_reason_category_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_change_reason_category_request::key},
    };
}

/**
 * @brief The members of get_change_reason_category_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_change_reason_category_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_change_reason_category_response::result},
        utility::codec::field{"change_reason_category",
                              &get_change_reason_category_response::change_reason_category},
    };
}

/**
 * @brief The members of get_many_change_reason_categories_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_change_reason_categories_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_change_reason_categories_request::keys},
    };
}

/**
 * @brief The members of get_many_change_reason_categories_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_change_reason_categories_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_change_reason_categories_response::result},
        utility::codec::field{"entries", &get_many_change_reason_categories_response::entries},
    };
}

/**
 * @brief The members of put_change_reason_category_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_change_reason_category_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_change_reason_category_request::change},
        utility::codec::field{"intent", &put_change_reason_category_request::intent},
    };
}

/**
 * @brief The members of put_change_reason_category_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_change_reason_category_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_change_reason_category_response::result},
        utility::codec::field{"change_reason_category",
                              &put_change_reason_category_response::change_reason_category},
    };
}

/**
 * @brief The members of put_many_change_reason_categories_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_change_reason_categories_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_change_reason_categories_request::changes},
        utility::codec::field{"intent", &put_many_change_reason_categories_request::intent},
    };
}

/**
 * @brief The members of put_many_change_reason_categories_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_change_reason_categories_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_change_reason_categories_response::result},
        utility::codec::field{"categories",
                              &put_many_change_reason_categories_response::categories},
    };
}

/**
 * @brief The members of delete_change_reason_category_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_change_reason_category_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_change_reason_category_request::removal},
        utility::codec::field{"intent", &delete_change_reason_category_request::intent},
    };
}

/**
 * @brief The members of delete_change_reason_category_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_change_reason_category_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_change_reason_category_response::result},
    };
}

/**
 * @brief The members of delete_many_change_reason_categories_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_change_reason_categories_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_change_reason_categories_request::removals},
        utility::codec::field{"intent", &delete_many_change_reason_categories_request::intent},
    };
}

/**
 * @brief The members of delete_many_change_reason_categories_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_change_reason_categories_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_change_reason_categories_response::result},
    };
}

/**
 * @brief The members of list_change_reason_category_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_change_reason_category_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_change_reason_category_versions_request::key},
        utility::codec::field{"offset", &list_change_reason_category_versions_request::offset},
        utility::codec::field{"limit", &list_change_reason_category_versions_request::limit},
        utility::codec::field{"order", &list_change_reason_category_versions_request::order},
        utility::codec::field{"filter", &list_change_reason_category_versions_request::filter},
    };
}

/**
 * @brief The members of list_change_reason_category_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_change_reason_category_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_change_reason_category_versions_response::result},
        utility::codec::field{"versions", &list_change_reason_category_versions_response::versions},
        utility::codec::field{"total", &list_change_reason_category_versions_response::total},
    };
}

/**
 * @brief The members of get_change_reason_category_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_change_reason_category_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_change_reason_category_version_request::key},
    };
}

/**
 * @brief The members of get_change_reason_category_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_change_reason_category_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_change_reason_category_version_response::result},
        utility::codec::field{"version", &get_change_reason_category_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_category_key& v);
    static ores::dq::messaging::change_reason_category_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::change_reason_category_key& v);
    static ores::dq::messaging::change_reason_category_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_category_write& v);
    static ores::dq::messaging::change_reason_category_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::change_reason_category_write& v);
    static ores::dq::messaging::change_reason_category_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_category_change& v);
    static ores::dq::messaging::change_reason_category_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::change_reason_category_change& v);
    static ores::dq::messaging::change_reason_category_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_category_removal& v);
    static ores::dq::messaging::change_reason_category_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::change_reason_category_removal& v);
    static ores::dq::messaging::change_reason_category_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_category_lookup& v);
    static ores::dq::messaging::change_reason_category_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::change_reason_category_lookup& v);
    static ores::dq::messaging::change_reason_category_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_categories_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_categories_filter& v);
    static ores::dq::messaging::change_reason_categories_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::change_reason_categories_filter& v);
    static ores::dq::messaging::change_reason_categories_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_category_event& v);
    static ores::dq::messaging::change_reason_category_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::change_reason_category_event& v);
    static ores::dq::messaging::change_reason_category_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::change_reason_category_version_key& v);
    static ores::dq::messaging::change_reason_category_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::change_reason_category_version_key& v);
    static ores::dq::messaging::change_reason_category_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::change_reason_category_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::change_reason_category_versions_filter& v);
    static ores::dq::messaging::change_reason_category_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::change_reason_category_versions_filter& v);
    static ores::dq::messaging::change_reason_category_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_change_reason_categories_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_change_reason_categories_request& v);
    static ores::dq::messaging::list_change_reason_categories_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_change_reason_categories_request& v);
    static ores::dq::messaging::list_change_reason_categories_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_change_reason_categories_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_change_reason_categories_response& v);
    static ores::dq::messaging::list_change_reason_categories_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_change_reason_categories_response& v);
    static ores::dq::messaging::list_change_reason_categories_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_change_reason_category_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_change_reason_category_request& v);
    static ores::dq::messaging::get_change_reason_category_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_change_reason_category_request& v);
    static ores::dq::messaging::get_change_reason_category_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_change_reason_category_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_change_reason_category_response& v);
    static ores::dq::messaging::get_change_reason_category_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_change_reason_category_response& v);
    static ores::dq::messaging::get_change_reason_category_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::get_many_change_reason_categories_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_many_change_reason_categories_request& v);
    static ores::dq::messaging::get_many_change_reason_categories_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_change_reason_categories_request& v);
    static ores::dq::messaging::get_many_change_reason_categories_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::get_many_change_reason_categories_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_many_change_reason_categories_response& v);
    static ores::dq::messaging::get_many_change_reason_categories_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_change_reason_categories_response& v);
    static ores::dq::messaging::get_many_change_reason_categories_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_change_reason_category_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_change_reason_category_request& v);
    static ores::dq::messaging::put_change_reason_category_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_change_reason_category_request& v);
    static ores::dq::messaging::put_change_reason_category_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_change_reason_category_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::put_change_reason_category_response& v);
    static ores::dq::messaging::put_change_reason_category_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_change_reason_category_response& v);
    static ores::dq::messaging::put_change_reason_category_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::put_many_change_reason_categories_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::put_many_change_reason_categories_request& v);
    static ores::dq::messaging::put_many_change_reason_categories_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_change_reason_categories_request& v);
    static ores::dq::messaging::put_many_change_reason_categories_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::put_many_change_reason_categories_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::put_many_change_reason_categories_response& v);
    static ores::dq::messaging::put_many_change_reason_categories_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::put_many_change_reason_categories_response& v);
    static ores::dq::messaging::put_many_change_reason_categories_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_change_reason_category_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_change_reason_category_request& v);
    static ores::dq::messaging::delete_change_reason_category_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_change_reason_category_request& v);
    static ores::dq::messaging::delete_change_reason_category_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_change_reason_category_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_change_reason_category_response& v);
    static ores::dq::messaging::delete_change_reason_category_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_change_reason_category_response& v);
    static ores::dq::messaging::delete_change_reason_category_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::delete_many_change_reason_categories_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_change_reason_categories_request& v);
    static ores::dq::messaging::delete_many_change_reason_categories_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_change_reason_categories_request& v);
    static ores::dq::messaging::delete_many_change_reason_categories_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::delete_many_change_reason_categories_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::delete_many_change_reason_categories_response& v);
    static ores::dq::messaging::delete_many_change_reason_categories_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_change_reason_categories_response& v);
    static ores::dq::messaging::delete_many_change_reason_categories_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_change_reason_category_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_change_reason_category_versions_request& v);
    static ores::dq::messaging::list_change_reason_category_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_change_reason_category_versions_request& v);
    static ores::dq::messaging::list_change_reason_category_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_change_reason_category_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_change_reason_category_versions_response& v);
    static ores::dq::messaging::list_change_reason_category_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_change_reason_category_versions_response& v);
    static ores::dq::messaging::list_change_reason_category_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::get_change_reason_category_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_change_reason_category_version_request& v);
    static ores::dq::messaging::get_change_reason_category_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_change_reason_category_version_request& v);
    static ores::dq::messaging::get_change_reason_category_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::get_change_reason_category_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_change_reason_category_version_response& v);
    static ores::dq::messaging::get_change_reason_category_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_change_reason_category_version_response& v);
    static ores::dq::messaging::get_change_reason_category_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
