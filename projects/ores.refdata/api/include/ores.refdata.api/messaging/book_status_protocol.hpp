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
#ifndef ORES_REFDATA_API_MESSAGING_BOOK_STATUS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BOOK_STATUS_PROTOCOL_HPP

#include "ores.refdata.api/domain/book_status.hpp"
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

struct book_status_key {
    std::string code;
};

struct book_status_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct book_status_change {
    book_status_write write;
    ores::utility::domain::precondition precondition;
};

struct book_status_removal {
    book_status_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct book_status_lookup {
    book_status_key key;
    std::optional<ores::refdata::domain::book_status> book_status;
};

struct book_statuses_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct book_status_event {
    boost::uuids::uuid event_id;
    book_status_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct book_status_version_key {
    book_status_key book_status;
    std::uint32_t version;
};

struct book_status_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_book_statuses_request {
    using response_type = struct list_book_statuses_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses.list";
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
    std::optional<book_statuses_filter> filter;
    std::optional<std::string> as_of;
};

struct list_book_statuses_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::book_status> statuses;
    std::uint64_t total;
};

struct get_book_status_request {
    using response_type = struct get_book_status_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_status_key key;
};

struct get_book_status_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::book_status> book_status;
};

struct get_many_book_statuses_request {
    using response_type = struct get_many_book_statuses_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<book_status_key> keys;
};

struct get_many_book_statuses_response {
    ores::utility::domain::result result;
    std::vector<book_status_lookup> entries;
};

struct put_book_status_request {
    using response_type = struct put_book_status_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_status_change change;
    ores::utility::domain::change_intent intent;
};

struct put_book_status_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::book_status> book_status;
};

struct put_many_book_statuses_request {
    using response_type = struct put_many_book_statuses_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<book_status_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_book_statuses_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::book_status> statuses;
};

struct delete_book_status_request {
    using response_type = struct delete_book_status_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_status_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_book_status_response {
    ores::utility::domain::result result;
};

struct delete_many_book_statuses_request {
    using response_type = struct delete_many_book_statuses_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<book_status_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_book_statuses_response {
    ores::utility::domain::result result;
};

struct list_book_status_versions_request {
    using response_type = struct list_book_status_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_status_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<book_status_versions_filter> filter;
};

struct list_book_status_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::book_status> versions;
    std::uint64_t total;
};

struct get_book_status_version_request {
    using response_type = struct get_book_status_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.book_statuses_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_status_version_key key;
};

struct get_book_status_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::book_status> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace book_status_event_subjects {
inline constexpr std::string_view created = "refdata.v1.book_statuses_events.created";
inline constexpr std::string_view updated = "refdata.v1.book_statuses_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.book_statuses_events.deleted";
}
/**
 * @brief The members of book_status_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_key&) {
    return std::tuple{
        utility::codec::field{"code", &book_status_key::code},
    };
}

/**
 * @brief The members of book_status_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_write&) {
    return std::tuple{
        utility::codec::field{"code", &book_status_write::code},
        utility::codec::field{"name", &book_status_write::name},
        utility::codec::field{"description", &book_status_write::description},
        utility::codec::field{"display_order", &book_status_write::display_order},
    };
}

/**
 * @brief The members of book_status_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_change&) {
    return std::tuple{
        utility::codec::field{"write", &book_status_change::write},
        utility::codec::field{"precondition", &book_status_change::precondition},
    };
}

/**
 * @brief The members of book_status_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_removal&) {
    return std::tuple{
        utility::codec::field{"key", &book_status_removal::key},
        utility::codec::field{"precondition", &book_status_removal::precondition},
    };
}

/**
 * @brief The members of book_status_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &book_status_lookup::key},
        utility::codec::field{"book_status", &book_status_lookup::book_status},
    };
}

/**
 * @brief The members of book_statuses_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_statuses_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &book_statuses_filter::code_one_of},
    };
}

/**
 * @brief The members of book_status_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &book_status_event::event_id},
        utility::codec::field{"key", &book_status_event::key},
        utility::codec::field{"action", &book_status_event::action},
        utility::codec::field{"version", &book_status_event::version},
        utility::codec::field{"occurred_at", &book_status_event::occurred_at},
        utility::codec::field{"correlation_id", &book_status_event::correlation_id},
    };
}

/**
 * @brief The members of book_status_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_version_key&) {
    return std::tuple{
        utility::codec::field{"book_status", &book_status_version_key::book_status},
        utility::codec::field{"version", &book_status_version_key::version},
    };
}

/**
 * @brief The members of book_status_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_status_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &book_status_versions_filter::version},
        utility::codec::field{"from_version", &book_status_versions_filter::from_version},
        utility::codec::field{"to_version", &book_status_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_book_statuses_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_book_statuses_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_book_statuses_request::offset},
        utility::codec::field{"limit", &list_book_statuses_request::limit},
        utility::codec::field{"order", &list_book_statuses_request::order},
        utility::codec::field{"filter", &list_book_statuses_request::filter},
        utility::codec::field{"as_of", &list_book_statuses_request::as_of},
    };
}

/**
 * @brief The members of list_book_statuses_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_book_statuses_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_book_statuses_response::result},
        utility::codec::field{"statuses", &list_book_statuses_response::statuses},
        utility::codec::field{"total", &list_book_statuses_response::total},
    };
}

/**
 * @brief The members of get_book_status_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_status_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_book_status_request::key},
    };
}

/**
 * @brief The members of get_book_status_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_status_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_book_status_response::result},
        utility::codec::field{"book_status", &get_book_status_response::book_status},
    };
}

/**
 * @brief The members of get_many_book_statuses_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_book_statuses_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_book_statuses_request::keys},
    };
}

/**
 * @brief The members of get_many_book_statuses_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_book_statuses_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_book_statuses_response::result},
        utility::codec::field{"entries", &get_many_book_statuses_response::entries},
    };
}

/**
 * @brief The members of put_book_status_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_book_status_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_book_status_request::change},
        utility::codec::field{"intent", &put_book_status_request::intent},
    };
}

/**
 * @brief The members of put_book_status_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_book_status_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_book_status_response::result},
        utility::codec::field{"book_status", &put_book_status_response::book_status},
    };
}

/**
 * @brief The members of put_many_book_statuses_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_book_statuses_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_book_statuses_request::changes},
        utility::codec::field{"intent", &put_many_book_statuses_request::intent},
    };
}

/**
 * @brief The members of put_many_book_statuses_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_book_statuses_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_book_statuses_response::result},
        utility::codec::field{"statuses", &put_many_book_statuses_response::statuses},
    };
}

/**
 * @brief The members of delete_book_status_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_book_status_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_book_status_request::removal},
        utility::codec::field{"intent", &delete_book_status_request::intent},
    };
}

/**
 * @brief The members of delete_book_status_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_book_status_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_book_status_response::result},
    };
}

/**
 * @brief The members of delete_many_book_statuses_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_book_statuses_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_book_statuses_request::removals},
        utility::codec::field{"intent", &delete_many_book_statuses_request::intent},
    };
}

/**
 * @brief The members of delete_many_book_statuses_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_book_statuses_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_book_statuses_response::result},
    };
}

/**
 * @brief The members of list_book_status_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_book_status_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_book_status_versions_request::key},
        utility::codec::field{"offset", &list_book_status_versions_request::offset},
        utility::codec::field{"limit", &list_book_status_versions_request::limit},
        utility::codec::field{"order", &list_book_status_versions_request::order},
        utility::codec::field{"filter", &list_book_status_versions_request::filter},
    };
}

/**
 * @brief The members of list_book_status_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_book_status_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_book_status_versions_response::result},
        utility::codec::field{"versions", &list_book_status_versions_response::versions},
        utility::codec::field{"total", &list_book_status_versions_response::total},
    };
}

/**
 * @brief The members of get_book_status_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_status_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_book_status_version_request::key},
    };
}

/**
 * @brief The members of get_book_status_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_status_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_book_status_version_response::result},
        utility::codec::field{"version", &get_book_status_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_key& v);
    static ores::refdata::messaging::book_status_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_status_key& v);
    static ores::refdata::messaging::book_status_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_write& v);
    static ores::refdata::messaging::book_status_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_status_write& v);
    static ores::refdata::messaging::book_status_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_change& v);
    static ores::refdata::messaging::book_status_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_status_change& v);
    static ores::refdata::messaging::book_status_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_removal& v);
    static ores::refdata::messaging::book_status_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_status_removal& v);
    static ores::refdata::messaging::book_status_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_lookup& v);
    static ores::refdata::messaging::book_status_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_status_lookup& v);
    static ores::refdata::messaging::book_status_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_statuses_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_statuses_filter& v);
    static ores::refdata::messaging::book_statuses_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_statuses_filter& v);
    static ores::refdata::messaging::book_statuses_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_event& v);
    static ores::refdata::messaging::book_status_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_status_event& v);
    static ores::refdata::messaging::book_status_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_version_key& v);
    static ores::refdata::messaging::book_status_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_status_version_key& v);
    static ores::refdata::messaging::book_status_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_status_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_status_versions_filter& v);
    static ores::refdata::messaging::book_status_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::book_status_versions_filter& v);
    static ores::refdata::messaging::book_status_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_book_statuses_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_book_statuses_request& v);
    static ores::refdata::messaging::list_book_statuses_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_book_statuses_request& v);
    static ores::refdata::messaging::list_book_statuses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_book_statuses_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_book_statuses_response& v);
    static ores::refdata::messaging::list_book_statuses_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_book_statuses_response& v);
    static ores::refdata::messaging::list_book_statuses_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_status_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_status_request& v);
    static ores::refdata::messaging::get_book_status_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_status_request& v);
    static ores::refdata::messaging::get_book_status_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_status_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_status_response& v);
    static ores::refdata::messaging::get_book_status_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_status_response& v);
    static ores::refdata::messaging::get_book_status_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_book_statuses_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_book_statuses_request& v);
    static ores::refdata::messaging::get_many_book_statuses_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_book_statuses_request& v);
    static ores::refdata::messaging::get_many_book_statuses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_book_statuses_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_book_statuses_response& v);
    static ores::refdata::messaging::get_many_book_statuses_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_book_statuses_response& v);
    static ores::refdata::messaging::get_many_book_statuses_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_book_status_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_book_status_request& v);
    static ores::refdata::messaging::put_book_status_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_book_status_request& v);
    static ores::refdata::messaging::put_book_status_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_book_status_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_book_status_response& v);
    static ores::refdata::messaging::put_book_status_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_book_status_response& v);
    static ores::refdata::messaging::put_book_status_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_book_statuses_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_book_statuses_request& v);
    static ores::refdata::messaging::put_many_book_statuses_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_book_statuses_request& v);
    static ores::refdata::messaging::put_many_book_statuses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_book_statuses_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_book_statuses_response& v);
    static ores::refdata::messaging::put_many_book_statuses_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_book_statuses_response& v);
    static ores::refdata::messaging::put_many_book_statuses_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_book_status_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_book_status_request& v);
    static ores::refdata::messaging::delete_book_status_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_book_status_request& v);
    static ores::refdata::messaging::delete_book_status_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_book_status_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_book_status_response& v);
    static ores::refdata::messaging::delete_book_status_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_book_status_response& v);
    static ores::refdata::messaging::delete_book_status_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_book_statuses_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_book_statuses_request& v);
    static ores::refdata::messaging::delete_many_book_statuses_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_book_statuses_request& v);
    static ores::refdata::messaging::delete_many_book_statuses_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_book_statuses_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_book_statuses_response& v);
    static ores::refdata::messaging::delete_many_book_statuses_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_book_statuses_response& v);
    static ores::refdata::messaging::delete_many_book_statuses_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_book_status_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_book_status_versions_request& v);
    static ores::refdata::messaging::list_book_status_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_book_status_versions_request& v);
    static ores::refdata::messaging::list_book_status_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_book_status_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_book_status_versions_response& v);
    static ores::refdata::messaging::list_book_status_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_book_status_versions_response& v);
    static ores::refdata::messaging::list_book_status_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_book_status_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_book_status_version_request& v);
    static ores::refdata::messaging::get_book_status_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_book_status_version_request& v);
    static ores::refdata::messaging::get_book_status_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_book_status_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_book_status_version_response& v);
    static ores::refdata::messaging::get_book_status_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_book_status_version_response& v);
    static ores::refdata::messaging::get_book_status_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
