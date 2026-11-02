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
#ifndef ORES_SYNTHETIC_API_MESSAGING_TRADE_GENERATION_BOOK_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_TRADE_GENERATION_BOOK_PROTOCOL_HPP

#include "ores.synthetic.api/domain/trade_generation_book.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct trade_generation_book_key {
    boost::uuids::uuid id;
};

struct trade_generation_book_write {
    boost::uuids::uuid id;
    boost::uuids::uuid config_id;
    boost::uuids::uuid book_id;
};

struct trade_generation_book_change {
    trade_generation_book_write write;
    ores::utility::domain::precondition precondition;
};

struct trade_generation_book_removal {
    trade_generation_book_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct trade_generation_book_lookup {
    trade_generation_book_key key;
    std::optional<ores::synthetic::domain::trade_generation_book> trade_generation_book;
};

struct trade_generation_books_filter {
    std::optional<boost::uuids::uuid> config_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> config_id_one_of;
};

struct trade_generation_book_event {
    boost::uuids::uuid event_id;
    trade_generation_book_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct trade_generation_book_version_key {
    trade_generation_book_key trade_generation_book;
    std::uint32_t version;
};

struct trade_generation_book_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_trade_generation_books_request {
    using response_type = struct list_trade_generation_books_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.trade_generation_books.list";
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
    std::optional<trade_generation_books_filter> filter;
    std::optional<std::string> as_of;
};

struct list_trade_generation_books_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::trade_generation_book> trade_generation_books;
    std::uint64_t total;
};

struct get_trade_generation_book_request {
    using response_type = struct get_trade_generation_book_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.trade_generation_books.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_generation_book_key key;
};

struct get_trade_generation_book_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::trade_generation_book> trade_generation_book;
};

struct get_many_trade_generation_books_request {
    using response_type = struct get_many_trade_generation_books_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.trade_generation_books.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_generation_book_key> keys;
};

struct get_many_trade_generation_books_response {
    ores::utility::domain::result result;
    std::vector<trade_generation_book_lookup> entries;
};

struct put_trade_generation_book_request {
    using response_type = struct put_trade_generation_book_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.trade_generation_books.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_generation_book_change change;
    ores::utility::domain::change_intent intent;
};

struct put_trade_generation_book_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::trade_generation_book> trade_generation_book;
};

struct put_many_trade_generation_books_request {
    using response_type = struct put_many_trade_generation_books_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.trade_generation_books.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_generation_book_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_trade_generation_books_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::trade_generation_book> trade_generation_books;
};

struct delete_trade_generation_book_request {
    using response_type = struct delete_trade_generation_book_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.trade_generation_books.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_generation_book_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_trade_generation_book_response {
    ores::utility::domain::result result;
};

struct delete_many_trade_generation_books_request {
    using response_type = struct delete_many_trade_generation_books_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.trade_generation_books.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<trade_generation_book_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_trade_generation_books_response {
    ores::utility::domain::result result;
};

struct list_trade_generation_book_versions_request {
    using response_type = struct list_trade_generation_book_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.trade_generation_books_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_generation_book_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<trade_generation_book_versions_filter> filter;
};

struct list_trade_generation_book_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::trade_generation_book> versions;
    std::uint64_t total;
};

struct get_trade_generation_book_version_request {
    using response_type = struct get_trade_generation_book_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.trade_generation_books_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    trade_generation_book_version_key key;
};

struct get_trade_generation_book_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::trade_generation_book> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace trade_generation_book_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.trade_generation_books_events.created";
inline constexpr std::string_view updated = "synthetic.v1.trade_generation_books_events.updated";
inline constexpr std::string_view deleted = "synthetic.v1.trade_generation_books_events.deleted";
}
/**
 * @brief The members of trade_generation_book_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_key&) {
    return std::tuple{
        utility::codec::field{"id", &trade_generation_book_key::id},
    };
}

/**
 * @brief The members of trade_generation_book_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_write&) {
    return std::tuple{
        utility::codec::field{"id", &trade_generation_book_write::id},
        utility::codec::field{"config_id", &trade_generation_book_write::config_id},
        utility::codec::field{"book_id", &trade_generation_book_write::book_id},
    };
}

/**
 * @brief The members of trade_generation_book_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_change&) {
    return std::tuple{
        utility::codec::field{"write", &trade_generation_book_change::write},
        utility::codec::field{"precondition", &trade_generation_book_change::precondition},
    };
}

/**
 * @brief The members of trade_generation_book_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_removal&) {
    return std::tuple{
        utility::codec::field{"key", &trade_generation_book_removal::key},
        utility::codec::field{"precondition", &trade_generation_book_removal::precondition},
    };
}

/**
 * @brief The members of trade_generation_book_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &trade_generation_book_lookup::key},
        utility::codec::field{"trade_generation_book",
                              &trade_generation_book_lookup::trade_generation_book},
    };
}

/**
 * @brief The members of trade_generation_books_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_books_filter&) {
    return std::tuple{
        utility::codec::field{"config_id", &trade_generation_books_filter::config_id},
        utility::codec::field{"id_one_of", &trade_generation_books_filter::id_one_of},
        utility::codec::field{"config_id_one_of", &trade_generation_books_filter::config_id_one_of},
    };
}

/**
 * @brief The members of trade_generation_book_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &trade_generation_book_event::event_id},
        utility::codec::field{"key", &trade_generation_book_event::key},
        utility::codec::field{"action", &trade_generation_book_event::action},
        utility::codec::field{"version", &trade_generation_book_event::version},
        utility::codec::field{"occurred_at", &trade_generation_book_event::occurred_at},
        utility::codec::field{"correlation_id", &trade_generation_book_event::correlation_id},
    };
}

/**
 * @brief The members of trade_generation_book_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_version_key&) {
    return std::tuple{
        utility::codec::field{"trade_generation_book",
                              &trade_generation_book_version_key::trade_generation_book},
        utility::codec::field{"version", &trade_generation_book_version_key::version},
    };
}

/**
 * @brief The members of trade_generation_book_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const trade_generation_book_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &trade_generation_book_versions_filter::version},
        utility::codec::field{"from_version", &trade_generation_book_versions_filter::from_version},
        utility::codec::field{"to_version", &trade_generation_book_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_trade_generation_books_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_generation_books_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_trade_generation_books_request::offset},
        utility::codec::field{"limit", &list_trade_generation_books_request::limit},
        utility::codec::field{"order", &list_trade_generation_books_request::order},
        utility::codec::field{"filter", &list_trade_generation_books_request::filter},
        utility::codec::field{"as_of", &list_trade_generation_books_request::as_of},
    };
}

/**
 * @brief The members of list_trade_generation_books_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_generation_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_generation_books_response::result},
        utility::codec::field{"trade_generation_books",
                              &list_trade_generation_books_response::trade_generation_books},
        utility::codec::field{"total", &list_trade_generation_books_response::total},
    };
}

/**
 * @brief The members of get_trade_generation_book_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_generation_book_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_generation_book_request::key},
    };
}

/**
 * @brief The members of get_trade_generation_book_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_generation_book_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_generation_book_response::result},
        utility::codec::field{"trade_generation_book",
                              &get_trade_generation_book_response::trade_generation_book},
    };
}

/**
 * @brief The members of get_many_trade_generation_books_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_generation_books_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_trade_generation_books_request::keys},
    };
}

/**
 * @brief The members of get_many_trade_generation_books_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_trade_generation_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_trade_generation_books_response::result},
        utility::codec::field{"entries", &get_many_trade_generation_books_response::entries},
    };
}

/**
 * @brief The members of put_trade_generation_book_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_generation_book_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_trade_generation_book_request::change},
        utility::codec::field{"intent", &put_trade_generation_book_request::intent},
    };
}

/**
 * @brief The members of put_trade_generation_book_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_trade_generation_book_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_trade_generation_book_response::result},
        utility::codec::field{"trade_generation_book",
                              &put_trade_generation_book_response::trade_generation_book},
    };
}

/**
 * @brief The members of put_many_trade_generation_books_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_generation_books_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_trade_generation_books_request::changes},
        utility::codec::field{"intent", &put_many_trade_generation_books_request::intent},
    };
}

/**
 * @brief The members of put_many_trade_generation_books_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_trade_generation_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_trade_generation_books_response::result},
        utility::codec::field{"trade_generation_books",
                              &put_many_trade_generation_books_response::trade_generation_books},
    };
}

/**
 * @brief The members of delete_trade_generation_book_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_generation_book_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_trade_generation_book_request::removal},
        utility::codec::field{"intent", &delete_trade_generation_book_request::intent},
    };
}

/**
 * @brief The members of delete_trade_generation_book_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_trade_generation_book_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_trade_generation_book_response::result},
    };
}

/**
 * @brief The members of delete_many_trade_generation_books_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_generation_books_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_trade_generation_books_request::removals},
        utility::codec::field{"intent", &delete_many_trade_generation_books_request::intent},
    };
}

/**
 * @brief The members of delete_many_trade_generation_books_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_trade_generation_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_trade_generation_books_response::result},
    };
}

/**
 * @brief The members of list_trade_generation_book_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_generation_book_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_trade_generation_book_versions_request::key},
        utility::codec::field{"offset", &list_trade_generation_book_versions_request::offset},
        utility::codec::field{"limit", &list_trade_generation_book_versions_request::limit},
        utility::codec::field{"order", &list_trade_generation_book_versions_request::order},
        utility::codec::field{"filter", &list_trade_generation_book_versions_request::filter},
    };
}

/**
 * @brief The members of list_trade_generation_book_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_trade_generation_book_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_trade_generation_book_versions_response::result},
        utility::codec::field{"versions", &list_trade_generation_book_versions_response::versions},
        utility::codec::field{"total", &list_trade_generation_book_versions_response::total},
    };
}

/**
 * @brief The members of get_trade_generation_book_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_generation_book_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_trade_generation_book_version_request::key},
    };
}

/**
 * @brief The members of get_trade_generation_book_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_trade_generation_book_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_trade_generation_book_version_response::result},
        utility::codec::field{"version", &get_trade_generation_book_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::trade_generation_book_key& v);
    static ores::synthetic::messaging::trade_generation_book_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_key& v);
    static ores::synthetic::messaging::trade_generation_book_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::trade_generation_book_write& v);
    static ores::synthetic::messaging::trade_generation_book_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_write& v);
    static ores::synthetic::messaging::trade_generation_book_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::trade_generation_book_change& v);
    static ores::synthetic::messaging::trade_generation_book_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_change& v);
    static ores::synthetic::messaging::trade_generation_book_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::trade_generation_book_removal& v);
    static ores::synthetic::messaging::trade_generation_book_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_removal& v);
    static ores::synthetic::messaging::trade_generation_book_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::trade_generation_book_lookup& v);
    static ores::synthetic::messaging::trade_generation_book_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_lookup& v);
    static ores::synthetic::messaging::trade_generation_book_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_books_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::trade_generation_books_filter& v);
    static ores::synthetic::messaging::trade_generation_books_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_books_filter& v);
    static ores::synthetic::messaging::trade_generation_books_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::trade_generation_book_event& v);
    static ores::synthetic::messaging::trade_generation_book_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_event& v);
    static ores::synthetic::messaging::trade_generation_book_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::trade_generation_book_version_key& v);
    static ores::synthetic::messaging::trade_generation_book_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_version_key& v);
    static ores::synthetic::messaging::trade_generation_book_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::trade_generation_book_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::trade_generation_book_versions_filter& v);
    static ores::synthetic::messaging::trade_generation_book_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::trade_generation_book_versions_filter& v);
    static ores::synthetic::messaging::trade_generation_book_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_trade_generation_books_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_trade_generation_books_request& v);
    static ores::synthetic::messaging::list_trade_generation_books_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_trade_generation_books_request& v);
    static ores::synthetic::messaging::list_trade_generation_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_trade_generation_books_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_trade_generation_books_response& v);
    static ores::synthetic::messaging::list_trade_generation_books_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_trade_generation_books_response& v);
    static ores::synthetic::messaging::list_trade_generation_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_trade_generation_book_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_trade_generation_book_request& v);
    static ores::synthetic::messaging::get_trade_generation_book_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_trade_generation_book_request& v);
    static ores::synthetic::messaging::get_trade_generation_book_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_trade_generation_book_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_trade_generation_book_response& v);
    static ores::synthetic::messaging::get_trade_generation_book_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_trade_generation_book_response& v);
    static ores::synthetic::messaging::get_trade_generation_book_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_trade_generation_books_request& v);
    static ores::synthetic::messaging::get_many_trade_generation_books_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_trade_generation_books_request& v);
    static ores::synthetic::messaging::get_many_trade_generation_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_trade_generation_books_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_trade_generation_books_response& v);
    static ores::synthetic::messaging::get_many_trade_generation_books_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_trade_generation_books_response& v);
    static ores::synthetic::messaging::get_many_trade_generation_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_trade_generation_book_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_trade_generation_book_request& v);
    static ores::synthetic::messaging::put_trade_generation_book_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_trade_generation_book_request& v);
    static ores::synthetic::messaging::put_trade_generation_book_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_trade_generation_book_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_trade_generation_book_response& v);
    static ores::synthetic::messaging::put_trade_generation_book_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_trade_generation_book_response& v);
    static ores::synthetic::messaging::put_trade_generation_book_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_trade_generation_books_request& v);
    static ores::synthetic::messaging::put_many_trade_generation_books_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_trade_generation_books_request& v);
    static ores::synthetic::messaging::put_many_trade_generation_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_trade_generation_books_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_trade_generation_books_response& v);
    static ores::synthetic::messaging::put_many_trade_generation_books_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_trade_generation_books_response& v);
    static ores::synthetic::messaging::put_many_trade_generation_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_trade_generation_book_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_trade_generation_book_request& v);
    static ores::synthetic::messaging::delete_trade_generation_book_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_trade_generation_book_request& v);
    static ores::synthetic::messaging::delete_trade_generation_book_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_trade_generation_book_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_trade_generation_book_response& v);
    static ores::synthetic::messaging::delete_trade_generation_book_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_trade_generation_book_response& v);
    static ores::synthetic::messaging::delete_trade_generation_book_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_trade_generation_books_request& v);
    static ores::synthetic::messaging::delete_many_trade_generation_books_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_trade_generation_books_request& v);
    static ores::synthetic::messaging::delete_many_trade_generation_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_trade_generation_books_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_trade_generation_books_response& v);
    static ores::synthetic::messaging::delete_many_trade_generation_books_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_trade_generation_books_response& v);
    static ores::synthetic::messaging::delete_many_trade_generation_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_trade_generation_book_versions_request& v);
    static ores::synthetic::messaging::list_trade_generation_book_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_trade_generation_book_versions_request& v);
    static ores::synthetic::messaging::list_trade_generation_book_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_trade_generation_book_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_trade_generation_book_versions_response& v);
    static ores::synthetic::messaging::list_trade_generation_book_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_trade_generation_book_versions_response& v);
    static ores::synthetic::messaging::list_trade_generation_book_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_trade_generation_book_version_request& v);
    static ores::synthetic::messaging::get_trade_generation_book_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_trade_generation_book_version_request& v);
    static ores::synthetic::messaging::get_trade_generation_book_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_trade_generation_book_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_trade_generation_book_version_response& v);
    static ores::synthetic::messaging::get_trade_generation_book_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_trade_generation_book_version_response& v);
    static ores::synthetic::messaging::get_trade_generation_book_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
