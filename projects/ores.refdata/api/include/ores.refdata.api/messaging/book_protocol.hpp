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
#ifndef ORES_REFDATA_API_MESSAGING_BOOK_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BOOK_PROTOCOL_HPP

#include "ores.refdata.api/domain/book.hpp"
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

struct book_key {
    std::string name;
};

struct book_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    std::string name;
    std::string description;
    boost::uuids::uuid parent_portfolio_id;
    std::optional<boost::uuids::uuid> owner_unit_id;
    std::string functional_currency;
    std::string gl_account_ref;
    std::string cost_center;
    std::string book_status;
    std::string regulatory_book_type;
    std::string book_purpose_type;
    std::string ledger_feed_type;
    bool is_sweepable;
    std::string rates_centre_code;
    std::optional<boost::uuids::uuid> sandbox_id;
};

struct book_change {
    book_write write;
    ores::utility::domain::precondition precondition;
};

struct book_removal {
    book_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct book_lookup {
    book_key key;
    std::optional<ores::refdata::domain::book> book;
};

struct books_filter {
    std::optional<boost::uuids::uuid> parent_portfolio_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> parent_portfolio_id_one_of;
};

struct book_event {
    boost::uuids::uuid event_id;
    book_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct book_version_key {
    book_key book;
    std::uint32_t version;
};

struct book_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_books_request {
    using response_type = struct list_books_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.list";
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
    std::optional<books_filter> filter;
    std::optional<std::string> as_of;
};

struct list_books_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::book> books;
    std::uint64_t total;
};

struct get_book_request {
    using response_type = struct get_book_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_key key;
};

struct get_book_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::book> book;
};

struct get_many_books_request {
    using response_type = struct get_many_books_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<book_key> keys;
};

struct get_many_books_response {
    ores::utility::domain::result result;
    std::vector<book_lookup> entries;
};

struct put_book_request {
    using response_type = struct put_book_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_change change;
    ores::utility::domain::change_intent intent;
};

struct put_book_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::book> book;
};

struct put_many_books_request {
    using response_type = struct put_many_books_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<book_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_books_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::book> books;
};

struct delete_book_request {
    using response_type = struct delete_book_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_book_response {
    ores::utility::domain::result result;
};

struct delete_many_books_request {
    using response_type = struct delete_many_books_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<book_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_books_response {
    ores::utility::domain::result result;
};

struct list_by_parent_portfolio_id_books_request {
    using response_type = struct list_by_parent_portfolio_id_books_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books.list_by_parent_portfolio_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid parent_portfolio_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<books_filter> filter;
};

struct list_by_parent_portfolio_id_books_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::book> books;
    std::uint64_t total;
};

struct list_book_versions_request {
    using response_type = struct list_book_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<book_versions_filter> filter;
};

struct list_book_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::book> versions;
    std::uint64_t total;
};

struct get_book_version_request {
    using response_type = struct get_book_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.books_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    book_version_key key;
};

struct get_book_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::book> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace book_event_subjects {
inline constexpr std::string_view created = "refdata.v1.books_events.created";
inline constexpr std::string_view updated = "refdata.v1.books_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.books_events.deleted";
}
/**
 * @brief The members of book_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_key&) {
    return std::tuple{
        utility::codec::field{"name", &book_key::name},
    };
}

/**
 * @brief The members of book_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_write&) {
    return std::tuple{
        utility::codec::field{"id", &book_write::id},
        utility::codec::field{"party_id", &book_write::party_id},
        utility::codec::field{"name", &book_write::name},
        utility::codec::field{"description", &book_write::description},
        utility::codec::field{"parent_portfolio_id", &book_write::parent_portfolio_id},
        utility::codec::field{"owner_unit_id", &book_write::owner_unit_id},
        utility::codec::field{"functional_currency", &book_write::functional_currency},
        utility::codec::field{"gl_account_ref", &book_write::gl_account_ref},
        utility::codec::field{"cost_center", &book_write::cost_center},
        utility::codec::field{"book_status", &book_write::book_status},
        utility::codec::field{"regulatory_book_type", &book_write::regulatory_book_type},
        utility::codec::field{"book_purpose_type", &book_write::book_purpose_type},
        utility::codec::field{"ledger_feed_type", &book_write::ledger_feed_type},
        utility::codec::field{"is_sweepable", &book_write::is_sweepable},
        utility::codec::field{"rates_centre_code", &book_write::rates_centre_code},
        utility::codec::field{"sandbox_id", &book_write::sandbox_id},
    };
}

/**
 * @brief The members of book_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_change&) {
    return std::tuple{
        utility::codec::field{"write", &book_change::write},
        utility::codec::field{"precondition", &book_change::precondition},
    };
}

/**
 * @brief The members of book_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_removal&) {
    return std::tuple{
        utility::codec::field{"key", &book_removal::key},
        utility::codec::field{"precondition", &book_removal::precondition},
    };
}

/**
 * @brief The members of book_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &book_lookup::key},
        utility::codec::field{"book", &book_lookup::book},
    };
}

/**
 * @brief The members of books_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const books_filter&) {
    return std::tuple{
        utility::codec::field{"parent_portfolio_id", &books_filter::parent_portfolio_id},
        utility::codec::field{"id_one_of", &books_filter::id_one_of},
        utility::codec::field{"parent_portfolio_id_one_of",
                              &books_filter::parent_portfolio_id_one_of},
    };
}

/**
 * @brief The members of book_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &book_event::event_id},
        utility::codec::field{"key", &book_event::key},
        utility::codec::field{"action", &book_event::action},
        utility::codec::field{"version", &book_event::version},
        utility::codec::field{"occurred_at", &book_event::occurred_at},
        utility::codec::field{"correlation_id", &book_event::correlation_id},
    };
}

/**
 * @brief The members of book_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_version_key&) {
    return std::tuple{
        utility::codec::field{"book", &book_version_key::book},
        utility::codec::field{"version", &book_version_key::version},
    };
}

/**
 * @brief The members of book_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const book_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &book_versions_filter::version},
        utility::codec::field{"from_version", &book_versions_filter::from_version},
        utility::codec::field{"to_version", &book_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_books_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_books_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_books_request::offset},
        utility::codec::field{"limit", &list_books_request::limit},
        utility::codec::field{"order", &list_books_request::order},
        utility::codec::field{"filter", &list_books_request::filter},
        utility::codec::field{"as_of", &list_books_request::as_of},
    };
}

/**
 * @brief The members of list_books_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_books_response::result},
        utility::codec::field{"books", &list_books_response::books},
        utility::codec::field{"total", &list_books_response::total},
    };
}

/**
 * @brief The members of get_book_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_book_request::key},
    };
}

/**
 * @brief The members of get_book_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_book_response::result},
        utility::codec::field{"book", &get_book_response::book},
    };
}

/**
 * @brief The members of get_many_books_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_books_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_books_request::keys},
    };
}

/**
 * @brief The members of get_many_books_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_books_response::result},
        utility::codec::field{"entries", &get_many_books_response::entries},
    };
}

/**
 * @brief The members of put_book_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_book_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_book_request::change},
        utility::codec::field{"intent", &put_book_request::intent},
    };
}

/**
 * @brief The members of put_book_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_book_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_book_response::result},
        utility::codec::field{"book", &put_book_response::book},
    };
}

/**
 * @brief The members of put_many_books_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_books_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_books_request::changes},
        utility::codec::field{"intent", &put_many_books_request::intent},
    };
}

/**
 * @brief The members of put_many_books_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_books_response::result},
        utility::codec::field{"books", &put_many_books_response::books},
    };
}

/**
 * @brief The members of delete_book_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_book_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_book_request::removal},
        utility::codec::field{"intent", &delete_book_request::intent},
    };
}

/**
 * @brief The members of delete_book_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_book_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_book_response::result},
    };
}

/**
 * @brief The members of delete_many_books_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_books_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_books_request::removals},
        utility::codec::field{"intent", &delete_many_books_request::intent},
    };
}

/**
 * @brief The members of delete_many_books_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_books_response::result},
    };
}

/**
 * @brief The members of list_by_parent_portfolio_id_books_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_parent_portfolio_id_books_request&) {
    return std::tuple{
        utility::codec::field{"parent_portfolio_id",
                              &list_by_parent_portfolio_id_books_request::parent_portfolio_id},
        utility::codec::field{"scope", &list_by_parent_portfolio_id_books_request::scope},
        utility::codec::field{"offset", &list_by_parent_portfolio_id_books_request::offset},
        utility::codec::field{"limit", &list_by_parent_portfolio_id_books_request::limit},
        utility::codec::field{"order", &list_by_parent_portfolio_id_books_request::order},
        utility::codec::field{"filter", &list_by_parent_portfolio_id_books_request::filter},
    };
}

/**
 * @brief The members of list_by_parent_portfolio_id_books_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_parent_portfolio_id_books_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_parent_portfolio_id_books_response::result},
        utility::codec::field{"books", &list_by_parent_portfolio_id_books_response::books},
        utility::codec::field{"total", &list_by_parent_portfolio_id_books_response::total},
    };
}

/**
 * @brief The members of list_book_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_book_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_book_versions_request::key},
        utility::codec::field{"offset", &list_book_versions_request::offset},
        utility::codec::field{"limit", &list_book_versions_request::limit},
        utility::codec::field{"order", &list_book_versions_request::order},
        utility::codec::field{"filter", &list_book_versions_request::filter},
    };
}

/**
 * @brief The members of list_book_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_book_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_book_versions_response::result},
        utility::codec::field{"versions", &list_book_versions_response::versions},
        utility::codec::field{"total", &list_book_versions_response::total},
    };
}

/**
 * @brief The members of get_book_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_book_version_request::key},
    };
}

/**
 * @brief The members of get_book_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_book_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_book_version_response::result},
        utility::codec::field{"version", &get_book_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_key& v);
    static ores::refdata::messaging::book_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_key& v);
    static ores::refdata::messaging::book_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_write& v);
    static ores::refdata::messaging::book_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_write& v);
    static ores::refdata::messaging::book_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_change& v);
    static ores::refdata::messaging::book_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_change& v);
    static ores::refdata::messaging::book_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_removal& v);
    static ores::refdata::messaging::book_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_removal& v);
    static ores::refdata::messaging::book_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_lookup& v);
    static ores::refdata::messaging::book_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_lookup& v);
    static ores::refdata::messaging::book_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::books_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::books_filter& v);
    static ores::refdata::messaging::books_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::books_filter& v);
    static ores::refdata::messaging::books_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_event& v);
    static ores::refdata::messaging::book_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_event& v);
    static ores::refdata::messaging::book_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_version_key& v);
    static ores::refdata::messaging::book_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_version_key& v);
    static ores::refdata::messaging::book_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_versions_filter& v);
    static ores::refdata::messaging::book_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_versions_filter& v);
    static ores::refdata::messaging::book_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_books_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_books_request& v);
    static ores::refdata::messaging::list_books_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_books_request& v);
    static ores::refdata::messaging::list_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_books_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_books_response& v);
    static ores::refdata::messaging::list_books_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_books_response& v);
    static ores::refdata::messaging::list_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_request& v);
    static ores::refdata::messaging::get_book_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_request& v);
    static ores::refdata::messaging::get_book_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_response& v);
    static ores::refdata::messaging::get_book_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_response& v);
    static ores::refdata::messaging::get_book_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_books_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_books_request& v);
    static ores::refdata::messaging::get_many_books_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_books_request& v);
    static ores::refdata::messaging::get_many_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_many_books_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_many_books_response& v);
    static ores::refdata::messaging::get_many_books_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_many_books_response& v);
    static ores::refdata::messaging::get_many_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_book_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_book_request& v);
    static ores::refdata::messaging::put_book_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_book_request& v);
    static ores::refdata::messaging::put_book_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_book_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_book_response& v);
    static ores::refdata::messaging::put_book_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_book_response& v);
    static ores::refdata::messaging::put_book_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_books_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_books_request& v);
    static ores::refdata::messaging::put_many_books_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_books_request& v);
    static ores::refdata::messaging::put_many_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_many_books_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_many_books_response& v);
    static ores::refdata::messaging::put_many_books_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_many_books_response& v);
    static ores::refdata::messaging::put_many_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_book_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_book_request& v);
    static ores::refdata::messaging::delete_book_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_book_request& v);
    static ores::refdata::messaging::delete_book_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_book_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_book_response& v);
    static ores::refdata::messaging::delete_book_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_book_response& v);
    static ores::refdata::messaging::delete_book_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_many_books_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_books_request& v);
    static ores::refdata::messaging::delete_many_books_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_many_books_request& v);
    static ores::refdata::messaging::delete_many_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_many_books_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_many_books_response& v);
    static ores::refdata::messaging::delete_many_books_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::delete_many_books_response& v);
    static ores::refdata::messaging::delete_many_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_parent_portfolio_id_books_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_parent_portfolio_id_books_request& v);
    static ores::refdata::messaging::list_by_parent_portfolio_id_books_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_parent_portfolio_id_books_request& v);
    static ores::refdata::messaging::list_by_parent_portfolio_id_books_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_parent_portfolio_id_books_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_parent_portfolio_id_books_response& v);
    static ores::refdata::messaging::list_by_parent_portfolio_id_books_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_parent_portfolio_id_books_response& v);
    static ores::refdata::messaging::list_by_parent_portfolio_id_books_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_book_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_book_versions_request& v);
    static ores::refdata::messaging::list_book_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_book_versions_request& v);
    static ores::refdata::messaging::list_book_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_book_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_book_versions_response& v);
    static ores::refdata::messaging::list_book_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_book_versions_response& v);
    static ores::refdata::messaging::list_book_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_version_request& v);
    static ores::refdata::messaging::get_book_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_version_request& v);
    static ores::refdata::messaging::get_book_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_version_response& v);
    static ores::refdata::messaging::get_book_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_version_response& v);
    static ores::refdata::messaging::get_book_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
