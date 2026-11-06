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
#ifndef ORES_REFDATA_API_MESSAGING_BOOK_PROPOSAL_OPERATIONS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BOOK_PROPOSAL_OPERATIONS_PROTOCOL_HPP

#include "ores.refdata.api/domain/book_change.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief What the real write said about one proposed line.
 *
 * The refusal is empty when the write would stand. The columns are the ones
 * the line changes, which is what the policy reads to name the parts.
 */
struct book_line_outcome {
    int line_no;
    std::string operation;
    boost::uuids::uuid entity_id;
    std::vector<std::string> columns;
    std::string refusal;
    /**
     * @brief The parts the policy names for this line alone, which decide it. The
     * request needs the union over its lines.
     */
    std::vector<std::string> part_codes;

    friend bool operator==(const book_line_outcome&, const book_line_outcome&) = default;
};

/**
 * @brief Runs each proposed line through the real write and writes nothing.
 *
 * A put line needs the permission to write books, a delete line the permission
 * to delete them. The lines are checked in order inside one transaction that is
 * never committed.
 */
struct preview_book_changes_request {
    using response_type = struct preview_book_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.preview_book_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The proposed writes. The request, line number and part of a line are
     * not read: the server numbers the lines and the policy names the parts.
     */
    std::vector<ores::refdata::domain::book_change> lines;

    friend bool operator==(const preview_book_changes_request&,
                           const preview_book_changes_request&) = default;
};

struct preview_book_changes_response {
    ores::utility::domain::result result;
    std::vector<book_line_outcome> lines;
    /**
     * @brief The parts the policy needs for the lines together. Empty when no
     * policy row gates them.
     */
    std::vector<std::string> part_codes;

    friend bool operator==(const preview_book_changes_response&,
                           const preview_book_changes_response&) = default;
};

/**
 * @brief Holds the proposed lines in an approval request.
 *
 * Previews first. A refused line, or lines no policy row gates, raise nothing.
 */
struct raise_book_changes_request {
    using response_type = struct raise_book_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.raise_book_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Why the person asks, in their words.
     */
    std::string reason;
    std::vector<ores::refdata::domain::book_change> lines;

    friend bool operator==(const raise_book_changes_request&,
                           const raise_book_changes_request&) = default;
};

struct raise_book_changes_response {
    ores::utility::domain::result result;
    /**
     * @brief The id of the request as raised, when the outcome is ok. The inbox
     * operations read the request itself.
     */
    std::optional<boost::uuids::uuid> request_id;
    std::vector<book_line_outcome> lines;
    std::vector<std::string> part_codes;

    friend bool operator==(const raise_book_changes_response&,
                           const raise_book_changes_response&) = default;
};

/**
 * @brief One column of a proposed line beside the live book's value.
 *
 * The values are text whatever the column's type, because a screen draws them
 * and does not compute with them. The before is empty when the line creates
 * the book.
 */
struct book_field_delta {
    std::string name;
    std::string before;
    std::string after;
    bool changed = false;

    friend bool operator==(const book_field_delta&, const book_field_delta&) = default;
};

/**
 * @brief One line of a request as the decider reads it: the operation, the book
 * it names, and each column beside the live value.
 */
struct book_change_delta {
    int line_no = 0;
    std::string operation;
    boost::uuids::uuid entity_id;
    int base_version = 0;
    bool applied = false;
    std::vector<book_field_delta> fields;

    friend bool operator==(const book_change_delta&, const book_change_delta&) = default;
};

/**
 * @brief Reads the lines a request holds, each beside the live book.
 *
 * Answered to the person who raised the request, to whoever may decide its
 * kind, and to whoever may read the tenant's requests. Answered as not found
 * otherwise, as opening the request is.
 */
struct get_book_changes_request {
    using response_type = struct get_book_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.get_book_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;

    friend bool operator==(const get_book_changes_request&,
                           const get_book_changes_request&) = default;
};

struct get_book_changes_response {
    ores::utility::domain::result result;
    /**
     * @brief The lines in line order.
     */
    std::vector<book_change_delta> changes;

    friend bool operator==(const get_book_changes_response&,
                           const get_book_changes_response&) = default;
};

/**
 * @brief Runs the lines a request holds through the real write again and
 * writes nothing.
 *
 * The live book may have moved since the request was raised, so the checks a
 * decider reads are run now. Answered to the same callers as the lines are.
 */
struct recheck_book_changes_request {
    using response_type = struct recheck_book_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.recheck_book_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;

    friend bool operator==(const recheck_book_changes_request&,
                           const recheck_book_changes_request&) = default;
};

struct recheck_book_changes_response {
    ores::utility::domain::result result;
    std::vector<book_line_outcome> lines;
    std::vector<std::string> part_codes;

    friend bool operator==(const recheck_book_changes_response&,
                           const recheck_book_changes_response&) = default;
};

/**
 * @brief The members of book_line_outcome, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<book_line_outcome>) {
    return std::tuple{
        ::ores::utility::codec::field{"line_no", &book_line_outcome::line_no},
        ::ores::utility::codec::field{"operation", &book_line_outcome::operation},
        ::ores::utility::codec::field{"entity_id", &book_line_outcome::entity_id},
        ::ores::utility::codec::field{"columns", &book_line_outcome::columns},
        ::ores::utility::codec::field{"refusal", &book_line_outcome::refusal},
        ::ores::utility::codec::field{"part_codes", &book_line_outcome::part_codes},
    };
}

/**
 * @brief The members of preview_book_changes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_book_changes_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"lines", &preview_book_changes_request::lines},
    };
}

/**
 * @brief The members of preview_book_changes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_book_changes_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &preview_book_changes_response::result},
        ::ores::utility::codec::field{"lines", &preview_book_changes_response::lines},
        ::ores::utility::codec::field{"part_codes", &preview_book_changes_response::part_codes},
    };
}

/**
 * @brief The members of raise_book_changes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_book_changes_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"reason", &raise_book_changes_request::reason},
        ::ores::utility::codec::field{"lines", &raise_book_changes_request::lines},
    };
}

/**
 * @brief The members of raise_book_changes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_book_changes_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &raise_book_changes_response::result},
        ::ores::utility::codec::field{"request_id", &raise_book_changes_response::request_id},
        ::ores::utility::codec::field{"lines", &raise_book_changes_response::lines},
        ::ores::utility::codec::field{"part_codes", &raise_book_changes_response::part_codes},
    };
}

/**
 * @brief The members of book_field_delta, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<book_field_delta>) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &book_field_delta::name},
        ::ores::utility::codec::field{"before", &book_field_delta::before},
        ::ores::utility::codec::field{"after", &book_field_delta::after},
        ::ores::utility::codec::field{"changed", &book_field_delta::changed},
    };
}

/**
 * @brief The members of book_change_delta, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<book_change_delta>) {
    return std::tuple{
        ::ores::utility::codec::field{"line_no", &book_change_delta::line_no},
        ::ores::utility::codec::field{"operation", &book_change_delta::operation},
        ::ores::utility::codec::field{"entity_id", &book_change_delta::entity_id},
        ::ores::utility::codec::field{"base_version", &book_change_delta::base_version},
        ::ores::utility::codec::field{"applied", &book_change_delta::applied},
        ::ores::utility::codec::field{"fields", &book_change_delta::fields},
    };
}

/**
 * @brief The members of get_book_changes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_book_changes_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &get_book_changes_request::request_id},
    };
}

/**
 * @brief The members of get_book_changes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_book_changes_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_book_changes_response::result},
        ::ores::utility::codec::field{"changes", &get_book_changes_response::changes},
    };
}

/**
 * @brief The members of recheck_book_changes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<recheck_book_changes_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &recheck_book_changes_request::request_id},
    };
}

/**
 * @brief The members of recheck_book_changes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<recheck_book_changes_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &recheck_book_changes_response::result},
        ::ores::utility::codec::field{"lines", &recheck_book_changes_response::lines},
        ::ores::utility::codec::field{"part_codes", &recheck_book_changes_response::part_codes},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_line_outcome> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_line_outcome& v);
    static ores::refdata::messaging::book_line_outcome read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_line_outcome& v);
    static ores::refdata::messaging::book_line_outcome
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::preview_book_changes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::preview_book_changes_request& v);
    static ores::refdata::messaging::preview_book_changes_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::preview_book_changes_request& v);
    static ores::refdata::messaging::preview_book_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::preview_book_changes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::preview_book_changes_response& v);
    static ores::refdata::messaging::preview_book_changes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::preview_book_changes_response& v);
    static ores::refdata::messaging::preview_book_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::raise_book_changes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::raise_book_changes_request& v);
    static ores::refdata::messaging::raise_book_changes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::raise_book_changes_request& v);
    static ores::refdata::messaging::raise_book_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::raise_book_changes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::raise_book_changes_response& v);
    static ores::refdata::messaging::raise_book_changes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::raise_book_changes_response& v);
    static ores::refdata::messaging::raise_book_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_field_delta> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_field_delta& v);
    static ores::refdata::messaging::book_field_delta read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_field_delta& v);
    static ores::refdata::messaging::book_field_delta read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_change_delta> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_change_delta& v);
    static ores::refdata::messaging::book_change_delta read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_change_delta& v);
    static ores::refdata::messaging::book_change_delta
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_changes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_changes_request& v);
    static ores::refdata::messaging::get_book_changes_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_changes_request& v);
    static ores::refdata::messaging::get_book_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_book_changes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_book_changes_response& v);
    static ores::refdata::messaging::get_book_changes_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_book_changes_response& v);
    static ores::refdata::messaging::get_book_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::recheck_book_changes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::recheck_book_changes_request& v);
    static ores::refdata::messaging::recheck_book_changes_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::recheck_book_changes_request& v);
    static ores::refdata::messaging::recheck_book_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::recheck_book_changes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::recheck_book_changes_response& v);
    static ores::refdata::messaging::recheck_book_changes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::recheck_book_changes_response& v);
    static ores::refdata::messaging::recheck_book_changes_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
