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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROPOSAL_OPERATIONS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROPOSAL_OPERATIONS_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty_change.hpp"
#include "ores.refdata.api/domain/counterparty_identifier_change.hpp"
#include "ores.refdata.api/domain/party_counterparty_change.hpp"
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
 * The refusal is empty when the write would stand. The columns are the ones the
 * line changes, and the parts are the ones the policy names for the line alone.
 */
struct counterparty_line_outcome {
    int line_no = 0;
    /**
     * @brief counterparty, counterparty_identifier or party_counterparty.
     */
    std::string entity_type;
    std::string operation;
    /**
     * @brief The row the line is about: its id, or the party and the counterparty of a link.
     */
    std::string subject_id;
    std::vector<std::string> columns;
    std::string refusal;
    std::vector<std::string> part_codes;

    friend bool operator==(const counterparty_line_outcome&,
                           const counterparty_line_outcome&) = default;
};

/**
 * @brief Runs each proposed line through the real write and writes nothing.
 *
 * A put line needs the permission to write what it names, a delete line the
 * permission to delete it. The lines are checked in order inside one transaction
 * that is never committed.
 */
struct preview_counterparty_changes_request {
    using response_type = struct preview_counterparty_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.preview_counterparty_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The proposed writes to counterparties. The request, line number and part of a line are
     * not read: the server numbers the lines and the policy names the parts.
     */
    std::vector<ores::refdata::domain::counterparty_change> counterparties;
    std::vector<ores::refdata::domain::counterparty_identifier_change> identifiers;
    std::vector<ores::refdata::domain::party_counterparty_change> links;

    friend bool operator==(const preview_counterparty_changes_request&,
                           const preview_counterparty_changes_request&) = default;
};

struct preview_counterparty_changes_response {
    ores::utility::domain::result result;
    std::vector<counterparty_line_outcome> lines;
    /**
     * @brief The parts the policy needs for the lines together. Empty when no policy row gates
     * them.
     */
    std::vector<std::string> part_codes;

    friend bool operator==(const preview_counterparty_changes_response&,
                           const preview_counterparty_changes_response&) = default;
};

/**
 * @brief Holds the proposed lines in an approval request.
 *
 * Previews first. A refused line, or lines no policy row gates, raise nothing.
 */
struct raise_counterparty_changes_request {
    using response_type = struct raise_counterparty_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.raise_counterparty_changes";
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
    std::vector<ores::refdata::domain::counterparty_change> counterparties;
    std::vector<ores::refdata::domain::counterparty_identifier_change> identifiers;
    std::vector<ores::refdata::domain::party_counterparty_change> links;

    friend bool operator==(const raise_counterparty_changes_request&,
                           const raise_counterparty_changes_request&) = default;
};

struct raise_counterparty_changes_response {
    ores::utility::domain::result result;
    /**
     * @brief The id of the request as raised, when the outcome is ok. The inbox operations read the
     * request itself.
     */
    std::optional<boost::uuids::uuid> request_id;
    std::vector<counterparty_line_outcome> lines;
    std::vector<std::string> part_codes;

    friend bool operator==(const raise_counterparty_changes_response&,
                           const raise_counterparty_changes_response&) = default;
};

/**
 * @brief One column of a proposed line beside the live row's value.
 *
 * The values are text whatever the column's type, because a screen draws them and
 * does not compute with them. The before is empty when the line creates the row.
 */
struct counterparty_field_delta {
    std::string name;
    std::string before;
    std::string after;
    bool changed = false;

    friend bool operator==(const counterparty_field_delta&,
                           const counterparty_field_delta&) = default;
};

/**
 * @brief One line of a request as the decider reads it: the kind of row, the
 * operation, the row it names, and each column beside the value it replaces.
 */
struct counterparty_change_delta {
    int line_no = 0;
    std::string entity_type;
    std::string operation;
    std::string subject_id;
    int base_version = 0;
    bool applied = false;
    std::vector<counterparty_field_delta> fields;

    friend bool operator==(const counterparty_change_delta&,
                           const counterparty_change_delta&) = default;
};

/**
 * @brief Reads the lines a request holds, each beside the row it changes.
 *
 * Answered to the person who raised the request, to whoever may decide its kind,
 * and to whoever may read the tenant's requests. Answered as not found otherwise,
 * as opening the request is.
 */
struct get_counterparty_changes_request {
    using response_type = struct get_counterparty_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.get_counterparty_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;

    friend bool operator==(const get_counterparty_changes_request&,
                           const get_counterparty_changes_request&) = default;
};

struct get_counterparty_changes_response {
    ores::utility::domain::result result;
    /**
     * @brief The lines in line order.
     */
    std::vector<counterparty_change_delta> changes;

    friend bool operator==(const get_counterparty_changes_response&,
                           const get_counterparty_changes_response&) = default;
};

/**
 * @brief Runs the lines a request holds through the real write again and writes
 * nothing.
 *
 * The live rows may have moved since the request was raised, so the checks a
 * decider reads are run now. Answered to the same callers as the lines are.
 */
struct recheck_counterparty_changes_request {
    using response_type = struct recheck_counterparty_changes_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.recheck_counterparty_changes";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string request_id;

    friend bool operator==(const recheck_counterparty_changes_request&,
                           const recheck_counterparty_changes_request&) = default;
};

struct recheck_counterparty_changes_response {
    ores::utility::domain::result result;
    std::vector<counterparty_line_outcome> lines;
    std::vector<std::string> part_codes;

    friend bool operator==(const recheck_counterparty_changes_response&,
                           const recheck_counterparty_changes_response&) = default;
};

/**
 * @brief The members of counterparty_line_outcome, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_line_outcome>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"line_no", &counterparty_line_outcome::line_no},
        ::ores::utility::codec::field{"entity_type", &counterparty_line_outcome::entity_type},
        ::ores::utility::codec::field{"operation", &counterparty_line_outcome::operation},
        ::ores::utility::codec::field{"subject_id", &counterparty_line_outcome::subject_id},
        ::ores::utility::codec::field{"columns", &counterparty_line_outcome::columns},
        ::ores::utility::codec::field{"refusal", &counterparty_line_outcome::refusal},
        ::ores::utility::codec::field{"part_codes", &counterparty_line_outcome::part_codes},
    };
}

/**
 * @brief The members of preview_counterparty_changes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_counterparty_changes_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"counterparties",
                                      &preview_counterparty_changes_request::counterparties},
        ::ores::utility::codec::field{"identifiers",
                                      &preview_counterparty_changes_request::identifiers},
        ::ores::utility::codec::field{"links", &preview_counterparty_changes_request::links},
    };
}

/**
 * @brief The members of preview_counterparty_changes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<preview_counterparty_changes_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &preview_counterparty_changes_response::result},
        ::ores::utility::codec::field{"lines", &preview_counterparty_changes_response::lines},
        ::ores::utility::codec::field{"part_codes",
                                      &preview_counterparty_changes_response::part_codes},
    };
}

/**
 * @brief The members of raise_counterparty_changes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_counterparty_changes_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"reason", &raise_counterparty_changes_request::reason},
        ::ores::utility::codec::field{"counterparties",
                                      &raise_counterparty_changes_request::counterparties},
        ::ores::utility::codec::field{"identifiers",
                                      &raise_counterparty_changes_request::identifiers},
        ::ores::utility::codec::field{"links", &raise_counterparty_changes_request::links},
    };
}

/**
 * @brief The members of raise_counterparty_changes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<raise_counterparty_changes_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &raise_counterparty_changes_response::result},
        ::ores::utility::codec::field{"request_id",
                                      &raise_counterparty_changes_response::request_id},
        ::ores::utility::codec::field{"lines", &raise_counterparty_changes_response::lines},
        ::ores::utility::codec::field{"part_codes",
                                      &raise_counterparty_changes_response::part_codes},
    };
}

/**
 * @brief The members of counterparty_field_delta, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_field_delta>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &counterparty_field_delta::name},
        ::ores::utility::codec::field{"before", &counterparty_field_delta::before},
        ::ores::utility::codec::field{"after", &counterparty_field_delta::after},
        ::ores::utility::codec::field{"changed", &counterparty_field_delta::changed},
    };
}

/**
 * @brief The members of counterparty_change_delta, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<counterparty_change_delta>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"line_no", &counterparty_change_delta::line_no},
        ::ores::utility::codec::field{"entity_type", &counterparty_change_delta::entity_type},
        ::ores::utility::codec::field{"operation", &counterparty_change_delta::operation},
        ::ores::utility::codec::field{"subject_id", &counterparty_change_delta::subject_id},
        ::ores::utility::codec::field{"base_version", &counterparty_change_delta::base_version},
        ::ores::utility::codec::field{"applied", &counterparty_change_delta::applied},
        ::ores::utility::codec::field{"fields", &counterparty_change_delta::fields},
    };
}

/**
 * @brief The members of get_counterparty_changes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_counterparty_changes_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"request_id", &get_counterparty_changes_request::request_id},
    };
}

/**
 * @brief The members of get_counterparty_changes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_counterparty_changes_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &get_counterparty_changes_response::result},
        ::ores::utility::codec::field{"changes", &get_counterparty_changes_response::changes},
    };
}

/**
 * @brief The members of recheck_counterparty_changes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<recheck_counterparty_changes_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"request_id",
                                      &recheck_counterparty_changes_request::request_id},
    };
}

/**
 * @brief The members of recheck_counterparty_changes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<recheck_counterparty_changes_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &recheck_counterparty_changes_response::result},
        ::ores::utility::codec::field{"lines", &recheck_counterparty_changes_response::lines},
        ::ores::utility::codec::field{"part_codes",
                                      &recheck_counterparty_changes_response::part_codes},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_line_outcome> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_line_outcome& v);
    static ores::refdata::messaging::counterparty_line_outcome read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_line_outcome& v);
    static ores::refdata::messaging::counterparty_line_outcome
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::preview_counterparty_changes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::preview_counterparty_changes_request& v);
    static ores::refdata::messaging::preview_counterparty_changes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::preview_counterparty_changes_request& v);
    static ores::refdata::messaging::preview_counterparty_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::preview_counterparty_changes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::preview_counterparty_changes_response& v);
    static ores::refdata::messaging::preview_counterparty_changes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::preview_counterparty_changes_response& v);
    static ores::refdata::messaging::preview_counterparty_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::raise_counterparty_changes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::raise_counterparty_changes_request& v);
    static ores::refdata::messaging::raise_counterparty_changes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::raise_counterparty_changes_request& v);
    static ores::refdata::messaging::raise_counterparty_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::raise_counterparty_changes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::raise_counterparty_changes_response& v);
    static ores::refdata::messaging::raise_counterparty_changes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::raise_counterparty_changes_response& v);
    static ores::refdata::messaging::raise_counterparty_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_field_delta> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_field_delta& v);
    static ores::refdata::messaging::counterparty_field_delta read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_field_delta& v);
    static ores::refdata::messaging::counterparty_field_delta
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_change_delta> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_change_delta& v);
    static ores::refdata::messaging::counterparty_change_delta read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_change_delta& v);
    static ores::refdata::messaging::counterparty_change_delta
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_changes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_changes_request& v);
    static ores::refdata::messaging::get_counterparty_changes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_changes_request& v);
    static ores::refdata::messaging::get_counterparty_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_changes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_changes_response& v);
    static ores::refdata::messaging::get_counterparty_changes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_changes_response& v);
    static ores::refdata::messaging::get_counterparty_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::recheck_counterparty_changes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::recheck_counterparty_changes_request& v);
    static ores::refdata::messaging::recheck_counterparty_changes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::recheck_counterparty_changes_request& v);
    static ores::refdata::messaging::recheck_counterparty_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::recheck_counterparty_changes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::recheck_counterparty_changes_response& v);
    static ores::refdata::messaging::recheck_counterparty_changes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::recheck_counterparty_changes_response& v);
    static ores::refdata::messaging::recheck_counterparty_changes_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
