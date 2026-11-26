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
#ifndef ORES_REFDATA_API_MESSAGING_PRODUCT_APPROVAL_OPERATIONS_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PRODUCT_APPROVAL_OPERATIONS_PROTOCOL_HPP

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
 * @brief The product types one book permits, as the chain resolves them.
 *
 * The set is absent when the book, its desk and its party all restrict nothing,
 * because no level then names a type the catalogue does not: every type is
 * permitted. The set is empty when a level restricts and names nothing, which
 * permits none. A caller that folded the two together would read an open book
 * and a shut one alike.
 */
struct book_product_approval {
    boost::uuids::uuid book_id;
    std::optional<std::vector<std::string>> allowed_product_types;

    friend bool operator==(const book_product_approval&, const book_product_approval&) = default;
};

/**
 * @brief Reads the resolved set of each book the request names.
 *
 * A book of another tenant is a row the caller cannot see through RLS, and the
 * resolution answers an empty set for it, so a caller reads it as permitting
 * nothing rather than permitting everything.
 */
struct get_book_product_approvals_request {
    using response_type = struct get_book_product_approvals_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.get_book_product_approvals";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The books to resolve, in the order the answer states them.
     */
    std::vector<boost::uuids::uuid> book_ids;

    friend bool operator==(const get_book_product_approvals_request&,
                           const get_book_product_approvals_request&) = default;
};

struct get_book_product_approvals_response {
    ores::utility::domain::result result;
    /**
     * @brief One entry per book named, in the order the request named them.
     *
     * A book that is not the tenant's resolves to the empty set, which is the
     * resolution's answer for a row the caller cannot see.
     */
    std::vector<book_product_approval> approvals;

    friend bool operator==(const get_book_product_approvals_response&,
                           const get_book_product_approvals_response&) = default;
};

/**
 * @brief The members of book_product_approval, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<book_product_approval>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"book_id", &book_product_approval::book_id},
        ::ores::utility::codec::field{"allowed_product_types",
                                      &book_product_approval::allowed_product_types},
    };
}

/**
 * @brief The members of get_book_product_approvals_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_book_product_approvals_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"book_ids", &get_book_product_approvals_request::book_ids},
    };
}

/**
 * @brief The members of get_book_product_approvals_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_book_product_approvals_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"result", &get_book_product_approvals_response::result},
        ::ores::utility::codec::field{"approvals", &get_book_product_approvals_response::approvals},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::book_product_approval> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::book_product_approval& v);
    static ores::refdata::messaging::book_product_approval read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::book_product_approval& v);
    static ores::refdata::messaging::book_product_approval
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_book_product_approvals_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_book_product_approvals_request& v);
    static ores::refdata::messaging::get_book_product_approvals_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_book_product_approvals_request& v);
    static ores::refdata::messaging::get_book_product_approvals_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_book_product_approvals_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_book_product_approvals_response& v);
    static ores::refdata::messaging::get_book_product_approvals_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_book_product_approvals_response& v);
    static ores::refdata::messaging::get_book_product_approvals_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
