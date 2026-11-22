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
#ifndef ORES_ORGMODE_API_MESSAGING_INDEX_OPERATIONS_PROTOCOL_HPP
#define ORES_ORGMODE_API_MESSAGING_INDEX_OPERATIONS_PROTOCOL_HPP

#include "ores.orgmode.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::orgmode::messaging {

/**
 * @brief Rebuild the knowledge index from the loaded knowledge pages.
 */
struct reindex_knowledge_pages_request {
    using response_type = struct reindex_knowledge_pages_response;
    static constexpr std::string_view nats_subject = "orgmode.v1.ops.reindex_knowledge_pages";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const reindex_knowledge_pages_request&,
                           const reindex_knowledge_pages_request&) = default;
};

struct reindex_knowledge_pages_response {
    std::uint64_t pages = 0;
    std::uint64_t sections = 0;
    std::uint64_t links = 0;
    bool success = false;
    std::string message;

    friend bool operator==(const reindex_knowledge_pages_response&,
                           const reindex_knowledge_pages_response&) = default;
};

/**
 * @brief The members of reindex_knowledge_pages_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<reindex_knowledge_pages_request>) {
    return ::ores::utility::codec::field_list{};
}

/**
 * @brief The members of reindex_knowledge_pages_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<reindex_knowledge_pages_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"pages", &reindex_knowledge_pages_response::pages},
        ::ores::utility::codec::field{"sections", &reindex_knowledge_pages_response::sections},
        ::ores::utility::codec::field{"links", &reindex_knowledge_pages_response::links},
        ::ores::utility::codec::field{"success", &reindex_knowledge_pages_response::success},
        ::ores::utility::codec::field{"message", &reindex_knowledge_pages_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::reindex_knowledge_pages_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::reindex_knowledge_pages_request& v);
    static ores::orgmode::messaging::reindex_knowledge_pages_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::reindex_knowledge_pages_request& v);
    static ores::orgmode::messaging::reindex_knowledge_pages_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ORGMODE_API_EXPORT
    wire_traits<ores::orgmode::messaging::reindex_knowledge_pages_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::orgmode::messaging::reindex_knowledge_pages_response& v);
    static ores::orgmode::messaging::reindex_knowledge_pages_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::orgmode::messaging::reindex_knowledge_pages_response& v);
    static ores::orgmode::messaging::reindex_knowledge_pages_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
