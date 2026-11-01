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
#ifndef ORES_TRADING_API_MESSAGING_BLOTTER_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_BLOTTER_PROTOCOL_HPP

#include "ores.trading.api/domain/trade_summary.hpp"
#include "ores.trading.api/export.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/result.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::trading::messaging {

/**
 * @brief One node of a rail, and the rows a filter places on it.
 */
struct blotter_rail_node {
    /**
     * @brief Which rail this node belongs to: portfolio, book, counterparty,
     * asset class or state.
     */
    std::string dimension;
    /**
     * @brief The node's own id, empty for the party at the head of a rail.
     */
    std::string node_id;
    /**
     * @brief How many rows the filter places on this node. A portfolio's figure
     * counts every row below it, because that is what a person reading the tree
     * takes it to mean.
     */
    std::uint64_t count;
};

/**
 * @brief The rail's counts for one filter, every dimension in one answer.
 *
 * One read rather than one per rail: the counts come from the same scan of the
 * same rows, and a screen that asks five times pays five times for it.
 */
struct get_blotter_rail_request {
    using response_type = struct get_blotter_rail_response;
    static constexpr std::string_view nats_subject = "trading.v1.blotter.rail";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Which rows the rail counts. Absent counts every row of the party.
     */
    std::optional<ores::trading::messaging::trade_summaries_filter> filter;
};

/**
 * @brief The counts at each node of each rail.
 */
struct get_blotter_rail_response {
    /**
     * @brief Outcome of the operation.
     */
    ores::utility::domain::result result;
    /**
     * @brief The nodes, each stating its own dimension. The party's own totals
     * come first, with an empty node id.
     */
    std::vector<blotter_rail_node> nodes;
};

/**
 * @brief One page of blotter rows, and where the next page starts.
 *
 * The page is keyed rather than offset: the caller states the last row it was
 * given, and the store starts after it. Every page then costs what the first
 * costs, which an offset cannot promise because it re-walks what it skips.
 */
struct get_blotter_page_request {
    using response_type = struct get_blotter_page_response;
    static constexpr std::string_view nats_subject = "trading.v1.blotter.page";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Which rows the page may hold. Absent states the whole party.
     */
    std::optional<ores::trading::messaging::trade_summaries_filter> filter;
    /**
     * @brief The column the page is ordered by. The row id is the tie-break, so
     * the order is total and a key is unambiguous.
     */
    ores::utility::domain::order order;
    /**
     * @brief How many rows the caller will accept. The service states its own
     * ceiling, because a page is a page and not the book.
     */
    std::uint32_t limit;
    /**
     * @brief The key to start after, as the previous answer stated it. Empty
     * starts at the first row. It is the row's sort value and id together, and the
     * caller does not read it: it is opaque, so the order can change without the
     * client changing.
     */
    std::string after;
};

/**
 * @brief One page of rows, and the key the next page starts after.
 */
struct get_blotter_page_response {
    /**
     * @brief Outcome of the operation.
     */
    ores::utility::domain::result result;
    /**
     * @brief The page, at most the limit asked for.
     */
    std::vector<ores::trading::domain::trade_summary> rows;
    /**
     * @brief The key the next page starts after, empty when this page is the
     * last. A caller that reaches the end is told so rather than guessing from a
     * short page, because a page may be short for a reason of its own.
     */
    std::string next;
};

/**
 * @brief The members of blotter_rail_node, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const blotter_rail_node&) {
    return std::tuple{
        ::ores::utility::codec::field{"dimension", &blotter_rail_node::dimension},
        ::ores::utility::codec::field{"node_id", &blotter_rail_node::node_id},
        ::ores::utility::codec::field{"count", &blotter_rail_node::count},
    };
}

/**
 * @brief The members of get_blotter_rail_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_blotter_rail_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"filter", &get_blotter_rail_request::filter},
    };
}

/**
 * @brief The members of get_blotter_rail_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_blotter_rail_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_blotter_rail_response::result},
        ::ores::utility::codec::field{"nodes", &get_blotter_rail_response::nodes},
    };
}

/**
 * @brief The members of get_blotter_page_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_blotter_page_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"filter", &get_blotter_page_request::filter},
        ::ores::utility::codec::field{"order", &get_blotter_page_request::order},
        ::ores::utility::codec::field{"limit", &get_blotter_page_request::limit},
        ::ores::utility::codec::field{"after", &get_blotter_page_request::after},
    };
}

/**
 * @brief The members of get_blotter_page_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_blotter_page_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_blotter_page_response::result},
        ::ores::utility::codec::field{"rows", &get_blotter_page_response::rows},
        ::ores::utility::codec::field{"next", &get_blotter_page_response::next},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::blotter_rail_node> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::blotter_rail_node& v);
    static ores::trading::messaging::blotter_rail_node read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::blotter_rail_node& v);
    static ores::trading::messaging::blotter_rail_node
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_blotter_rail_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_blotter_rail_request& v);
    static ores::trading::messaging::get_blotter_rail_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_blotter_rail_request& v);
    static ores::trading::messaging::get_blotter_rail_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_blotter_rail_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_blotter_rail_response& v);
    static ores::trading::messaging::get_blotter_rail_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_blotter_rail_response& v);
    static ores::trading::messaging::get_blotter_rail_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_blotter_page_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_blotter_page_request& v);
    static ores::trading::messaging::get_blotter_page_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_blotter_page_request& v);
    static ores::trading::messaging::get_blotter_page_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_blotter_page_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_blotter_page_response& v);
    static ores::trading::messaging::get_blotter_page_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_blotter_page_response& v);
    static ores::trading::messaging::get_blotter_page_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
