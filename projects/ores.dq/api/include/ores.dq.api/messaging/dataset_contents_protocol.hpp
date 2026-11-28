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
#ifndef ORES_DQ_API_MESSAGING_DATASET_CONTENTS_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_DATASET_CONTENTS_PROTOCOL_HPP

#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief Asks for the tables of one dataset, or for one page of one table's rows.
 */
struct dataset_contents_request {
    using response_type = struct dataset_contents_response;
    static constexpr std::string_view nats_subject = "dq.v1.ops.dataset_contents";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The dataset whose contents to read.
     */
    std::string dataset_code;
    /**
     * @brief The table whose rows to read, or empty for the list of tables alone.
     */
    std::string table_name;
    /**
     * @brief How many rows to answer, at most.
     */
    int limit = 50;
    /**
     * @brief How many rows to skip before the ones answered.
     */
    int offset = 0;

    friend bool operator==(const dataset_contents_request&,
                           const dataset_contents_request&) = default;
};

/**
 * @brief One table a dataset stages into.
 */
struct dataset_table {
    /**
     * @brief The table's name, as the store states it.
     */
    std::string table_name;
    /**
     * @brief Where the table sits in the family its artefact type names.
     */
    int display_order = 0;
    /**
     * @brief The columns this table carries, comma separated, without the two every
     * family shares.
     */
    std::string columns;
    /**
     * @brief How many rows this table holds for the dataset.
     */
    std::int64_t staged_count = 0;

    friend bool operator==(const dataset_table&, const dataset_table&) = default;
};

/**
 * @brief Reports the tables and, when one was named, its rows.
 */
struct dataset_contents_response {
    /**
     * @brief Whether the contents were read.
     */
    bool success = false;
    /**
     * @brief Why they could not be read, when they could not.
     */
    std::string error_message;
    /**
     * @brief The tables the dataset stages into, in its family's order.
     */
    std::vector<dataset_table> tables;
    /**
     * @brief One page of the named table's rows, as JSON: the columns it carries, the
     * rows in order, and the number the table holds.
     */
    std::string rows;

    friend bool operator==(const dataset_contents_response&,
                           const dataset_contents_response&) = default;
};

/**
 * @brief The members of dataset_contents_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_contents_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"dataset_code", &dataset_contents_request::dataset_code},
        ::ores::utility::codec::field{"table_name", &dataset_contents_request::table_name},
        ::ores::utility::codec::field{"limit", &dataset_contents_request::limit},
        ::ores::utility::codec::field{"offset", &dataset_contents_request::offset},
    };
}

/**
 * @brief The members of dataset_table, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_table>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"table_name", &dataset_table::table_name},
        ::ores::utility::codec::field{"display_order", &dataset_table::display_order},
        ::ores::utility::codec::field{"columns", &dataset_table::columns},
        ::ores::utility::codec::field{"staged_count", &dataset_table::staged_count},
    };
}

/**
 * @brief The members of dataset_contents_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dataset_contents_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &dataset_contents_response::success},
        ::ores::utility::codec::field{"error_message", &dataset_contents_response::error_message},
        ::ores::utility::codec::field{"tables", &dataset_contents_response::tables},
        ::ores::utility::codec::field{"rows", &dataset_contents_response::rows},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_contents_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_contents_request& v);
    static ores::dq::messaging::dataset_contents_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_contents_request& v);
    static ores::dq::messaging::dataset_contents_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_table> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_table& v);
    static ores::dq::messaging::dataset_table read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_table& v);
    static ores::dq::messaging::dataset_table read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dataset_contents_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dataset_contents_response& v);
    static ores::dq::messaging::dataset_contents_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dataset_contents_response& v);
    static ores::dq::messaging::dataset_contents_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
