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
#ifndef ORES_MARKETDATA_API_MESSAGING_BILLING_OPERATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_BILLING_OPERATION_PROTOCOL_HPP

#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One component of a rate card, the quantity it counted and what it costs.
 */
struct bill_line_view {
    std::string driver;
    std::string licence_right;
    std::int64_t quantity = 0;
    std::int64_t charge_micros = 0;
    std::int64_t total_micros = 0;

    friend bool operator==(const bill_line_view&, const bill_line_view&) = default;
};

/**
 * @brief The bill of a source.
 *
 * A shadow bill is the bill of a simulated source. It has the same lines and the same amounts as a
 * bill of a live source, and nobody pays it.
 */
struct source_bill_view {
    boost::uuids::uuid feed_id;
    std::string source_name;
    std::string currency;
    bool shadow = false;
    std::vector<bill_line_view> lines;
    std::int64_t total_micros = 0;

    friend bool operator==(const source_bill_view&, const source_bill_view&) = default;
};

/**
 * @brief The charge of a party for a component of a source.
 */
struct party_charge_view {
    std::string tenant_id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid feed_id;
    std::string driver;
    std::string licence_right;
    std::int64_t total_micros = 0;

    friend bool operator==(const party_charge_view&, const party_charge_view&) = default;
};

/**
 * @brief Asks for the bills of a half open period.
 *
 * A period of no length, or one that ends before it starts, is refused.
 */
struct compute_bill_request {
    using response_type = struct compute_bill_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.compute_bill";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::chrono::system_clock::time_point period_from;
    std::chrono::system_clock::time_point period_to;
    std::string source_name;

    friend bool operator==(const compute_bill_request&, const compute_bill_request&) = default;
};

/**
 * @brief The source bills, the charge of each party and the part that no party bears.
 */
struct compute_bill_response {
    std::vector<source_bill_view> source_bills;
    std::vector<party_charge_view> party_charges;
    std::int64_t unallocated_micros = 0;
    bool success = false;
    std::string message;

    friend bool operator==(const compute_bill_response&, const compute_bill_response&) = default;
};

/**
 * @brief The members of bill_line_view, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bill_line_view>) {
    return std::tuple{
        ::ores::utility::codec::field{"driver", &bill_line_view::driver},
        ::ores::utility::codec::field{"licence_right", &bill_line_view::licence_right},
        ::ores::utility::codec::field{"quantity", &bill_line_view::quantity},
        ::ores::utility::codec::field{"charge_micros", &bill_line_view::charge_micros},
        ::ores::utility::codec::field{"total_micros", &bill_line_view::total_micros},
    };
}

/**
 * @brief The members of source_bill_view, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<source_bill_view>) {
    return std::tuple{
        ::ores::utility::codec::field{"feed_id", &source_bill_view::feed_id},
        ::ores::utility::codec::field{"source_name", &source_bill_view::source_name},
        ::ores::utility::codec::field{"currency", &source_bill_view::currency},
        ::ores::utility::codec::field{"shadow", &source_bill_view::shadow},
        ::ores::utility::codec::field{"lines", &source_bill_view::lines},
        ::ores::utility::codec::field{"total_micros", &source_bill_view::total_micros},
    };
}

/**
 * @brief The members of party_charge_view, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<party_charge_view>) {
    return std::tuple{
        ::ores::utility::codec::field{"tenant_id", &party_charge_view::tenant_id},
        ::ores::utility::codec::field{"party_id", &party_charge_view::party_id},
        ::ores::utility::codec::field{"feed_id", &party_charge_view::feed_id},
        ::ores::utility::codec::field{"driver", &party_charge_view::driver},
        ::ores::utility::codec::field{"licence_right", &party_charge_view::licence_right},
        ::ores::utility::codec::field{"total_micros", &party_charge_view::total_micros},
    };
}

/**
 * @brief The members of compute_bill_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<compute_bill_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"period_from", &compute_bill_request::period_from},
        ::ores::utility::codec::field{"period_to", &compute_bill_request::period_to},
        ::ores::utility::codec::field{"source_name", &compute_bill_request::source_name},
    };
}

/**
 * @brief The members of compute_bill_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<compute_bill_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"source_bills", &compute_bill_response::source_bills},
        ::ores::utility::codec::field{"party_charges", &compute_bill_response::party_charges},
        ::ores::utility::codec::field{"unallocated_micros",
                                      &compute_bill_response::unallocated_micros},
        ::ores::utility::codec::field{"success", &compute_bill_response::success},
        ::ores::utility::codec::field{"message", &compute_bill_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::bill_line_view> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::bill_line_view& v);
    static ores::marketdata::messaging::bill_line_view read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::bill_line_view& v);
    static ores::marketdata::messaging::bill_line_view
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::source_bill_view> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::source_bill_view& v);
    static ores::marketdata::messaging::source_bill_view read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::source_bill_view& v);
    static ores::marketdata::messaging::source_bill_view
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::party_charge_view> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::party_charge_view& v);
    static ores::marketdata::messaging::party_charge_view read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::party_charge_view& v);
    static ores::marketdata::messaging::party_charge_view
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::compute_bill_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::compute_bill_request& v);
    static ores::marketdata::messaging::compute_bill_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::compute_bill_request& v);
    static ores::marketdata::messaging::compute_bill_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::compute_bill_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::compute_bill_response& v);
    static ores::marketdata::messaging::compute_bill_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::compute_bill_response& v);
    static ores::marketdata::messaging::compute_bill_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
