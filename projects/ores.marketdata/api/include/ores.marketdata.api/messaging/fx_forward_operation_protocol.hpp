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
#ifndef ORES_MARKETDATA_API_MESSAGING_FX_FORWARD_OPERATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FX_FORWARD_OPERATION_PROTOCOL_HPP

#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <chrono>
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief The published funding curve one side of the pair was read from.
 */
struct fx_forward_curve_source {
    std::string currency;
    std::string curve_name;
    std::string output_series_id;
    std::string publish_id;
    std::chrono::system_clock::time_point as_of;
    std::string published_by;

    friend bool operator==(const fx_forward_curve_source&,
                           const fx_forward_curve_source&) = default;
};

/**
 * @brief One tenor of the ladder.
 *
 * Forward points run from spot to the value date. Swap points run from the near
 * date to the value date, so a reader can recompute them for another near date
 * from the forward rates alone.
 */
struct fx_forward_ladder_row {
    std::string tenor;
    std::chrono::year_month_day near_date = {};
    std::chrono::year_month_day value_date = {};
    double base_discount = 0.0;
    double quote_discount = 0.0;
    double forward_rate = 0.0;
    double forward_points = 0.0;
    double swap_points = 0.0;

    friend bool operator==(const fx_forward_ladder_row&, const fx_forward_ladder_row&) = default;
};

/**
 * @brief The forward ladder of one USD pair at one as-of.
 */
struct get_fx_forward_ladder_request {
    using response_type = struct get_fx_forward_ladder_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_fx_forward_ladder";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string pair_code;
    std::chrono::system_clock::time_point as_of;
    std::string party_id;

    friend bool operator==(const get_fx_forward_ladder_request&,
                           const get_fx_forward_ladder_request&) = default;
};

struct get_fx_forward_ladder_response {
    bool success = false;
    std::string message;
    std::string pair_code;
    std::chrono::system_clock::time_point as_of;
    double spot = 0.0;
    std::chrono::system_clock::time_point spot_observed_at;
    std::chrono::year_month_day spot_date = {};
    int spot_days = 0;
    double pip_factor = 0.0;
    int decimal_places = 0;
    fx_forward_curve_source base_curve;
    fx_forward_curve_source quote_curve;
    std::vector<fx_forward_ladder_row> rows;

    friend bool operator==(const get_fx_forward_ladder_response&,
                           const get_fx_forward_ladder_response&) = default;
};

/**
 * @brief The members of fx_forward_curve_source, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_curve_source>) {
    return std::tuple{
        ::ores::utility::codec::field{"currency", &fx_forward_curve_source::currency},
        ::ores::utility::codec::field{"curve_name", &fx_forward_curve_source::curve_name},
        ::ores::utility::codec::field{"output_series_id",
                                      &fx_forward_curve_source::output_series_id},
        ::ores::utility::codec::field{"publish_id", &fx_forward_curve_source::publish_id},
        ::ores::utility::codec::field{"as_of", &fx_forward_curve_source::as_of},
        ::ores::utility::codec::field{"published_by", &fx_forward_curve_source::published_by},
    };
}

/**
 * @brief The members of fx_forward_ladder_row, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_ladder_row>) {
    return std::tuple{
        ::ores::utility::codec::field{"tenor", &fx_forward_ladder_row::tenor},
        ::ores::utility::codec::field{"near_date", &fx_forward_ladder_row::near_date},
        ::ores::utility::codec::field{"value_date", &fx_forward_ladder_row::value_date},
        ::ores::utility::codec::field{"base_discount", &fx_forward_ladder_row::base_discount},
        ::ores::utility::codec::field{"quote_discount", &fx_forward_ladder_row::quote_discount},
        ::ores::utility::codec::field{"forward_rate", &fx_forward_ladder_row::forward_rate},
        ::ores::utility::codec::field{"forward_points", &fx_forward_ladder_row::forward_points},
        ::ores::utility::codec::field{"swap_points", &fx_forward_ladder_row::swap_points},
    };
}

/**
 * @brief The members of get_fx_forward_ladder_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_forward_ladder_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"pair_code", &get_fx_forward_ladder_request::pair_code},
        ::ores::utility::codec::field{"as_of", &get_fx_forward_ladder_request::as_of},
        ::ores::utility::codec::field{"party_id", &get_fx_forward_ladder_request::party_id},
    };
}

/**
 * @brief The members of get_fx_forward_ladder_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_forward_ladder_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_fx_forward_ladder_response::success},
        ::ores::utility::codec::field{"message", &get_fx_forward_ladder_response::message},
        ::ores::utility::codec::field{"pair_code", &get_fx_forward_ladder_response::pair_code},
        ::ores::utility::codec::field{"as_of", &get_fx_forward_ladder_response::as_of},
        ::ores::utility::codec::field{"spot", &get_fx_forward_ladder_response::spot},
        ::ores::utility::codec::field{"spot_observed_at",
                                      &get_fx_forward_ladder_response::spot_observed_at},
        ::ores::utility::codec::field{"spot_date", &get_fx_forward_ladder_response::spot_date},
        ::ores::utility::codec::field{"spot_days", &get_fx_forward_ladder_response::spot_days},
        ::ores::utility::codec::field{"pip_factor", &get_fx_forward_ladder_response::pip_factor},
        ::ores::utility::codec::field{"decimal_places",
                                      &get_fx_forward_ladder_response::decimal_places},
        ::ores::utility::codec::field{"base_curve", &get_fx_forward_ladder_response::base_curve},
        ::ores::utility::codec::field{"quote_curve", &get_fx_forward_ladder_response::quote_curve},
        ::ores::utility::codec::field{"rows", &get_fx_forward_ladder_response::rows},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fx_forward_curve_source> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_forward_curve_source& v);
    static ores::marketdata::messaging::fx_forward_curve_source read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_forward_curve_source& v);
    static ores::marketdata::messaging::fx_forward_curve_source
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_forward_ladder_row> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_forward_ladder_row& v);
    static ores::marketdata::messaging::fx_forward_ladder_row read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_forward_ladder_row& v);
    static ores::marketdata::messaging::fx_forward_ladder_row
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_forward_ladder_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_forward_ladder_request& v);
    static ores::marketdata::messaging::get_fx_forward_ladder_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_fx_forward_ladder_request& v);
    static ores::marketdata::messaging::get_fx_forward_ladder_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_forward_ladder_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_forward_ladder_response& v);
    static ores::marketdata::messaging::get_fx_forward_ladder_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_fx_forward_ladder_response& v);
    static ores::marketdata::messaging::get_fx_forward_ladder_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
