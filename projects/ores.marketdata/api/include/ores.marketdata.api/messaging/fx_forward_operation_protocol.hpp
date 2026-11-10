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
#include <optional>
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
    /**
     * @brief The curve publish's state now: published, withdrawn or superseded. A set built from a
     * withdrawn curve says so.
     */
    std::string state_code = "";

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
 * @brief One spot observation a published set read: its own for a USD pair, or one USD leg of a
 * cross.
 */
struct fx_forward_spot_source {
    std::string leg_code;
    std::string pair_code;
    std::string series_id;
    std::chrono::system_clock::time_point observed_at;
    double value = 0.0;

    friend bool operator==(const fx_forward_spot_source&, const fx_forward_spot_source&) = default;
};

/**
 * @brief Raises, or applies, the publish of one pair's forward set for the caller's party. A call
 * that names no approved request computes the set from the party's spot and published funding
 * curves, raises a marketdata.fx_forward_publish request and stores the set as its candidate. A
 * call that names the approved request publishes that candidate, unchanged. A set whose spot or
 * curve is missing is refused, naming the input, and nothing is written.
 */
struct publish_fx_forward_request {
    using response_type = struct publish_fx_forward_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.publish_fx_forward";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The pair, such as EUR/USD.
     */
    std::string pair_code;
    /**
     * @brief The instant the set reads the market at.
     */
    std::chrono::system_clock::time_point as_of;
    /**
     * @brief The approved request this call applies, or empty to raise one.
     */
    std::string approval_request_id = "";
    /**
     * @brief Why the set should be published, for the decider to read.
     */
    std::string reason = "";

    friend bool operator==(const publish_fx_forward_request&,
                           const publish_fx_forward_request&) = default;
};

/**
 * @brief What the call did: raised a request, published a set, or refused and why.
 */
struct publish_fx_forward_response {
    bool success = false;
    std::string message;
    std::string approval_request_id;
    std::string state_code;
    std::optional<std::chrono::system_clock::time_point> published_at;
    std::string published_by;

    friend bool operator==(const publish_fx_forward_response&,
                           const publish_fx_forward_response&) = default;
};

/**
 * @brief Reads the caller's party's published forward set of a pair: the newest one at or before
 * the as-of, exactly as it was published, with where each number came from.
 */
struct get_published_fx_forward_request {
    using response_type = struct get_published_fx_forward_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_published_fx_forward";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string pair_code;
    std::chrono::system_clock::time_point as_of;

    friend bool operator==(const get_published_fx_forward_request&,
                           const get_published_fx_forward_request&) = default;
};

/**
 * @brief One published forward set and its provenance, or why there is none.
 */
struct get_published_fx_forward_response {
    bool success = false;
    std::string message;
    std::string publish_id;
    std::string request_id;
    std::string party_id;
    std::string pair_code;
    std::chrono::system_clock::time_point as_of;
    std::chrono::year_month_day spot_date = {};
    double spot_rate = 0.0;
    std::string published_by;
    std::optional<std::chrono::system_clock::time_point> published_at;
    int convention_version = 0;
    std::string calendar_codes;
    std::string method_code;
    fx_forward_curve_source base_curve;
    fx_forward_curve_source quote_curve;
    /**
     * @brief Business days from the trade date to spot, from the convention.
     */
    int spot_days = 0;
    /**
     * @brief The size of one pip of the pair, from the convention.
     */
    double pip_factor = 0.0;
    /**
     * @brief The decimal places the pair is quoted to, from the convention.
     */
    int decimal_places = 0;
    /**
     * @brief Who raised the publish.
     */
    std::string requested_by = "";
    /**
     * @brief The state of the request that authorised the publish.
     */
    std::string approval_state_code = "";
    /**
     * @brief The rule that closed the request without a person, or empty when a person decided it.
     */
    std::string auto_approval_code = "";
    /**
     * @brief Who decided the request: a person, or the account that closed it under the rule.
     */
    std::string decided_by = "";
    /**
     * @brief The decision's comment. Under auto-approval it names the rule and who turned it on.
     */
    std::string decision_comment = "";
    std::vector<fx_forward_spot_source> spots;
    std::vector<fx_forward_ladder_row> rows;

    friend bool operator==(const get_published_fx_forward_response&,
                           const get_published_fx_forward_response&) = default;
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
        ::ores::utility::codec::field{"state_code", &fx_forward_curve_source::state_code},
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
 * @brief The members of fx_forward_spot_source, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_spot_source>) {
    return std::tuple{
        ::ores::utility::codec::field{"leg_code", &fx_forward_spot_source::leg_code},
        ::ores::utility::codec::field{"pair_code", &fx_forward_spot_source::pair_code},
        ::ores::utility::codec::field{"series_id", &fx_forward_spot_source::series_id},
        ::ores::utility::codec::field{"observed_at", &fx_forward_spot_source::observed_at},
        ::ores::utility::codec::field{"value", &fx_forward_spot_source::value},
    };
}

/**
 * @brief The members of publish_fx_forward_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_fx_forward_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"pair_code", &publish_fx_forward_request::pair_code},
        ::ores::utility::codec::field{"as_of", &publish_fx_forward_request::as_of},
        ::ores::utility::codec::field{"approval_request_id",
                                      &publish_fx_forward_request::approval_request_id},
        ::ores::utility::codec::field{"reason", &publish_fx_forward_request::reason},
    };
}

/**
 * @brief The members of publish_fx_forward_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_fx_forward_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &publish_fx_forward_response::success},
        ::ores::utility::codec::field{"message", &publish_fx_forward_response::message},
        ::ores::utility::codec::field{"approval_request_id",
                                      &publish_fx_forward_response::approval_request_id},
        ::ores::utility::codec::field{"state_code", &publish_fx_forward_response::state_code},
        ::ores::utility::codec::field{"published_at", &publish_fx_forward_response::published_at},
        ::ores::utility::codec::field{"published_by", &publish_fx_forward_response::published_by},
    };
}

/**
 * @brief The members of get_published_fx_forward_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_published_fx_forward_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"pair_code", &get_published_fx_forward_request::pair_code},
        ::ores::utility::codec::field{"as_of", &get_published_fx_forward_request::as_of},
    };
}

/**
 * @brief The members of get_published_fx_forward_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_published_fx_forward_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_published_fx_forward_response::success},
        ::ores::utility::codec::field{"message", &get_published_fx_forward_response::message},
        ::ores::utility::codec::field{"publish_id", &get_published_fx_forward_response::publish_id},
        ::ores::utility::codec::field{"request_id", &get_published_fx_forward_response::request_id},
        ::ores::utility::codec::field{"party_id", &get_published_fx_forward_response::party_id},
        ::ores::utility::codec::field{"pair_code", &get_published_fx_forward_response::pair_code},
        ::ores::utility::codec::field{"as_of", &get_published_fx_forward_response::as_of},
        ::ores::utility::codec::field{"spot_date", &get_published_fx_forward_response::spot_date},
        ::ores::utility::codec::field{"spot_rate", &get_published_fx_forward_response::spot_rate},
        ::ores::utility::codec::field{"published_by",
                                      &get_published_fx_forward_response::published_by},
        ::ores::utility::codec::field{"published_at",
                                      &get_published_fx_forward_response::published_at},
        ::ores::utility::codec::field{"convention_version",
                                      &get_published_fx_forward_response::convention_version},
        ::ores::utility::codec::field{"calendar_codes",
                                      &get_published_fx_forward_response::calendar_codes},
        ::ores::utility::codec::field{"method_code",
                                      &get_published_fx_forward_response::method_code},
        ::ores::utility::codec::field{"base_curve", &get_published_fx_forward_response::base_curve},
        ::ores::utility::codec::field{"quote_curve",
                                      &get_published_fx_forward_response::quote_curve},
        ::ores::utility::codec::field{"spot_days", &get_published_fx_forward_response::spot_days},
        ::ores::utility::codec::field{"pip_factor", &get_published_fx_forward_response::pip_factor},
        ::ores::utility::codec::field{"decimal_places",
                                      &get_published_fx_forward_response::decimal_places},
        ::ores::utility::codec::field{"requested_by",
                                      &get_published_fx_forward_response::requested_by},
        ::ores::utility::codec::field{"approval_state_code",
                                      &get_published_fx_forward_response::approval_state_code},
        ::ores::utility::codec::field{"auto_approval_code",
                                      &get_published_fx_forward_response::auto_approval_code},
        ::ores::utility::codec::field{"decided_by", &get_published_fx_forward_response::decided_by},
        ::ores::utility::codec::field{"decision_comment",
                                      &get_published_fx_forward_response::decision_comment},
        ::ores::utility::codec::field{"spots", &get_published_fx_forward_response::spots},
        ::ores::utility::codec::field{"rows", &get_published_fx_forward_response::rows},
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
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_forward_spot_source> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_forward_spot_source& v);
    static ores::marketdata::messaging::fx_forward_spot_source read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_forward_spot_source& v);
    static ores::marketdata::messaging::fx_forward_spot_source
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::publish_fx_forward_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::publish_fx_forward_request& v);
    static ores::marketdata::messaging::publish_fx_forward_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::publish_fx_forward_request& v);
    static ores::marketdata::messaging::publish_fx_forward_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::publish_fx_forward_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::publish_fx_forward_response& v);
    static ores::marketdata::messaging::publish_fx_forward_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::publish_fx_forward_response& v);
    static ores::marketdata::messaging::publish_fx_forward_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_published_fx_forward_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_published_fx_forward_request& v);
    static ores::marketdata::messaging::get_published_fx_forward_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_published_fx_forward_request& v);
    static ores::marketdata::messaging::get_published_fx_forward_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_published_fx_forward_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_published_fx_forward_response& v);
    static ores::marketdata::messaging::get_published_fx_forward_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_published_fx_forward_response& v);
    static ores::marketdata::messaging::get_published_fx_forward_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
