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
#ifndef ORES_MARKETDATA_API_MESSAGING_FX_VOL_QUOTE_FIT_OPERATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FX_VOL_QUOTE_FIT_OPERATION_PROTOCOL_HPP

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
 * @brief One quoted point of a source's surface: its expiry, its strike label as quoted, such as
 * ATM, 25RR or 10BF, and its volatility.
 */
struct fx_vol_quote {
    std::string expiry;
    std::string label;
    double volatility = 0.0;

    friend bool operator==(const fx_vol_quote&, const fx_vol_quote&) = default;
};

/**
 * @brief The quotes of one source, such as a vendor or the consensus of others, fitted on their
 * own.
 */
struct fx_vol_quote_source {
    std::string name;
    std::vector<fx_vol_quote> quotes;

    friend bool operator==(const fx_vol_quote_source&, const fx_vol_quote_source&) = default;
};

/**
 * @brief One point of a source's smile by delta: its expiry, its delta label, such as 25P, ATM or
 * 10C, and its volatility.
 */
struct fx_vol_delta_point {
    std::string expiry;
    std::string strike;
    double volatility = 0.0;

    friend bool operator==(const fx_vol_delta_point&, const fx_vol_delta_point&) = default;
};

/**
 * @brief One source's smile by delta, expiry by expiry, or why it could not be fitted.
 */
struct fx_vol_source_fit {
    std::string name;
    bool fitted = false;
    /**
     * @brief Why the source was not fitted; empty when it was.
     */
    std::string message;
    /**
     * @brief The volatility at each delta of each fitted expiry, shortest expiry first and each
     * expiry's strikes from the put wing to the call wing.
     */
    std::vector<fx_vol_delta_point> points;
    /**
     * @brief Each quoted expiry the fit left out, with why, such as "10Y: no pillar quoted".
     */
    std::vector<std::string> unfitted_expiries;

    friend bool operator==(const fx_vol_source_fit&, const fx_vol_source_fit&) = default;
};

/**
 * @brief Reads the quotes of each source as a smile by delta: fits SSVI to the source's ATM, risk
 * reversals and butterflies in the pair's FX option convention, on the caller's party's published
 * forward set, and answers the volatility at 10P, 25P, ATM, 25C and 10C of each expiry. Writes
 * nothing.
 */
struct fit_fx_vol_quotes_request {
    using response_type = struct fit_fx_vol_quotes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.fit_fx_vol_quotes";
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
     * @brief The instant the forward set is read at.
     */
    std::chrono::system_clock::time_point as_of;
    std::vector<fx_vol_quote_source> sources;

    friend bool operator==(const fit_fx_vol_quotes_request&,
                           const fit_fx_vol_quotes_request&) = default;
};

/**
 * @brief Each source's smile by delta, or why the request was refused.
 */
struct fit_fx_vol_quotes_response {
    bool success = false;
    std::string message;
    /**
     * @brief The FX option convention the quotes were read in, such as EUR-USD-FXOPTION.
     */
    std::string convention_id;
    /**
     * @brief The convention's delta type, such as PaSpot, which the deltas of the answer are in.
     */
    std::string delta_type;
    std::vector<fx_vol_source_fit> fits;

    friend bool operator==(const fit_fx_vol_quotes_response&,
                           const fit_fx_vol_quotes_response&) = default;
};

/**
 * @brief The members of fx_vol_quote, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_quote>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"expiry", &fx_vol_quote::expiry},
        ::ores::utility::codec::field{"label", &fx_vol_quote::label},
        ::ores::utility::codec::field{"volatility", &fx_vol_quote::volatility},
    };
}

/**
 * @brief The members of fx_vol_quote_source, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_quote_source>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &fx_vol_quote_source::name},
        ::ores::utility::codec::field{"quotes", &fx_vol_quote_source::quotes},
    };
}

/**
 * @brief The members of fx_vol_delta_point, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_delta_point>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"expiry", &fx_vol_delta_point::expiry},
        ::ores::utility::codec::field{"strike", &fx_vol_delta_point::strike},
        ::ores::utility::codec::field{"volatility", &fx_vol_delta_point::volatility},
    };
}

/**
 * @brief The members of fx_vol_source_fit, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_source_fit>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &fx_vol_source_fit::name},
        ::ores::utility::codec::field{"fitted", &fx_vol_source_fit::fitted},
        ::ores::utility::codec::field{"message", &fx_vol_source_fit::message},
        ::ores::utility::codec::field{"points", &fx_vol_source_fit::points},
        ::ores::utility::codec::field{"unfitted_expiries", &fx_vol_source_fit::unfitted_expiries},
    };
}

/**
 * @brief The members of fit_fx_vol_quotes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fit_fx_vol_quotes_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"pair_code", &fit_fx_vol_quotes_request::pair_code},
        ::ores::utility::codec::field{"as_of", &fit_fx_vol_quotes_request::as_of},
        ::ores::utility::codec::field{"sources", &fit_fx_vol_quotes_request::sources},
    };
}

/**
 * @brief The members of fit_fx_vol_quotes_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fit_fx_vol_quotes_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &fit_fx_vol_quotes_response::success},
        ::ores::utility::codec::field{"message", &fit_fx_vol_quotes_response::message},
        ::ores::utility::codec::field{"convention_id", &fit_fx_vol_quotes_response::convention_id},
        ::ores::utility::codec::field{"delta_type", &fit_fx_vol_quotes_response::delta_type},
        ::ores::utility::codec::field{"fits", &fit_fx_vol_quotes_response::fits},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_quote> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_quote& v);
    static ores::marketdata::messaging::fx_vol_quote read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_quote& v);
    static ores::marketdata::messaging::fx_vol_quote read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_quote_source> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_quote_source& v);
    static ores::marketdata::messaging::fx_vol_quote_source read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_quote_source& v);
    static ores::marketdata::messaging::fx_vol_quote_source
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_delta_point> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_delta_point& v);
    static ores::marketdata::messaging::fx_vol_delta_point read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_delta_point& v);
    static ores::marketdata::messaging::fx_vol_delta_point
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_source_fit> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_source_fit& v);
    static ores::marketdata::messaging::fx_vol_source_fit read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_source_fit& v);
    static ores::marketdata::messaging::fx_vol_source_fit
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fit_fx_vol_quotes_request& v);
    static ores::marketdata::messaging::fit_fx_vol_quotes_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fit_fx_vol_quotes_request& v);
    static ores::marketdata::messaging::fit_fx_vol_quotes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fit_fx_vol_quotes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fit_fx_vol_quotes_response& v);
    static ores::marketdata::messaging::fit_fx_vol_quotes_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fit_fx_vol_quotes_response& v);
    static ores::marketdata::messaging::fit_fx_vol_quotes_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
