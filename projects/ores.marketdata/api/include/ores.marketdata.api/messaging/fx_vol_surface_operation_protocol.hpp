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
#ifndef ORES_MARKETDATA_API_MESSAGING_FX_VOL_SURFACE_OPERATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FX_VOL_SURFACE_OPERATION_PROTOCOL_HPP

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
 * @brief How far one quoted pillar is from the calibrated surface.
 */
struct fx_vol_pillar_fit {
    std::string point;
    std::string expiry;
    std::string label;
    double quoted = 0.0;
    double fitted = 0.0;
    double error = 0.0;
    bool beyond_tolerance = false;

    friend bool operator==(const fx_vol_pillar_fit&, const fx_vol_pillar_fit&) = default;
};

/**
 * @brief One point of the candidate's grid, as written to the output series.
 */
struct fx_vol_grid_point {
    std::string expiry;
    std::string strike;
    double log_moneyness = 0.0;
    double volatility = 0.0;

    friend bool operator==(const fx_vol_grid_point&, const fx_vol_grid_point&) = default;
};

/**
 * @brief One arbitrage condition the calibrated surface breaks.
 */
struct fx_vol_violation {
    std::string kind;
    std::string expiry;
    std::string description;

    friend bool operator==(const fx_vol_violation&, const fx_vol_violation&) = default;
};

/**
 * @brief A pillar no contributor quoted at the as-of, with the series it read.
 */
struct fx_vol_missing_pillar {
    std::string point;
    std::vector<std::string> series_ids;

    friend bool operator==(const fx_vol_missing_pillar&, const fx_vol_missing_pillar&) = default;
};

/**
 * @brief Calibrates one FX volatility surface blueprint of the caller's party at an as-of and
 * writes the candidate. The recipe is checked before any quote is read. A refusal names what
 * failed, and writes nothing.
 */
struct calibrate_fx_vol_surface_request {
    using response_type = struct calibrate_fx_vol_surface_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.calibrate_fx_vol_surface";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The blueprint to calibrate.
     */
    std::string blueprint_id;
    /**
     * @brief The instant the calibration reads its quotes and forwards at.
     */
    std::chrono::system_clock::time_point as_of;

    friend bool operator==(const calibrate_fx_vol_surface_request&,
                           const calibrate_fx_vol_surface_request&) = default;
};

/**
 * @brief The candidate the calibration wrote, or why it refused.
 */
struct calibrate_fx_vol_surface_response {
    bool success = false;
    std::string message;
    std::string calibration_id;
    std::string blueprint_name;
    int blueprint_version = 0;
    std::chrono::system_clock::time_point as_of;
    std::string forward_publish_id;
    double rho = 0.0;
    double eta = 0.0;
    double gamma = 0.0;
    bool converged = false;
    double max_abs_error = 0.0;
    std::vector<fx_vol_pillar_fit> pillars;
    std::vector<fx_vol_grid_point> grid;
    std::vector<fx_vol_violation> violations;
    std::vector<fx_vol_missing_pillar> missing;

    friend bool operator==(const calibrate_fx_vol_surface_response&,
                           const calibrate_fx_vol_surface_response&) = default;
};

/**
 * @brief The members of fx_vol_pillar_fit, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_pillar_fit>) {
    return std::tuple{
        ::ores::utility::codec::field{"point", &fx_vol_pillar_fit::point},
        ::ores::utility::codec::field{"expiry", &fx_vol_pillar_fit::expiry},
        ::ores::utility::codec::field{"label", &fx_vol_pillar_fit::label},
        ::ores::utility::codec::field{"quoted", &fx_vol_pillar_fit::quoted},
        ::ores::utility::codec::field{"fitted", &fx_vol_pillar_fit::fitted},
        ::ores::utility::codec::field{"error", &fx_vol_pillar_fit::error},
        ::ores::utility::codec::field{"beyond_tolerance", &fx_vol_pillar_fit::beyond_tolerance},
    };
}

/**
 * @brief The members of fx_vol_grid_point, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_grid_point>) {
    return std::tuple{
        ::ores::utility::codec::field{"expiry", &fx_vol_grid_point::expiry},
        ::ores::utility::codec::field{"strike", &fx_vol_grid_point::strike},
        ::ores::utility::codec::field{"log_moneyness", &fx_vol_grid_point::log_moneyness},
        ::ores::utility::codec::field{"volatility", &fx_vol_grid_point::volatility},
    };
}

/**
 * @brief The members of fx_vol_violation, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_violation>) {
    return std::tuple{
        ::ores::utility::codec::field{"kind", &fx_vol_violation::kind},
        ::ores::utility::codec::field{"expiry", &fx_vol_violation::expiry},
        ::ores::utility::codec::field{"description", &fx_vol_violation::description},
    };
}

/**
 * @brief The members of fx_vol_missing_pillar, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_missing_pillar>) {
    return std::tuple{
        ::ores::utility::codec::field{"point", &fx_vol_missing_pillar::point},
        ::ores::utility::codec::field{"series_ids", &fx_vol_missing_pillar::series_ids},
    };
}

/**
 * @brief The members of calibrate_fx_vol_surface_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibrate_fx_vol_surface_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"blueprint_id",
                                      &calibrate_fx_vol_surface_request::blueprint_id},
        ::ores::utility::codec::field{"as_of", &calibrate_fx_vol_surface_request::as_of},
    };
}

/**
 * @brief The members of calibrate_fx_vol_surface_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibrate_fx_vol_surface_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &calibrate_fx_vol_surface_response::success},
        ::ores::utility::codec::field{"message", &calibrate_fx_vol_surface_response::message},
        ::ores::utility::codec::field{"calibration_id",
                                      &calibrate_fx_vol_surface_response::calibration_id},
        ::ores::utility::codec::field{"blueprint_name",
                                      &calibrate_fx_vol_surface_response::blueprint_name},
        ::ores::utility::codec::field{"blueprint_version",
                                      &calibrate_fx_vol_surface_response::blueprint_version},
        ::ores::utility::codec::field{"as_of", &calibrate_fx_vol_surface_response::as_of},
        ::ores::utility::codec::field{"forward_publish_id",
                                      &calibrate_fx_vol_surface_response::forward_publish_id},
        ::ores::utility::codec::field{"rho", &calibrate_fx_vol_surface_response::rho},
        ::ores::utility::codec::field{"eta", &calibrate_fx_vol_surface_response::eta},
        ::ores::utility::codec::field{"gamma", &calibrate_fx_vol_surface_response::gamma},
        ::ores::utility::codec::field{"converged", &calibrate_fx_vol_surface_response::converged},
        ::ores::utility::codec::field{"max_abs_error",
                                      &calibrate_fx_vol_surface_response::max_abs_error},
        ::ores::utility::codec::field{"pillars", &calibrate_fx_vol_surface_response::pillars},
        ::ores::utility::codec::field{"grid", &calibrate_fx_vol_surface_response::grid},
        ::ores::utility::codec::field{"violations", &calibrate_fx_vol_surface_response::violations},
        ::ores::utility::codec::field{"missing", &calibrate_fx_vol_surface_response::missing},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_pillar_fit> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_pillar_fit& v);
    static ores::marketdata::messaging::fx_vol_pillar_fit read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_pillar_fit& v);
    static ores::marketdata::messaging::fx_vol_pillar_fit
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_grid_point> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_grid_point& v);
    static ores::marketdata::messaging::fx_vol_grid_point read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_grid_point& v);
    static ores::marketdata::messaging::fx_vol_grid_point
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_violation> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_violation& v);
    static ores::marketdata::messaging::fx_vol_violation read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_violation& v);
    static ores::marketdata::messaging::fx_vol_violation
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_missing_pillar> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_missing_pillar& v);
    static ores::marketdata::messaging::fx_vol_missing_pillar read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_missing_pillar& v);
    static ores::marketdata::messaging::fx_vol_missing_pillar
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::calibrate_fx_vol_surface_request& v);
    static ores::marketdata::messaging::calibrate_fx_vol_surface_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::calibrate_fx_vol_surface_request& v);
    static ores::marketdata::messaging::calibrate_fx_vol_surface_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::calibrate_fx_vol_surface_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::calibrate_fx_vol_surface_response& v);
    static ores::marketdata::messaging::calibrate_fx_vol_surface_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::calibrate_fx_vol_surface_response& v);
    static ores::marketdata::messaging::calibrate_fx_vol_surface_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
