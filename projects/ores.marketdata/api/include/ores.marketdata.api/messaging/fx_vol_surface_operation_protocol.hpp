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
#include <optional>
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
 * @brief Raises, or applies, the publish of a calibrated surface of the caller's party. A call that
 * names no approved request raises a marketdata.fx_vol_surface_publish request for the calibration
 * of the blueprint at the as-of, and records its checks for the decider. A call that names the
 * approved request publishes that calibration and ends the set it replaces. A blueprint with no
 * calibration at the as-of is refused, and nothing is written.
 */
struct publish_fx_vol_surface_request {
    using response_type = struct publish_fx_vol_surface_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.publish_fx_vol_surface";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string blueprint_id;
    std::chrono::system_clock::time_point as_of;
    /**
     * @brief The approved request this call applies, or empty to raise one.
     */
    std::string approval_request_id = "";
    /**
     * @brief Why the surface should be published, for the decider to read.
     */
    std::string reason = "";

    friend bool operator==(const publish_fx_vol_surface_request&,
                           const publish_fx_vol_surface_request&) = default;
};

/**
 * @brief What the call did: raised a request, published a surface, or refused and why.
 */
struct publish_fx_vol_surface_response {
    bool success = false;
    std::string message;
    std::string approval_request_id;
    std::string state_code;
    std::optional<std::chrono::system_clock::time_point> published_at;
    std::string published_by;

    friend bool operator==(const publish_fx_vol_surface_response&,
                           const publish_fx_vol_surface_response&) = default;
};

/**
 * @brief Reads the subject of a surface publish request: the candidate it authorises, the checks
 * the raise recorded, and what has happened to it since.
 */
struct get_fx_vol_surface_publish_subject_request {
    using response_type = struct get_fx_vol_surface_publish_subject_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.ops.get_fx_vol_surface_publish_subject";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string approval_request_id;

    friend bool operator==(const get_fx_vol_surface_publish_subject_request&,
                           const get_fx_vol_surface_publish_subject_request&) = default;
};

/**
 * @brief The subject of a surface publish request, or why it cannot be read.
 */
struct get_fx_vol_surface_publish_subject_response {
    bool success = false;
    std::string message;
    std::string approval_request_id;
    std::string state_code;
    std::string blueprint_name;
    std::string party_id;
    std::chrono::system_clock::time_point as_of;
    std::string calibration_id;
    int calibration_version = 0;
    bool converged = false;
    double max_abs_error = 0.0;
    int pillars_beyond_tolerance = 0;
    int violation_count = 0;
    std::optional<std::chrono::system_clock::time_point> published_at;
    std::string published_by;
    std::optional<std::chrono::system_clock::time_point> superseded_at;
    std::string superseded_by_request_id;

    friend bool operator==(const get_fx_vol_surface_publish_subject_response&,
                           const get_fx_vol_surface_publish_subject_response&) = default;
};

/**
 * @brief One point of a published surface: its expiry, its strike and its volatility.
 */
struct fx_vol_surface_point {
    std::string expiry;
    std::string strike;
    double volatility = 0.0;

    friend bool operator==(const fx_vol_surface_point&, const fx_vol_surface_point&) = default;
};

/**
 * @brief Reads the published surface of a blueprint of the caller's party: the newest published set
 * at or before the as-of, exactly as it was published. Naming an expiry reads that expiry across
 * the strikes; naming a strike reads that strike across the expiries. A candidate is never read.
 */
struct get_published_fx_vol_surface_request {
    using response_type = struct get_published_fx_vol_surface_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.ops.get_published_fx_vol_surface";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string blueprint_id;
    std::chrono::system_clock::time_point as_of;
    /**
     * @brief The expiry to slice at, such as 3M, or empty for every expiry.
     */
    std::string expiry = "";
    /**
     * @brief The strike to slice at, such as ATM or DEL/Spot/Put/25, or empty for every strike.
     */
    std::string strike = "";

    friend bool operator==(const get_published_fx_vol_surface_request&,
                           const get_published_fx_vol_surface_request&) = default;
};

/**
 * @brief The published surface, or the slice of it asked for, and the publish it answers from.
 */
struct get_published_fx_vol_surface_response {
    bool success = false;
    std::string message;
    std::string blueprint_name;
    std::string publish_request_id;
    std::chrono::system_clock::time_point as_of;
    std::optional<std::chrono::system_clock::time_point> published_at;
    std::string published_by;
    std::optional<std::chrono::system_clock::time_point> superseded_at;
    /**
     * @brief The expiries the answer holds, shortest first.
     */
    std::vector<std::string> expiries;
    /**
     * @brief The strikes the answer holds, in the blueprint's order.
     */
    std::vector<std::string> strikes;
    /**
     * @brief The points, expiry by expiry, each expiry's strikes in order.
     */
    std::vector<fx_vol_surface_point> points;

    friend bool operator==(const get_published_fx_vol_surface_response&,
                           const get_published_fx_vol_surface_response&) = default;
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
    return ::ores::utility::codec::field_list{
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
    return ::ores::utility::codec::field_list{
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
    return ::ores::utility::codec::field_list{
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
    return ::ores::utility::codec::field_list{
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
    return ::ores::utility::codec::field_list{
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
    return ::ores::utility::codec::field_list{
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

/**
 * @brief The members of publish_fx_vol_surface_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_fx_vol_surface_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"blueprint_id",
                                      &publish_fx_vol_surface_request::blueprint_id},
        ::ores::utility::codec::field{"as_of", &publish_fx_vol_surface_request::as_of},
        ::ores::utility::codec::field{"approval_request_id",
                                      &publish_fx_vol_surface_request::approval_request_id},
        ::ores::utility::codec::field{"reason", &publish_fx_vol_surface_request::reason},
    };
}

/**
 * @brief The members of publish_fx_vol_surface_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publish_fx_vol_surface_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &publish_fx_vol_surface_response::success},
        ::ores::utility::codec::field{"message", &publish_fx_vol_surface_response::message},
        ::ores::utility::codec::field{"approval_request_id",
                                      &publish_fx_vol_surface_response::approval_request_id},
        ::ores::utility::codec::field{"state_code", &publish_fx_vol_surface_response::state_code},
        ::ores::utility::codec::field{"published_at",
                                      &publish_fx_vol_surface_response::published_at},
        ::ores::utility::codec::field{"published_by",
                                      &publish_fx_vol_surface_response::published_by},
    };
}

/**
 * @brief The members of get_fx_vol_surface_publish_subject_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_vol_surface_publish_subject_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{
            "approval_request_id",
            &get_fx_vol_surface_publish_subject_request::approval_request_id},
    };
}

/**
 * @brief The members of get_fx_vol_surface_publish_subject_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_vol_surface_publish_subject_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success",
                                      &get_fx_vol_surface_publish_subject_response::success},
        ::ores::utility::codec::field{"message",
                                      &get_fx_vol_surface_publish_subject_response::message},
        ::ores::utility::codec::field{
            "approval_request_id",
            &get_fx_vol_surface_publish_subject_response::approval_request_id},
        ::ores::utility::codec::field{"state_code",
                                      &get_fx_vol_surface_publish_subject_response::state_code},
        ::ores::utility::codec::field{"blueprint_name",
                                      &get_fx_vol_surface_publish_subject_response::blueprint_name},
        ::ores::utility::codec::field{"party_id",
                                      &get_fx_vol_surface_publish_subject_response::party_id},
        ::ores::utility::codec::field{"as_of", &get_fx_vol_surface_publish_subject_response::as_of},
        ::ores::utility::codec::field{"calibration_id",
                                      &get_fx_vol_surface_publish_subject_response::calibration_id},
        ::ores::utility::codec::field{
            "calibration_version",
            &get_fx_vol_surface_publish_subject_response::calibration_version},
        ::ores::utility::codec::field{"converged",
                                      &get_fx_vol_surface_publish_subject_response::converged},
        ::ores::utility::codec::field{"max_abs_error",
                                      &get_fx_vol_surface_publish_subject_response::max_abs_error},
        ::ores::utility::codec::field{
            "pillars_beyond_tolerance",
            &get_fx_vol_surface_publish_subject_response::pillars_beyond_tolerance},
        ::ores::utility::codec::field{
            "violation_count", &get_fx_vol_surface_publish_subject_response::violation_count},
        ::ores::utility::codec::field{"published_at",
                                      &get_fx_vol_surface_publish_subject_response::published_at},
        ::ores::utility::codec::field{"published_by",
                                      &get_fx_vol_surface_publish_subject_response::published_by},
        ::ores::utility::codec::field{"superseded_at",
                                      &get_fx_vol_surface_publish_subject_response::superseded_at},
        ::ores::utility::codec::field{
            "superseded_by_request_id",
            &get_fx_vol_surface_publish_subject_response::superseded_by_request_id},
    };
}

/**
 * @brief The members of fx_vol_surface_point, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_vol_surface_point>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"expiry", &fx_vol_surface_point::expiry},
        ::ores::utility::codec::field{"strike", &fx_vol_surface_point::strike},
        ::ores::utility::codec::field{"volatility", &fx_vol_surface_point::volatility},
    };
}

/**
 * @brief The members of get_published_fx_vol_surface_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_published_fx_vol_surface_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"blueprint_id",
                                      &get_published_fx_vol_surface_request::blueprint_id},
        ::ores::utility::codec::field{"as_of", &get_published_fx_vol_surface_request::as_of},
        ::ores::utility::codec::field{"expiry", &get_published_fx_vol_surface_request::expiry},
        ::ores::utility::codec::field{"strike", &get_published_fx_vol_surface_request::strike},
    };
}

/**
 * @brief The members of get_published_fx_vol_surface_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_published_fx_vol_surface_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_published_fx_vol_surface_response::success},
        ::ores::utility::codec::field{"message", &get_published_fx_vol_surface_response::message},
        ::ores::utility::codec::field{"blueprint_name",
                                      &get_published_fx_vol_surface_response::blueprint_name},
        ::ores::utility::codec::field{"publish_request_id",
                                      &get_published_fx_vol_surface_response::publish_request_id},
        ::ores::utility::codec::field{"as_of", &get_published_fx_vol_surface_response::as_of},
        ::ores::utility::codec::field{"published_at",
                                      &get_published_fx_vol_surface_response::published_at},
        ::ores::utility::codec::field{"published_by",
                                      &get_published_fx_vol_surface_response::published_by},
        ::ores::utility::codec::field{"superseded_at",
                                      &get_published_fx_vol_surface_response::superseded_at},
        ::ores::utility::codec::field{"expiries", &get_published_fx_vol_surface_response::expiries},
        ::ores::utility::codec::field{"strikes", &get_published_fx_vol_surface_response::strikes},
        ::ores::utility::codec::field{"points", &get_published_fx_vol_surface_response::points},
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
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::publish_fx_vol_surface_request& v);
    static ores::marketdata::messaging::publish_fx_vol_surface_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::publish_fx_vol_surface_request& v);
    static ores::marketdata::messaging::publish_fx_vol_surface_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::publish_fx_vol_surface_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::publish_fx_vol_surface_response& v);
    static ores::marketdata::messaging::publish_fx_vol_surface_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::publish_fx_vol_surface_response& v);
    static ores::marketdata::messaging::publish_fx_vol_surface_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request& v);
    static ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request& v);
    static ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response& v);
    static ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response& v);
    static ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::fx_vol_surface_point> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::fx_vol_surface_point& v);
    static ores::marketdata::messaging::fx_vol_surface_point read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::fx_vol_surface_point& v);
    static ores::marketdata::messaging::fx_vol_surface_point
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_published_fx_vol_surface_request& v);
    static ores::marketdata::messaging::get_published_fx_vol_surface_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_published_fx_vol_surface_request& v);
    static ores::marketdata::messaging::get_published_fx_vol_surface_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_published_fx_vol_surface_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_published_fx_vol_surface_response& v);
    static ores::marketdata::messaging::get_published_fx_vol_surface_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_published_fx_vol_surface_response& v);
    static ores::marketdata::messaging::get_published_fx_vol_surface_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
