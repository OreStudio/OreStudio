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
#ifndef ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_OPERATIONS_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_OPERATIONS_PROTOCOL_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief One named process parameter, as edited in an IR curve editor's
 * parameter table.
 *
 * The wire-shaped analogue of a {parameter_definition_id, value} row: the
 * row-based store joins values onto the system-tenant parameter definitions
 * catalogue, but a stateless preview request carries the name itself, because
 * the screen already has the definitions on screen, and lets the handler
 * re-materialise the catalogue entry on the fly.
 */
struct parameter_spec {
    std::string parameter_name;
    double parameter_value = 0.0;

    friend bool operator==(const parameter_spec&, const parameter_spec&) = default;
};

/**
 * @brief Request a batch (dry-run) simulation of short-rate sample paths.
 *
 * The IR curve analogue of simulate_fx_spot_paths_request. Stateless: the
 * caller names the process parameters rather than a stored config, so an
 * unsaved editor can preview what it is about to save.
 */
struct simulate_ir_curve_paths_request {
    using response_type = struct simulate_ir_curve_paths_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.simulate_ir_curve_paths";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Short-rate process engine: "vasicek", "cox_ingersoll_ross",
     * "hull_white", or "two_factor_gaussian".
     */
    std::string process_type = "vasicek";
    /** @brief The process's named parameters, one entry per parameter. */
    std::vector<parameter_spec> parameters;
    /** @brief Number of ticks (steps) to generate per path. */
    int num_ticks = 100;
    /** @brief Number of independent sample paths to generate (for contrast). */
    int num_paths = 5;
    /** @brief Base RNG seed; path i uses seed + i. Change to reseed. */
    std::uint32_t seed = 1;

    friend bool operator==(const simulate_ir_curve_paths_request&,
                           const simulate_ir_curve_paths_request&) = default;
};

/**
 * @brief The generated batch, and why it is empty when it is.
 */
struct simulate_ir_curve_paths_response {
    bool success = false;
    std::string message;
    /** @brief num_paths series, each of num_ticks short-rate values. */
    std::vector<std::vector<double>> paths;

    friend bool operator==(const simulate_ir_curve_paths_response&,
                           const simulate_ir_curve_paths_response&) = default;
};

/**
 * @brief One (possibly unsaved) Curve Template row, as edited in an IR curve
 * editor's Curve Template tab.
 *
 * Deliberately not the persisted ir_curve_template_entry domain struct: a row
 * being edited has no id, party_id or tenant_id yet.
 */
struct preview_ir_curve_template_row {
    int sequence_index = 0;
    std::string start_tenor_code;
    std::string end_tenor_code;
    std::string instrument_code;

    friend bool operator==(const preview_ir_curve_template_row&,
                           const preview_ir_curve_template_row&) = default;
};

/**
 * @brief Request a stateless preview of the curve shape (rate per Curve
 * Template entry) implied by the given process parameters and template rows.
 *
 * Derives each row's rate the same way ir_curve_feed does
 * (i_yield_curve_process::discount_factor() + curve_instrument_pricer, via the
 * shared price_ir_curve_entry()), from the process's current state, with no
 * ticking, so an editor can show "if I save these parameters and this
 * template, here is the curve you would publish" before committing anything.
 * Nothing is persisted or published.
 */
struct preview_ir_curve_shape_request {
    using response_type = struct preview_ir_curve_shape_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.preview_ir_curve_shape";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Short-rate process engine: "vasicek", "cox_ingersoll_ross",
     * "hull_white", or "two_factor_gaussian".
     */
    std::string process_type = "vasicek";
    /** @brief The process's named parameters, one entry per parameter. */
    std::vector<parameter_spec> parameters;
    /**
     * @brief References ores.refdata.payment_frequency.code; needed to build a
     * Swap row's fixed-leg schedule.
     */
    std::string fixed_leg_payment_frequency_code = "Annual";
    /** @brief The template rows to price, in the order the editor shows them. */
    std::vector<preview_ir_curve_template_row> entries;

    friend bool operator==(const preview_ir_curve_shape_request&,
                           const preview_ir_curve_shape_request&) = default;
};

/**
 * @brief One priced Curve Template row.
 */
struct preview_ir_curve_shape_point {
    int sequence_index = 0;
    std::string start_tenor_code;
    std::string end_tenor_code;
    double rate = 0.0;

    friend bool operator==(const preview_ir_curve_shape_point&,
                           const preview_ir_curve_shape_point&) = default;
};

/**
 * @brief The priced curve, and why it is empty when it is.
 */
struct preview_ir_curve_shape_response {
    bool success = false;
    std::string message;
    /** @brief One point per request entry, sorted by sequence_index. */
    std::vector<preview_ir_curve_shape_point> points;

    friend bool operator==(const preview_ir_curve_shape_response&,
                           const preview_ir_curve_shape_response&) = default;
};

/**
 * @brief The members of parameter_spec, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<parameter_spec>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"parameter_name", &parameter_spec::parameter_name},
        ::ores::utility::codec::field{"parameter_value", &parameter_spec::parameter_value},
    };
}

/**
 * @brief The members of simulate_ir_curve_paths_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<simulate_ir_curve_paths_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"process_type",
                                      &simulate_ir_curve_paths_request::process_type},
        ::ores::utility::codec::field{"parameters", &simulate_ir_curve_paths_request::parameters},
        ::ores::utility::codec::field{"num_ticks", &simulate_ir_curve_paths_request::num_ticks},
        ::ores::utility::codec::field{"num_paths", &simulate_ir_curve_paths_request::num_paths},
        ::ores::utility::codec::field{"seed", &simulate_ir_curve_paths_request::seed},
    };
}

/**
 * @brief The members of simulate_ir_curve_paths_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<simulate_ir_curve_paths_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &simulate_ir_curve_paths_response::success},
        ::ores::utility::codec::field{"message", &simulate_ir_curve_paths_response::message},
        ::ores::utility::codec::field{"paths", &simulate_ir_curve_paths_response::paths},
    };
}

/**
 * @brief The members of preview_ir_curve_template_row, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_ir_curve_template_row>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"sequence_index",
                                      &preview_ir_curve_template_row::sequence_index},
        ::ores::utility::codec::field{"start_tenor_code",
                                      &preview_ir_curve_template_row::start_tenor_code},
        ::ores::utility::codec::field{"end_tenor_code",
                                      &preview_ir_curve_template_row::end_tenor_code},
        ::ores::utility::codec::field{"instrument_code",
                                      &preview_ir_curve_template_row::instrument_code},
    };
}

/**
 * @brief The members of preview_ir_curve_shape_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_ir_curve_shape_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"process_type",
                                      &preview_ir_curve_shape_request::process_type},
        ::ores::utility::codec::field{"parameters", &preview_ir_curve_shape_request::parameters},
        ::ores::utility::codec::field{
            "fixed_leg_payment_frequency_code",
            &preview_ir_curve_shape_request::fixed_leg_payment_frequency_code},
        ::ores::utility::codec::field{"entries", &preview_ir_curve_shape_request::entries},
    };
}

/**
 * @brief The members of preview_ir_curve_shape_point, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_ir_curve_shape_point>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"sequence_index",
                                      &preview_ir_curve_shape_point::sequence_index},
        ::ores::utility::codec::field{"start_tenor_code",
                                      &preview_ir_curve_shape_point::start_tenor_code},
        ::ores::utility::codec::field{"end_tenor_code",
                                      &preview_ir_curve_shape_point::end_tenor_code},
        ::ores::utility::codec::field{"rate", &preview_ir_curve_shape_point::rate},
    };
}

/**
 * @brief The members of preview_ir_curve_shape_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_ir_curve_shape_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &preview_ir_curve_shape_response::success},
        ::ores::utility::codec::field{"message", &preview_ir_curve_shape_response::message},
        ::ores::utility::codec::field{"points", &preview_ir_curve_shape_response::points},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::parameter_spec> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::parameter_spec& v);
    static ores::synthetic::messaging::parameter_spec read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::parameter_spec& v);
    static ores::synthetic::messaging::parameter_spec read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::simulate_ir_curve_paths_request& v);
    static ores::synthetic::messaging::simulate_ir_curve_paths_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::simulate_ir_curve_paths_request& v);
    static ores::synthetic::messaging::simulate_ir_curve_paths_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::simulate_ir_curve_paths_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::simulate_ir_curve_paths_response& v);
    static ores::synthetic::messaging::simulate_ir_curve_paths_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::simulate_ir_curve_paths_response& v);
    static ores::synthetic::messaging::simulate_ir_curve_paths_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::preview_ir_curve_template_row> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::preview_ir_curve_template_row& v);
    static ores::synthetic::messaging::preview_ir_curve_template_row
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::preview_ir_curve_template_row& v);
    static ores::synthetic::messaging::preview_ir_curve_template_row
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::preview_ir_curve_shape_request& v);
    static ores::synthetic::messaging::preview_ir_curve_shape_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::preview_ir_curve_shape_request& v);
    static ores::synthetic::messaging::preview_ir_curve_shape_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_point> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::preview_ir_curve_shape_point& v);
    static ores::synthetic::messaging::preview_ir_curve_shape_point
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::preview_ir_curve_shape_point& v);
    static ores::synthetic::messaging::preview_ir_curve_shape_point
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::preview_ir_curve_shape_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::preview_ir_curve_shape_response& v);
    static ores::synthetic::messaging::preview_ir_curve_shape_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::preview_ir_curve_shape_response& v);
    static ores::synthetic::messaging::preview_ir_curve_shape_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
