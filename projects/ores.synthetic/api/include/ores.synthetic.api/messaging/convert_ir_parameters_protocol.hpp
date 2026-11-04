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
#ifndef ORES_SYNTHETIC_API_MESSAGING_CONVERT_IR_PARAMETERS_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_CONVERT_IR_PARAMETERS_PROTOCOL_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request the parameters of one view in the terms of the other.
 *
 * Stateless: the caller names the values as they stand on its screen, so an
 * unsaved set converts as readily as a saved one. The simple names are
 * start_rate, long_run_rate, half_life_years and yearly_move_bp.
 */
struct convert_ir_parameters_request {
    using response_type = struct convert_ir_parameters_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.convert_ir_parameters";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The engine the parameters are for, as its code: VASICEK, HULL_WHITE and so on. */
    std::string process_type;
    /** @brief True to read the engine's parameters as the simple view; false for the other way. */
    bool to_simple = false;
    /** @brief The names of the values given; names and values have equal size. */
    std::vector<std::string> parameter_names;
    /** @brief The values given, rates as decimals. */
    std::vector<double> parameter_values;

    friend bool operator==(const convert_ir_parameters_request&,
                           const convert_ir_parameters_request&) = default;
};

/**
 * @brief The converted values, and why there are none when there are none.
 */
struct convert_ir_parameters_response {
    bool success = false;
    std::string message;
    /** @brief False when the parameters given have no simple view; message then says why. */
    bool has_simple_form = false;
    /** @brief A risk the parameters carry, as a code; empty when there is none. */
    std::string caution;
    /** @brief The names of the converted values. */
    std::vector<std::string> parameter_names;
    /** @brief The converted values. */
    std::vector<double> parameter_values;

    friend bool operator==(const convert_ir_parameters_response&,
                           const convert_ir_parameters_response&) = default;
};

/**
 * @brief The members of convert_ir_parameters_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const convert_ir_parameters_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"process_type", &convert_ir_parameters_request::process_type},
        ::ores::utility::codec::field{"to_simple", &convert_ir_parameters_request::to_simple},
        ::ores::utility::codec::field{"parameter_names",
                                      &convert_ir_parameters_request::parameter_names},
        ::ores::utility::codec::field{"parameter_values",
                                      &convert_ir_parameters_request::parameter_values},
    };
}

/**
 * @brief The members of convert_ir_parameters_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const convert_ir_parameters_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &convert_ir_parameters_response::success},
        ::ores::utility::codec::field{"message", &convert_ir_parameters_response::message},
        ::ores::utility::codec::field{"has_simple_form",
                                      &convert_ir_parameters_response::has_simple_form},
        ::ores::utility::codec::field{"caution", &convert_ir_parameters_response::caution},
        ::ores::utility::codec::field{"parameter_names",
                                      &convert_ir_parameters_response::parameter_names},
        ::ores::utility::codec::field{"parameter_values",
                                      &convert_ir_parameters_response::parameter_values},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::convert_ir_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::convert_ir_parameters_request& v);
    static ores::synthetic::messaging::convert_ir_parameters_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::convert_ir_parameters_request& v);
    static ores::synthetic::messaging::convert_ir_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::convert_ir_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::convert_ir_parameters_response& v);
    static ores::synthetic::messaging::convert_ir_parameters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::convert_ir_parameters_response& v);
    static ores::synthetic::messaging::convert_ir_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
