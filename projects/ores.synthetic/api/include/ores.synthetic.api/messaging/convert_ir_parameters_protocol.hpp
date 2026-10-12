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

#include <string>
#include <string_view>
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
};

}

#endif
