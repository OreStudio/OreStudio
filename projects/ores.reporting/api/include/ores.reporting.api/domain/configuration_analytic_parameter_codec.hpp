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
 * Template: cpp_domain_type_codec.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_API_DOMAIN_CONFIGURATION_ANALYTIC_PARAMETER_CODEC_HPP
#define ORES_REPORTING_API_DOMAIN_CONFIGURATION_ANALYTIC_PARAMETER_CODEC_HPP

#include "ores.reporting.api/domain/configuration_analytic_parameter.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::reporting::domain {

/**
 * @brief The members of configuration_analytic_parameter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const configuration_analytic_parameter&) {
    return std::tuple{
        utility::codec::field{"version", &configuration_analytic_parameter::version},
        utility::codec::field{"tenant_id", &configuration_analytic_parameter::tenant_id},
        utility::codec::field{"id", &configuration_analytic_parameter::id},
        utility::codec::field{"party_id", &configuration_analytic_parameter::party_id},
        utility::codec::field{"configuration_analytic_id",
                              &configuration_analytic_parameter::configuration_analytic_id},
        utility::codec::field{"ore_parameter_id",
                              &configuration_analytic_parameter::ore_parameter_id},
        utility::codec::field{"value", &configuration_analytic_parameter::value},
        utility::codec::field{"position", &configuration_analytic_parameter::position},
        utility::codec::field{"modified_by", &configuration_analytic_parameter::modified_by},
        utility::codec::field{"performed_by", &configuration_analytic_parameter::performed_by},
        utility::codec::field{"change_reason_code",
                              &configuration_analytic_parameter::change_reason_code},
        utility::codec::field{"change_commentary",
                              &configuration_analytic_parameter::change_commentary},
        utility::codec::field{"recorded_at", &configuration_analytic_parameter::recorded_at},
    };
}

}

namespace ores::utility::codec {

/**
 * @brief The generated codec: what the wire codec resolves to for this type.
 *
 * The functions are declared here and defined in this type's own translation
 * unit, so a translation unit that only sends the type compiles a declaration
 * rather than a second copy of the encoder.
 */
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::domain::configuration_analytic_parameter> {
    /**
     * @brief States that this is a generated codec.
     *
     * A test asserts this rather than the encoding, because the encoding is
     * what the type's own case already compares with the reflector: what it
     * cannot see is whether the seam resolved to this specialisation or fell
     * through to the primary template, and a marker that only this
     * specialisation declares is that answer at compile time.
     */
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::domain::configuration_analytic_parameter& v);

    static ores::reporting::domain::configuration_analytic_parameter
    read_json(std::string_view text);

    static std::string
    write_msgpack(const ores::reporting::domain::configuration_analytic_parameter& v);

    static ores::reporting::domain::configuration_analytic_parameter
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
