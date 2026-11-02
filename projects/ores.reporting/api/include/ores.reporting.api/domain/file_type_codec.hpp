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
#ifndef ORES_REPORTING_API_DOMAIN_FILE_TYPE_CODEC_HPP
#define ORES_REPORTING_API_DOMAIN_FILE_TYPE_CODEC_HPP

#include "ores.reporting.api/domain/file_type.hpp"
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
 * @brief The members of file_type, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const file_type&) {
    return std::tuple{
        utility::codec::field{"version", &file_type::version},
        utility::codec::field{"tenant_id", &file_type::tenant_id},
        utility::codec::field{"code", &file_type::code},
        utility::codec::field{"analytic_type_code", &file_type::analytic_type_code},
        utility::codec::field{"ore_parameter", &file_type::ore_parameter},
        utility::codec::field{"direction", &file_type::direction},
        utility::codec::field{"format", &file_type::format},
        utility::codec::field{"xsd_file", &file_type::xsd_file},
        utility::codec::field{"kind", &file_type::kind},
        utility::codec::field{"configuration_type_code", &file_type::configuration_type_code},
        utility::codec::field{"name_pattern", &file_type::name_pattern},
        utility::codec::field{"is_default", &file_type::is_default},
        utility::codec::field{"produced_by_file_type_code", &file_type::produced_by_file_type_code},
        utility::codec::field{"sample_count", &file_type::sample_count},
        utility::codec::field{"sample_total", &file_type::sample_total},
        utility::codec::field{"description", &file_type::description},
        utility::codec::field{"display_order", &file_type::display_order},
        utility::codec::field{"modified_by", &file_type::modified_by},
        utility::codec::field{"performed_by", &file_type::performed_by},
        utility::codec::field{"change_reason_code", &file_type::change_reason_code},
        utility::codec::field{"change_commentary", &file_type::change_commentary},
        utility::codec::field{"recorded_at", &file_type::recorded_at},
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
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::domain::file_type> {
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

    static std::string write_json(const ores::reporting::domain::file_type& v);

    static ores::reporting::domain::file_type read_json(std::string_view text);

    static std::string write_msgpack(const ores::reporting::domain::file_type& v);

    static ores::reporting::domain::file_type read_msgpack(std::span<const std::byte> data);
};

}

#endif
