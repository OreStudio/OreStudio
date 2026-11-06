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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_TEXT_VALUE_CODEC_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_TEXT_VALUE_CODEC_HPP

#include "ores.reporting.api/domain/report_text_value.hpp"
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
 * @brief The members of report_text_value, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_value>) {
    return std::tuple{
        utility::codec::field{"tenant_id", &report_text_value::tenant_id},
        utility::codec::field{"id", &report_text_value::id},
        utility::codec::field{"party_id", &report_text_value::party_id},
        utility::codec::field{"report_instance_id", &report_text_value::report_instance_id},
        utility::codec::field{"result_id", &report_text_value::result_id},
        utility::codec::field{"source", &report_text_value::source},
        utility::codec::field{"line", &report_text_value::line},
        utility::codec::field{"as_of", &report_text_value::as_of},
        utility::codec::field{"report_definition_id", &report_text_value::report_definition_id},
        utility::codec::field{"family", &report_text_value::family},
        utility::codec::field{"subject_type", &report_text_value::subject_type},
        utility::codec::field{"subject_id", &report_text_value::subject_id},
        utility::codec::field{"name", &report_text_value::name},
        utility::codec::field{"value_type", &report_text_value::value_type},
        utility::codec::field{"value", &report_text_value::value},
        utility::codec::field{"figure", &report_text_value::figure},
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
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::domain::report_text_value> {
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

    static std::string write_json(const ores::reporting::domain::report_text_value& v);

    static ores::reporting::domain::report_text_value read_json(std::string_view text);

    static std::string write_msgpack(const ores::reporting::domain::report_text_value& v);

    static ores::reporting::domain::report_text_value read_msgpack(std::span<const std::byte> data);
};

}

#endif
