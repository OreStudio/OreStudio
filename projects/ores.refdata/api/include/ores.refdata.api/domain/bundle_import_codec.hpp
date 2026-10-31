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
#ifndef ORES_REFDATA_API_DOMAIN_BUNDLE_IMPORT_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_BUNDLE_IMPORT_CODEC_HPP

#include "ores.refdata.api/domain/bundle_import.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::refdata::domain {

/**
 * @brief The members of bundle_import, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const bundle_import&) {
    return std::tuple{
        utility::codec::field{"version", &bundle_import::version},
        utility::codec::field{"tenant_id", &bundle_import::tenant_id},
        utility::codec::field{"id", &bundle_import::id},
        utility::codec::field{"party_id", &bundle_import::party_id},
        utility::codec::field{"source", &bundle_import::source},
        utility::codec::field{"state", &bundle_import::state},
        utility::codec::field{"families_expected", &bundle_import::families_expected},
        utility::codec::field{"expires_at", &bundle_import::expires_at},
        utility::codec::field{"modified_by", &bundle_import::modified_by},
        utility::codec::field{"performed_by", &bundle_import::performed_by},
        utility::codec::field{"change_reason_code", &bundle_import::change_reason_code},
        utility::codec::field{"change_commentary", &bundle_import::change_commentary},
        utility::codec::field{"recorded_at", &bundle_import::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::bundle_import> {
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

    static std::string write_json(const ores::refdata::domain::bundle_import& v);

    static ores::refdata::domain::bundle_import read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::bundle_import& v);

    static ores::refdata::domain::bundle_import read_msgpack(std::span<const std::byte> data);
};

}

#endif
