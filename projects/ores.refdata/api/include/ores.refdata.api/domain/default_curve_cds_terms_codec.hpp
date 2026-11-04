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
#ifndef ORES_REFDATA_API_DOMAIN_DEFAULT_CURVE_CDS_TERMS_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_DEFAULT_CURVE_CDS_TERMS_CODEC_HPP

#include "ores.refdata.api/domain/default_curve_cds_terms.hpp"
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
 * @brief The members of default_curve_cds_terms, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const default_curve_cds_terms&) {
    return std::tuple{
        utility::codec::field{"version", &default_curve_cds_terms::version},
        utility::codec::field{"tenant_id", &default_curve_cds_terms::tenant_id},
        utility::codec::field{"id", &default_curve_cds_terms::id},
        utility::codec::field{"party_id", &default_curve_cds_terms::party_id},
        utility::codec::field{"default_curve_configuration_id",
                              &default_curve_cds_terms::default_curve_configuration_id},
        utility::codec::field{"start_date", &default_curve_cds_terms::start_date},
        utility::codec::field{"running_spread", &default_curve_cds_terms::running_spread},
        utility::codec::field{"index_term", &default_curve_cds_terms::index_term},
        utility::codec::field{"imply_default_from_market",
                              &default_curve_cds_terms::imply_default_from_market},
        utility::codec::field{"price_is_upfront", &default_curve_cds_terms::price_is_upfront},
        utility::codec::field{"modified_by", &default_curve_cds_terms::modified_by},
        utility::codec::field{"performed_by", &default_curve_cds_terms::performed_by},
        utility::codec::field{"change_reason_code", &default_curve_cds_terms::change_reason_code},
        utility::codec::field{"change_commentary", &default_curve_cds_terms::change_commentary},
        utility::codec::field{"recorded_at", &default_curve_cds_terms::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::default_curve_cds_terms> {
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

    static std::string write_json(const ores::refdata::domain::default_curve_cds_terms& v);

    static ores::refdata::domain::default_curve_cds_terms read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::default_curve_cds_terms& v);

    static ores::refdata::domain::default_curve_cds_terms
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
