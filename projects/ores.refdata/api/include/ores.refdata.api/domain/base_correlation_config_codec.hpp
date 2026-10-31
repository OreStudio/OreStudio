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
#ifndef ORES_REFDATA_API_DOMAIN_BASE_CORRELATION_CONFIG_CODEC_HPP
#define ORES_REFDATA_API_DOMAIN_BASE_CORRELATION_CONFIG_CODEC_HPP

#include "ores.refdata.api/domain/base_correlation_config.hpp"
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
 * @brief The members of base_correlation_config, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const base_correlation_config&) {
    return std::tuple{
        utility::codec::field{"version", &base_correlation_config::version},
        utility::codec::field{"tenant_id", &base_correlation_config::tenant_id},
        utility::codec::field{"id", &base_correlation_config::id},
        utility::codec::field{"party_id", &base_correlation_config::party_id},
        utility::codec::field{"curve_definition_id", &base_correlation_config::curve_definition_id},
        utility::codec::field{"terms", &base_correlation_config::terms},
        utility::codec::field{"detachment_points", &base_correlation_config::detachment_points},
        utility::codec::field{"settlement_days", &base_correlation_config::settlement_days},
        utility::codec::field{"calendar", &base_correlation_config::calendar},
        utility::codec::field{"business_day_convention",
                              &base_correlation_config::business_day_convention},
        utility::codec::field{"day_counter", &base_correlation_config::day_counter},
        utility::codec::field{"extrapolate", &base_correlation_config::extrapolate},
        utility::codec::field{"quote_name", &base_correlation_config::quote_name},
        utility::codec::field{"start_date", &base_correlation_config::start_date},
        utility::codec::field{"rule", &base_correlation_config::rule},
        utility::codec::field{"adjust_for_losses", &base_correlation_config::adjust_for_losses},
        utility::codec::field{"index_term", &base_correlation_config::index_term},
        utility::codec::field{"index_spread", &base_correlation_config::index_spread},
        utility::codec::field{"currency", &base_correlation_config::currency},
        utility::codec::field{"calibrate_constituents_to_index_spread",
                              &base_correlation_config::calibrate_constituents_to_index_spread},
        utility::codec::field{"use_assumed_recovery",
                              &base_correlation_config::use_assumed_recovery},
        utility::codec::field{"modified_by", &base_correlation_config::modified_by},
        utility::codec::field{"performed_by", &base_correlation_config::performed_by},
        utility::codec::field{"change_reason_code", &base_correlation_config::change_reason_code},
        utility::codec::field{"change_commentary", &base_correlation_config::change_commentary},
        utility::codec::field{"recorded_at", &base_correlation_config::recorded_at},
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
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::domain::base_correlation_config> {
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

    static std::string write_json(const ores::refdata::domain::base_correlation_config& v);

    static ores::refdata::domain::base_correlation_config read_json(std::string_view text);

    static std::string write_msgpack(const ores::refdata::domain::base_correlation_config& v);

    static ores::refdata::domain::base_correlation_config
    read_msgpack(std::span<const std::byte> data);
};

}

#endif
