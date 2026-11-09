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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_INSTANCE_CODEC_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_INSTANCE_CODEC_HPP

#include "ores.reporting.api/domain/report_instance.hpp"
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
 * @brief The members of report_instance, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_instance>) {
    return std::tuple{
        utility::codec::field{"version", &report_instance::version},
        utility::codec::field{"tenant_id", &report_instance::tenant_id},
        utility::codec::field{"id", &report_instance::id},
        utility::codec::field{"name", &report_instance::name},
        utility::codec::field{"description", &report_instance::description},
        utility::codec::field{"party_id", &report_instance::party_id},
        utility::codec::field{"definition_id", &report_instance::definition_id},
        utility::codec::field{"kind", &report_instance::kind},
        utility::codec::field{"run_grant_id", &report_instance::run_grant_id},
        utility::codec::field{"fsm_state_id", &report_instance::fsm_state_id},
        utility::codec::field{"trigger_id", &report_instance::trigger_id},
        utility::codec::field{"output_message", &report_instance::output_message},
        utility::codec::field{"output_storage_key", &report_instance::output_storage_key},
        utility::codec::field{"started_at", &report_instance::started_at},
        utility::codec::field{"completed_at", &report_instance::completed_at},
        utility::codec::field{"priced_at", &report_instance::priced_at},
        utility::codec::field{"price_note", &report_instance::price_note},
        utility::codec::field{"engine_settings_id", &report_instance::engine_settings_id},
        utility::codec::field{"engine_settings_version", &report_instance::engine_settings_version},
        utility::codec::field{"cut_id", &report_instance::cut_id},
        utility::codec::field{"cut_label", &report_instance::cut_label},
        utility::codec::field{"cut_kind_code", &report_instance::cut_kind_code},
        utility::codec::field{"cut_timepoint", &report_instance::cut_timepoint},
        utility::codec::field{"modified_by", &report_instance::modified_by},
        utility::codec::field{"performed_by", &report_instance::performed_by},
        utility::codec::field{"change_reason_code", &report_instance::change_reason_code},
        utility::codec::field{"change_commentary", &report_instance::change_commentary},
        utility::codec::field{"recorded_at", &report_instance::recorded_at},
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
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::domain::report_instance> {
    /**
     * @brief States that this is a generated codec.
     *
     * A test asserts this rather than the encoding, because the encoding is
     * what the type's own case already round-trips: what it
     * cannot see is whether the seam resolved to this specialisation or fell
     * through to the primary template, and a marker that only this
     * specialisation declares is that answer at compile time.
     */
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::domain::report_instance& v);

    static ores::reporting::domain::report_instance read_json(std::string_view text);

    static std::string write_msgpack(const ores::reporting::domain::report_instance& v);

    static ores::reporting::domain::report_instance read_msgpack(std::span<const std::byte> data);
};

}

#endif
