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
#ifndef ORES_WORKFLOW_API_DOMAIN_WORKFLOW_INSTANCE_CODEC_HPP
#define ORES_WORKFLOW_API_DOMAIN_WORKFLOW_INSTANCE_CODEC_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.workflow.api/domain/workflow_instance.hpp"
#include "ores.workflow.api/export.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::workflow::domain {

/**
 * @brief The members of workflow_instance, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance>) {
    return std::tuple{
        utility::codec::field{"version", &workflow_instance::version},
        utility::codec::field{"tenant_id", &workflow_instance::tenant_id},
        utility::codec::field{"id", &workflow_instance::id},
        utility::codec::field{"type", &workflow_instance::type},
        utility::codec::field{"target_kind", &workflow_instance::target_kind},
        utility::codec::field{"target_id", &workflow_instance::target_id},
        utility::codec::field{"state_id", &workflow_instance::state_id},
        utility::codec::field{"request_json", &workflow_instance::request_json},
        utility::codec::field{"result_json", &workflow_instance::result_json},
        utility::codec::field{"error", &workflow_instance::error},
        utility::codec::field{"correlation_id", &workflow_instance::correlation_id},
        utility::codec::field{"created_by", &workflow_instance::created_by},
        utility::codec::field{"step_count", &workflow_instance::step_count},
        utility::codec::field{"completed_at", &workflow_instance::completed_at},
        utility::codec::field{"last_event_at", &workflow_instance::last_event_at},
        utility::codec::field{"modified_by", &workflow_instance::modified_by},
        utility::codec::field{"performed_by", &workflow_instance::performed_by},
        utility::codec::field{"change_reason_code", &workflow_instance::change_reason_code},
        utility::codec::field{"change_commentary", &workflow_instance::change_commentary},
        utility::codec::field{"recorded_at", &workflow_instance::recorded_at},
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
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::domain::workflow_instance> {
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

    static std::string write_json(const ores::workflow::domain::workflow_instance& v);

    static ores::workflow::domain::workflow_instance read_json(std::string_view text);

    static std::string write_msgpack(const ores::workflow::domain::workflow_instance& v);

    static ores::workflow::domain::workflow_instance read_msgpack(std::span<const std::byte> data);
};

}

#endif
