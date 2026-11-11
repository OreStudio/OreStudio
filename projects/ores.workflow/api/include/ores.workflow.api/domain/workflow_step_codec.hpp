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
#ifndef ORES_WORKFLOW_API_DOMAIN_WORKFLOW_STEP_CODEC_HPP
#define ORES_WORKFLOW_API_DOMAIN_WORKFLOW_STEP_CODEC_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.workflow.api/domain/workflow_step.hpp"
#include "ores.workflow.api/export.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::workflow::domain {

/**
 * @brief The members of workflow_step, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &workflow_step::version},
        utility::codec::field{"tenant_id", &workflow_step::tenant_id},
        utility::codec::field{"id", &workflow_step::id},
        utility::codec::field{"workflow_id", &workflow_step::workflow_id},
        utility::codec::field{"step_index", &workflow_step::step_index},
        utility::codec::field{"name", &workflow_step::name},
        utility::codec::field{"state_id", &workflow_step::state_id},
        utility::codec::field{"request_json", &workflow_step::request_json},
        utility::codec::field{"response_json", &workflow_step::response_json},
        utility::codec::field{"error", &workflow_step::error},
        utility::codec::field{"step_log_json", &workflow_step::step_log_json},
        utility::codec::field{"command_subject", &workflow_step::command_subject},
        utility::codec::field{"command_json", &workflow_step::command_json},
        utility::codec::field{"command_published_at", &workflow_step::command_published_at},
        utility::codec::field{"idempotency_key", &workflow_step::idempotency_key},
        utility::codec::field{"compensation_subject", &workflow_step::compensation_subject},
        utility::codec::field{"compensation_json", &workflow_step::compensation_json},
        utility::codec::field{"started_at", &workflow_step::started_at},
        utility::codec::field{"completed_at", &workflow_step::completed_at},
        utility::codec::field{"modified_by", &workflow_step::modified_by},
        utility::codec::field{"performed_by", &workflow_step::performed_by},
        utility::codec::field{"change_reason_code", &workflow_step::change_reason_code},
        utility::codec::field{"change_commentary", &workflow_step::change_commentary},
        utility::codec::field{"recorded_at", &workflow_step::recorded_at},
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
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::domain::workflow_step> {
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

    static std::string write_json(const ores::workflow::domain::workflow_step& v);

    static ores::workflow::domain::workflow_step read_json(std::string_view text);

    static std::string write_msgpack(const ores::workflow::domain::workflow_step& v);

    static ores::workflow::domain::workflow_step read_msgpack(std::span<const std::byte> data);
};

}

#endif
