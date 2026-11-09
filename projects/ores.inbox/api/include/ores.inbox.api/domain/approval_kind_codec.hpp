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
#ifndef ORES_INBOX_API_DOMAIN_APPROVAL_KIND_CODEC_HPP
#define ORES_INBOX_API_DOMAIN_APPROVAL_KIND_CODEC_HPP

#include "ores.inbox.api/domain/approval_kind.hpp"
#include "ores.inbox.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::inbox::domain {

/**
 * @brief The members of approval_kind, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<approval_kind>) {
    return std::tuple{
        utility::codec::field{"version", &approval_kind::version},
        utility::codec::field{"tenant_id", &approval_kind::tenant_id},
        utility::codec::field{"code", &approval_kind::code},
        utility::codec::field{"name", &approval_kind::name},
        utility::codec::field{"description", &approval_kind::description},
        utility::codec::field{"decide_permission_code", &approval_kind::decide_permission_code},
        utility::codec::field{"raise_permission_code", &approval_kind::raise_permission_code},
        utility::codec::field{"allows_hold", &approval_kind::allows_hold},
        utility::codec::field{"comment_on_approve", &approval_kind::comment_on_approve},
        utility::codec::field{"expires_after_days", &approval_kind::expires_after_days},
        utility::codec::field{"apply_workflow_type", &approval_kind::apply_workflow_type},
        utility::codec::field{"apply_window_hours", &approval_kind::apply_window_hours},
        utility::codec::field{"approvals_required", &approval_kind::approvals_required},
        utility::codec::field{"auto_approval_code", &approval_kind::auto_approval_code},
        utility::codec::field{"display_order", &approval_kind::display_order},
        utility::codec::field{"modified_by", &approval_kind::modified_by},
        utility::codec::field{"performed_by", &approval_kind::performed_by},
        utility::codec::field{"change_reason_code", &approval_kind::change_reason_code},
        utility::codec::field{"change_commentary", &approval_kind::change_commentary},
        utility::codec::field{"recorded_at", &approval_kind::recorded_at},
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
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::domain::approval_kind> {
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

    static std::string write_json(const ores::inbox::domain::approval_kind& v);

    static ores::inbox::domain::approval_kind read_json(std::string_view text);

    static std::string write_msgpack(const ores::inbox::domain::approval_kind& v);

    static ores::inbox::domain::approval_kind read_msgpack(std::span<const std::byte> data);
};

}

#endif
