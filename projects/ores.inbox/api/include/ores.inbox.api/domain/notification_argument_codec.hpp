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
#ifndef ORES_INBOX_API_DOMAIN_NOTIFICATION_ARGUMENT_CODEC_HPP
#define ORES_INBOX_API_DOMAIN_NOTIFICATION_ARGUMENT_CODEC_HPP

#include "ores.inbox.api/domain/notification_argument.hpp"
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
 * @brief The members of notification_argument, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<notification_argument>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &notification_argument::version},
        utility::codec::field{"tenant_id", &notification_argument::tenant_id},
        utility::codec::field{"notification_id", &notification_argument::notification_id},
        utility::codec::field{"name", &notification_argument::name},
        utility::codec::field{"value", &notification_argument::value},
        utility::codec::field{"modified_by", &notification_argument::modified_by},
        utility::codec::field{"performed_by", &notification_argument::performed_by},
        utility::codec::field{"change_reason_code", &notification_argument::change_reason_code},
        utility::codec::field{"change_commentary", &notification_argument::change_commentary},
        utility::codec::field{"recorded_at", &notification_argument::recorded_at},
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
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::domain::notification_argument> {
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

    static std::string write_json(const ores::inbox::domain::notification_argument& v);

    static ores::inbox::domain::notification_argument read_json(std::string_view text);

    static std::string write_msgpack(const ores::inbox::domain::notification_argument& v);

    static ores::inbox::domain::notification_argument read_msgpack(std::span<const std::byte> data);
};

}

#endif
