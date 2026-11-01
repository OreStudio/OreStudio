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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ASSETS_API_MESSAGING_PUBLISH_OPERATION_PROTOCOL_HPP
#define ORES_ASSETS_API_MESSAGING_PUBLISH_OPERATION_PROTOCOL_HPP

#include "ores.assets.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::assets::messaging {

/**
 * @brief The workflow step that publishes the DQ-cleared images.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_assets_publish_images_from_dq_fn.
 */
struct publish_images_from_dq_request {
    static constexpr std::string_view nats_subject = "assets.v1.ops.publish_images_from_dq";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The members of publish_images_from_dq_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const publish_images_from_dq_request&) {
    return std::tuple{};
}

}
namespace ores::utility::codec {

template <>
struct ORES_ASSETS_API_EXPORT wire_traits<ores::assets::messaging::publish_images_from_dq_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::assets::messaging::publish_images_from_dq_request& v);
    static ores::assets::messaging::publish_images_from_dq_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::assets::messaging::publish_images_from_dq_request& v);
    static ores::assets::messaging::publish_images_from_dq_request
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
