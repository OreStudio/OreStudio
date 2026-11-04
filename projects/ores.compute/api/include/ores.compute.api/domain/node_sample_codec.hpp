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
#ifndef ORES_COMPUTE_API_DOMAIN_NODE_SAMPLE_CODEC_HPP
#define ORES_COMPUTE_API_DOMAIN_NODE_SAMPLE_CODEC_HPP

#include "ores.compute.api/domain/node_sample.hpp"
#include "ores.compute.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::compute::domain {

/**
 * @brief The members of node_sample, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the type's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message whose
 * member is this type encodes it as its own object without knowing the type.
 */
[[nodiscard]] constexpr auto wire_fields(const node_sample&) {
    return std::tuple{
        utility::codec::field{"tenant_id", &node_sample::tenant_id},
        utility::codec::field{"id", &node_sample::id},
        utility::codec::field{"sampled_at", &node_sample::sampled_at},
        utility::codec::field{"host_id", &node_sample::host_id},
        utility::codec::field{"machine", &node_sample::machine},
        utility::codec::field{"tasks_completed", &node_sample::tasks_completed},
        utility::codec::field{"tasks_failed", &node_sample::tasks_failed},
        utility::codec::field{"tasks_since_last", &node_sample::tasks_since_last},
        utility::codec::field{"avg_task_duration_ms", &node_sample::avg_task_duration_ms},
        utility::codec::field{"max_task_duration_ms", &node_sample::max_task_duration_ms},
        utility::codec::field{"input_bytes_fetched", &node_sample::input_bytes_fetched},
        utility::codec::field{"output_bytes_uploaded", &node_sample::output_bytes_uploaded},
        utility::codec::field{"seconds_since_hb", &node_sample::seconds_since_hb},
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
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::domain::node_sample> {
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

    static std::string write_json(const ores::compute::domain::node_sample& v);

    static ores::compute::domain::node_sample read_json(std::string_view text);

    static std::string write_msgpack(const ores::compute::domain::node_sample& v);

    static ores::compute::domain::node_sample read_msgpack(std::span<const std::byte> data);
};

}

#endif
