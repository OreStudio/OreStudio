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
#ifndef ORES_DQ_API_DOMAIN_DATASET_DEPENDENCY_CODEC_HPP
#define ORES_DQ_API_DOMAIN_DATASET_DEPENDENCY_CODEC_HPP

#include "ores.dq.api/domain/dataset_dependency.hpp"
#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::dq::domain {


/**
 * @brief The members of dataset_dependency, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the type's shape, the same one the generated
 * types make.
 */
[[nodiscard]] constexpr auto wire_fields(const dataset_dependency&) {
    return std::tuple{
        utility::codec::field{"tenant_id", &dataset_dependency::tenant_id},
        utility::codec::field{"dataset_code", &dataset_dependency::dataset_code},
        utility::codec::field{"dependency_code", &dataset_dependency::dependency_code},
        utility::codec::field{"role", &dataset_dependency::role},
        utility::codec::field{"modified_by", &dataset_dependency::modified_by},
        utility::codec::field{"performed_by", &dataset_dependency::performed_by},
        utility::codec::field{"recorded_at", &dataset_dependency::recorded_at}};
}

}

namespace ores::utility::codec {

/**
 * @brief The hand-written codec, which is how the wire codec reaches this
 * type.
 */
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::domain::dataset_dependency> {
    /**
     * @brief States that this type's codec is its own, not the reflector's.
     */
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::domain::dataset_dependency& v);

    static ores::dq::domain::dataset_dependency read_json(std::string_view text);

    static std::string write_msgpack(const ores::dq::domain::dataset_dependency& v);

    static ores::dq::domain::dataset_dependency read_msgpack(std::span<const std::byte> data);
};

}

#endif
