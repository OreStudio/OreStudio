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
#include "ores.utility/codec/wire_traits.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief The wire form of dataset_dependency, in each format the wire states.
 *
 * This type is hand-written, so its codec is too, and it states the same four
 * functions the generated ones do -- a member list held in the helper, and one
 * definition per format over it. A type the generator does not own has to look
 * indistinguishable from one it does, or every caller would need to know which
 * is which.
 */
struct ORES_DQ_API_EXPORT dataset_dependency_codec final {
    /**
     * @brief Writes @p v as JSON: an object whose members are the members of
     * the type.
     */
    static std::string to_json(const dataset_dependency& v);

    /**
     * @brief Reads a dataset_dependency back out of its JSON form.
     */
    static dataset_dependency from_json(std::string_view text);

    /**
     * @brief Writes @p v as MessagePack: a map whose keys are the members of
     * the type.
     */
    static std::string to_msgpack(const dataset_dependency& v);

    /**
     * @brief Reads a dataset_dependency back out of its MessagePack form.
     */
    static dataset_dependency from_msgpack(std::span<const std::byte> data);
};

}

namespace ores::utility::codec {

/**
 * @brief The hand-written codec, which is how the wire codec reaches this
 * type.
 */
template <>
struct wire_traits<ores::dq::domain::dataset_dependency> {
    /**
     * @brief States that this type's codec is its own, not the reflector's.
     */
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::domain::dataset_dependency& v) {
        return ores::dq::domain::dataset_dependency_codec::to_json(v);
    }

    static ores::dq::domain::dataset_dependency read_json(std::string_view text) {
        return ores::dq::domain::dataset_dependency_codec::from_json(text);
    }

    static std::string write_msgpack(const ores::dq::domain::dataset_dependency& v) {
        return ores::dq::domain::dataset_dependency_codec::to_msgpack(v);
    }

    static ores::dq::domain::dataset_dependency read_msgpack(std::span<const std::byte> data) {
        return ores::dq::domain::dataset_dependency_codec::from_msgpack(data);
    }
};

}

#endif
