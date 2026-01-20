/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_MESSAGING_DATASET_DEPENDENCY_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_DATASET_DEPENDENCY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/dataset_dependency.hpp"

namespace ores::dq::messaging {

/**
 * @brief Request to retrieve all dataset dependencies.
 */
struct get_dataset_dependencies_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_dependencies_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_request& v);

/**
 * @brief Response containing all dataset dependencies.
 */
struct get_dataset_dependencies_response final {
    std::vector<domain::dataset_dependency> dependencies;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_dependencies_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_response& v);

/**
 * @brief Request to retrieve dataset dependencies for a specific dataset.
 */
struct get_dataset_dependencies_by_dataset_request final {
    std::string dataset_code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_dependencies_by_dataset_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_by_dataset_request& v);

/**
 * @brief Response containing dataset dependencies for a specific dataset.
 */
struct get_dataset_dependencies_by_dataset_response final {
    std::vector<domain::dataset_dependency> dependencies;

    std::vector<std::byte> serialize() const;
    static std::expected<get_dataset_dependencies_by_dataset_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_by_dataset_response& v);

}

namespace ores::comms::messaging {

template<>
struct message_traits<dq::messaging::get_dataset_dependencies_request> {
    using request_type = dq::messaging::get_dataset_dependencies_request;
    using response_type = dq::messaging::get_dataset_dependencies_response;
    static constexpr message_type request_message_type =
        message_type::get_dataset_dependencies_request;
};

template<>
struct message_traits<dq::messaging::get_dataset_dependencies_by_dataset_request> {
    using request_type = dq::messaging::get_dataset_dependencies_by_dataset_request;
    using response_type = dq::messaging::get_dataset_dependencies_by_dataset_response;
    static constexpr message_type request_message_type =
        message_type::get_dataset_dependencies_by_dataset_request;
};

}

#endif
