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
#ifndef ORES_DQ_MESSAGING_CATALOG_DEPENDENCY_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_CATALOG_DEPENDENCY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.dq/domain/catalog_dependency.hpp"

namespace ores::dq::messaging {

/**
 * @brief Request to retrieve all catalog dependencies.
 */
struct get_catalog_dependencies_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_catalog_dependencies_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_request& v);

/**
 * @brief Response containing all catalog dependencies.
 */
struct get_catalog_dependencies_response final {
    std::vector<domain::catalog_dependency> dependencies;

    std::vector<std::byte> serialize() const;
    static std::expected<get_catalog_dependencies_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_response& v);

/**
 * @brief Request to retrieve catalog dependencies for a specific catalog.
 */
struct get_catalog_dependencies_by_catalog_request final {
    std::string catalog_name;

    std::vector<std::byte> serialize() const;
    static std::expected<get_catalog_dependencies_by_catalog_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_by_catalog_request& v);

/**
 * @brief Response containing catalog dependencies for a specific catalog.
 */
struct get_catalog_dependencies_by_catalog_response final {
    std::vector<domain::catalog_dependency> dependencies;

    std::vector<std::byte> serialize() const;
    static std::expected<get_catalog_dependencies_by_catalog_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_by_catalog_response& v);

}

namespace ores::comms::messaging {

template<>
struct message_traits<dq::messaging::get_catalog_dependencies_request> {
    using request_type = dq::messaging::get_catalog_dependencies_request;
    using response_type = dq::messaging::get_catalog_dependencies_response;
    static constexpr message_type request_message_type =
        message_type::get_catalog_dependencies_request;
};

template<>
struct message_traits<dq::messaging::get_catalog_dependencies_by_catalog_request> {
    using request_type = dq::messaging::get_catalog_dependencies_by_catalog_request;
    using response_type = dq::messaging::get_catalog_dependencies_by_catalog_response;
    static constexpr message_type request_message_type =
        message_type::get_catalog_dependencies_by_catalog_request;
};

}

#endif
