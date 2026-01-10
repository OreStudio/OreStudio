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
#ifndef ORES_IAM_MESSAGING_CHANGE_MANAGEMENT_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_CHANGE_MANAGEMENT_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/change_reason_category.hpp"
#include "ores.iam/domain/change_reason.hpp"

namespace ores::iam::messaging {

/**
 * @brief Request to retrieve all change reason categories.
 */
struct get_change_reason_categories_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_categories_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_categories_request& v);

/**
 * @brief Response containing all change reason categories.
 */
struct get_change_reason_categories_response final {
    std::vector<domain::change_reason_category> categories;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reason_categories_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reason_categories_response& v);

/**
 * @brief Request to retrieve all change reasons.
 */
struct get_change_reasons_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_request& v);

/**
 * @brief Response containing all change reasons.
 */
struct get_change_reasons_response final {
    std::vector<domain::change_reason> reasons;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_response& v);

/**
 * @brief Request to retrieve change reasons for a specific category.
 */
struct get_change_reasons_by_category_request final {
    std::string category_code;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_by_category_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_by_category_request& v);

/**
 * @brief Response containing change reasons for a category.
 */
struct get_change_reasons_by_category_response final {
    std::vector<domain::change_reason> reasons;

    std::vector<std::byte> serialize() const;
    static std::expected<get_change_reasons_by_category_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s,
    const get_change_reasons_by_category_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for get_change_reason_categories_request.
 */
template<>
struct message_traits<iam::messaging::get_change_reason_categories_request> {
    using request_type = iam::messaging::get_change_reason_categories_request;
    using response_type = iam::messaging::get_change_reason_categories_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reason_categories_request;
};

/**
 * @brief Message traits specialization for get_change_reasons_request.
 */
template<>
struct message_traits<iam::messaging::get_change_reasons_request> {
    using request_type = iam::messaging::get_change_reasons_request;
    using response_type = iam::messaging::get_change_reasons_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reasons_request;
};

/**
 * @brief Message traits specialization for get_change_reasons_by_category_request.
 */
template<>
struct message_traits<iam::messaging::get_change_reasons_by_category_request> {
    using request_type = iam::messaging::get_change_reasons_by_category_request;
    using response_type = iam::messaging::get_change_reasons_by_category_response;
    static constexpr message_type request_message_type =
        message_type::get_change_reasons_by_category_request;
};

}

#endif
