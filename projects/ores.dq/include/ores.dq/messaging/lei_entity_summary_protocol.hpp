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
#ifndef ORES_DQ_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP
#define ORES_DQ_MESSAGING_LEI_ENTITY_SUMMARY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/message_traits.hpp"

namespace ores::dq::messaging {

/**
 * @brief Summary of an LEI entity from the DQ staging table.
 */
struct lei_entity_summary final {
    std::string lei;
    std::string entity_legal_name;
    std::string country;
    std::string entity_category;

    std::vector<std::byte> serialize() const;
    static std::expected<lei_entity_summary, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte>& data);
};

std::ostream& operator<<(std::ostream& s, const lei_entity_summary& v);

/**
 * @brief Request to retrieve LEI entity summaries from staging data.
 *
 * Used by the Qt UI to populate the LEI entity picker.
 */
struct get_lei_entities_summary_request final {
    /**
     * @brief Optional filter string. If non-empty, filters by LEI or name.
     */
    std::string search_filter;

    std::vector<std::byte> serialize() const;
    static std::expected<get_lei_entities_summary_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_lei_entities_summary_request& v);

/**
 * @brief Response containing LEI entity summaries.
 */
struct get_lei_entities_summary_response final {
    bool success = false;
    std::string error_message;
    std::vector<lei_entity_summary> entities;

    std::vector<std::byte> serialize() const;
    static std::expected<get_lei_entities_summary_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_lei_entities_summary_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits for get_lei_entities_summary_request.
 */
template<>
struct message_traits<dq::messaging::get_lei_entities_summary_request> {
    using request_type = dq::messaging::get_lei_entities_summary_request;
    using response_type = dq::messaging::get_lei_entities_summary_response;
    static constexpr message_type request_message_type =
        message_type::get_lei_entities_summary_request;
};

}

#endif
