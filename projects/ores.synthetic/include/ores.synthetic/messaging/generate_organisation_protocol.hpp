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
#ifndef ORES_SYNTHETIC_MESSAGING_GENERATE_ORGANISATION_PROTOCOL_HPP
#define ORES_SYNTHETIC_MESSAGING_GENERATE_ORGANISATION_PROTOCOL_HPP

#include <cstdint>
#include <expected>
#include <iosfwd>
#include <optional>
#include <span>
#include <string>
#include <vector>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::synthetic::messaging {

/**
 * @brief Request to generate a synthetic organisation.
 *
 * Carries all generation options from the client to the server.
 * The server will generate the organisation and persist it in one step.
 */
struct generate_organisation_request final {
    std::string country = "GB";
    std::uint32_t party_count = 5;
    std::uint32_t party_max_depth = 3;
    std::uint32_t counterparty_count = 10;
    std::uint32_t counterparty_max_depth = 3;
    std::uint32_t portfolio_leaf_count = 8;
    std::uint32_t portfolio_max_depth = 4;
    std::uint32_t books_per_leaf_portfolio = 2;
    std::uint32_t business_unit_count = 10;
    std::uint32_t business_unit_max_depth = 3;
    bool generate_addresses = true;
    bool generate_identifiers = true;
    std::uint32_t contacts_per_party = 2;
    std::uint32_t contacts_per_counterparty = 1;
    std::optional<std::uint64_t> seed;
    std::string published_by;

    std::vector<std::byte> serialize() const;
    static std::expected<generate_organisation_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const generate_organisation_request& v);

/**
 * @brief Response from organisation generation.
 *
 * Reports success/failure and counts of entities created.
 */
struct generate_organisation_response final {
    bool success = false;
    std::string error_message;
    std::uint32_t parties_count = 0;
    std::uint32_t counterparties_count = 0;
    std::uint32_t portfolios_count = 0;
    std::uint32_t books_count = 0;
    std::uint32_t business_units_count = 0;
    std::uint32_t contacts_count = 0;
    std::uint32_t identifiers_count = 0;

    std::vector<std::byte> serialize() const;
    static std::expected<generate_organisation_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const generate_organisation_response& v);

}

namespace ores::comms::messaging {

template<>
struct message_traits<synthetic::messaging::generate_organisation_request> {
    using request_type = synthetic::messaging::generate_organisation_request;
    using response_type = synthetic::messaging::generate_organisation_response;
    static constexpr message_type request_message_type =
        message_type::generate_organisation_request;
};

}

#endif
