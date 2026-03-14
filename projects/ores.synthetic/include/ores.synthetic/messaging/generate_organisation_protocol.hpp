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

#include <string>
#include <string_view>
#include <cstdint>

namespace ores::synthetic::messaging {

struct generate_organisation_request {
    using response_type = struct generate_organisation_response;
    static constexpr std::string_view nats_subject =
        "ores.synthetic.v1.organisation.generate";
    std::string country;
    std::uint32_t party_count = 10;
    std::uint32_t counterparty_count = 5;
    std::uint32_t portfolio_leaf_count = 3;
    std::uint32_t books_per_leaf_portfolio = 2;
    std::uint32_t business_unit_count = 3;
    bool generate_addresses = true;
    bool generate_identifiers = true;
    std::string published_by;
};

struct generate_organisation_response {
    bool success = false;
    std::string error_message;
    int parties_count = 0;
    int counterparties_count = 0;
    int business_unit_types_count = 0;
    int business_units_count = 0;
    int portfolios_count = 0;
    int books_count = 0;
    int contacts_count = 0;
    int identifiers_count = 0;
};

}

#endif
