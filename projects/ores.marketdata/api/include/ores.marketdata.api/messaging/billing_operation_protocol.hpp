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
#ifndef ORES_MARKETDATA_API_MESSAGING_BILLING_OPERATION_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_BILLING_OPERATION_PROTOCOL_HPP

#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One component of a rate card, the quantity it counted and what it costs.
 */
struct bill_line_view {
    std::string driver;
    std::string licence_right;
    std::int64_t quantity = 0;
    std::int64_t charge_micros = 0;
    std::int64_t total_micros = 0;
};

/**
 * @brief The bill of a source.
 *
 * A shadow bill is the bill of a simulated source. It has the same lines and the same amounts as a
 * bill of a live source, and nobody pays it.
 */
struct source_bill_view {
    boost::uuids::uuid source_id;
    std::string source_name;
    std::string currency;
    bool shadow = false;
    std::vector<bill_line_view> lines;
    std::int64_t total_micros = 0;
};

/**
 * @brief The charge of a party for a component of a source.
 */
struct party_charge_view {
    std::string tenant_id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid source_id;
    std::string driver;
    std::string licence_right;
    std::int64_t total_micros = 0;
};

/**
 * @brief Asks for the bills of a half open period.
 *
 * A period of no length, or one that ends before it starts, is refused.
 */
struct compute_bill_request {
    using response_type = struct compute_bill_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.compute_bill";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::chrono::system_clock::time_point period_from;
    std::chrono::system_clock::time_point period_to;
    std::string source_name;
};

/**
 * @brief The source bills, the charge of each party and the part that no party bears.
 */
struct compute_bill_response {
    std::vector<source_bill_view> source_bills;
    std::vector<party_charge_view> party_charges;
    std::int64_t unallocated_micros = 0;
    bool success = false;
    std::string message;
};

}

#endif
