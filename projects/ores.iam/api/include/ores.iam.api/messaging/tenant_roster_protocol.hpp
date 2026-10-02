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
#ifndef ORES_IAM_API_MESSAGING_TENANT_ROSTER_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_ROSTER_PROTOCOL_HPP

#include "ores.iam.api/domain/tenant.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief One page of the tenants that match a search and two filters.
 *
 * The system tenant is never in the answer. The page is in code order, and the
 * answer carries how many tenants match in all.
 */
struct search_tenants_request {
    using response_type = struct search_tenants_response;
    static constexpr std::string_view nats_subject = "iam.v1.tenants.search";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Text matched, without regard to case, anywhere in the code, the name
     * or the hostname. Empty matches every tenant.
     */
    std::string search = {};
    /**
     * @brief A tenant type code to keep. Empty keeps every type.
     */
    std::string type_filter = {};
    /**
     * @brief A tenant status code to keep. Empty keeps every status.
     */
    std::string status_filter = {};
    /**
     * @brief How many matching tenants to skip.
     */
    std::uint32_t offset = 0;
    /**
     * @brief The most tenants to return. The handler caps it at 1000.
     */
    std::uint32_t limit = 100;
};

struct search_tenants_response {
    bool success = false;
    /**
     * @brief Why the search failed, or empty.
     */
    std::string message;
    /**
     * @brief The page of matching tenants, in code order.
     */
    std::vector<ores::iam::domain::tenant> tenants;
    /**
     * @brief How many tenants match in all, across every page.
     */
    std::uint64_t total = 0;
};

}

#endif
