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
#ifndef ORES_REFDATA_API_MESSAGING_TENANT_PARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENANT_PARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/party.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief One page of the current parties of one named tenant.
 *
 * Refused to a caller outside the system tenant. The system tenant itself is
 * not a tenant this read answers for.
 */
struct list_tenant_parties_request {
    using response_type = struct list_tenant_parties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.parties.list-of-tenant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The id of the tenant whose parties are read.
     */
    std::string tenant_id;
    /**
     * @brief How many parties to skip.
     */
    std::uint32_t offset = 0;
    /**
     * @brief The most parties to return. The handler caps it at 1000.
     */
    std::uint32_t limit = 100;
};

struct list_tenant_parties_response {
    bool success = false;
    /**
     * @brief Why the read failed, or empty.
     */
    std::string message;
    /**
     * @brief The page of the tenant's current parties.
     */
    std::vector<ores::refdata::domain::party> parties;
    /**
     * @brief How many current parties the tenant holds in all.
     */
    std::uint64_t total = 0;
};

}

#endif
