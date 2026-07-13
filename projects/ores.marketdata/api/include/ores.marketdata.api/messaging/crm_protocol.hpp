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
#ifndef ORES_MARKETDATA_API_MESSAGING_CRM_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_CRM_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One CRM rate: direct or triangulated, indistinguishable to the
 * caller -- see ores.analytics.quant::domain::derived_rate.
 */
struct crm_rate_item {
    std::string base_currency_code;
    std::string quote_currency_code;
    double rate = 0.0;
    /// "fresh" | "stale" | "unavailable" -- see
    /// ores.analytics.quant::domain::rate_status.
    std::string status;
    /// ISO-8601 UTC, empty when status is "unavailable".
    std::string as_of;
};

/**
 * @brief Request a single CRM rate (direct or derived) for a party.
 *
 * Pull-only, computed on demand from that party's live rate_engine --
 * see the CRM story's architecture decision to never broadcast the full
 * derived set. party_id is explicit (not derived from session) because
 * a tenant may have many parties and no single "current party" concept
 * exists at the request-context layer today.
 */
struct get_crm_rate_request {
    using response_type = struct get_crm_rate_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm.rate";
    std::string party_id;
    std::string base_currency_code;
    std::string quote_currency_code;
};

struct get_crm_rate_response {
    bool success = true;
    std::string message;
    crm_rate_item rate;
};

/**
 * @brief Request every currently-configured CRM rate for a party in one
 * call -- the union of that party's enabled driver pairs and enabled
 * derived pairs, resolved via a single rate_engine::rates() batch (one
 * atomic snapshot load, not N single-pair round trips).
 */
struct get_crm_rates_request {
    using response_type = struct get_crm_rates_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm.rates";
    std::string party_id;
};

struct get_crm_rates_response {
    bool success = true;
    std::string message;
    std::vector<crm_rate_item> rates;
};

} // namespace ores::marketdata::messaging

#endif
