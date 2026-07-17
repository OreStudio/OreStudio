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
#ifndef ORES_MARKETDATA_CLIENT_CRM_CLIENT_HPP
#define ORES_MARKETDATA_CLIENT_CRM_CLIENT_HPP

#include "ores.marketdata.api/messaging/crm_protocol.hpp"
#include "ores.marketdata.client/export.hpp"
#include "ores.nats/service/client.hpp"
#include <functional>
#include <string>
#include <vector>

namespace ores::marketdata::client {

/**
 * @brief Typed, authenticated facade over the CRM cross-rates NATS request
 * (@c marketdata.v1.crm.rates), so callers never hand-roll request_sync +
 * rfl::json + auth headers themselves -- the "underlying CRM" the
 * presentation::crm_rate_display_service facade delegates to.
 *
 * Mirrors ores.refdata.client::currency_pair_convention_cache's token
 * plumbing: pass a @c token_provider so every request attaches a fresh
 * Bearer token (a service-account provider, or a GUI session's snapshot
 * token -- see CrmCrossRatesMatrixMdiWindow for the latter).
 */
class ORES_MARKETDATA_CLIENT_EXPORT crm_client {
public:
    explicit crm_client(ores::nats::service::client& nats,
                        std::function<std::string(bool)> token_provider = nullptr);

    struct rates_result {
        bool success = false;
        std::string error;
        std::vector<messaging::crm_rate_item> rates;
    };

    /**
     * @brief Fetches every currently-configured CRM rate for a party in one
     * NATS round trip. crm_name empty selects every enabled CRM.
     */
    [[nodiscard]] rates_result
    rates(const std::string& party_id, const std::string& crm_name, bool inverted);

private:
    ores::nats::service::client& nats_;
    std::function<std::string(bool)> token_provider_;
};

}

#endif
