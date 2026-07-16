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
#ifndef ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_DISPLAY_SERVICE_HPP
#define ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_DISPLAY_SERVICE_HPP

#include "ores.marketdata.client/crm_client.hpp"
#include "ores.marketdata.client/export.hpp"
#include "ores.marketdata.client/presentation/crm_rate_formatter.hpp"
#include "ores.refdata.api/domain/currency_pair_convention.hpp"
#include <functional>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::client::presentation {

/**
 * @brief Facade over a CRM rates source + a convention lookup +
 * crm_rate_formatter -- the single place that knows how to turn "give me
 * a party's CRM rates" into ready-to-bind display rows. Every UI (Qt, Wt,
 * shell, HTTP) calls the same rates() and gets identical formatting for
 * free, rather than each re-deriving convention lookup + formatting
 * itself, as the pre-facade Qt window used to.
 *
 * Takes its two dependencies as std::function seams rather than concrete
 * crm_client/currency_pair_convention_cache references, matching this
 * codebase's token_provider idiom: production code wires real NATS-backed
 * calls, tests wire canned data -- so this facade's own orchestration
 * (batching, direct/reverse pair-code lookup fallback) is Catch2-testable
 * with no NATS involved. Only crm_client/currency_pair_convention_cache
 * themselves need a live NATS connection to test.
 */
class ORES_MARKETDATA_CLIENT_EXPORT crm_rate_display_service {
public:
    using rates_fn_t =
        std::function<crm_client::rates_result(const std::string&, const std::string&, bool)>;
    using lookup_fn_t = std::function<std::optional<ores::refdata::domain::currency_pair_convention>(
        const std::string&, const std::string&)>;

    crm_rate_display_service(rates_fn_t rates_fn, lookup_fn_t lookup_fn);

    /// One CRM cell, fully ready to bind -- the grid-placement fields
    /// (base/quote/status/inverted) a UI needs alongside the formatted
    /// display strings.
    struct row {
        std::string base_currency_code;
        std::string quote_currency_code;
        std::string status;
        bool inverted = false;
        std::optional<double> delta_pct;
        double rate = 0.0;
        std::string as_of;
        crm_rate_display display;
    };

    struct result {
        bool success = false;
        std::string error;
        std::vector<row> rows;
    };

    /**
     * @brief Fetches a party's CRM rates (one round trip via rates_fn_t),
     * resolves each row's convention against lookup_fn_t (direct
     * pair-code, then reversed -- a convention may only be stored in one
     * direction), and formats the whole batch in a single
     * crm_rate_formatter::format() call.
     */
    [[nodiscard]] result rates(const std::string& tenant_id, const std::string& party_id,
        const std::string& crm_name, bool inverted);

private:
    rates_fn_t rates_fn_;
    lookup_fn_t lookup_fn_;
};

}

#endif
