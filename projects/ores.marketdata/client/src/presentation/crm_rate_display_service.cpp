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
#include "ores.marketdata.client/presentation/crm_rate_display_service.hpp"

namespace ores::marketdata::client::presentation {

crm_rate_display_service::crm_rate_display_service(rates_fn_t rates_fn, lookup_fn_t lookup_fn)
    : rates_fn_(std::move(rates_fn))
    , lookup_fn_(std::move(lookup_fn)) {}

crm_rate_display_service::result crm_rate_display_service::rates(const std::string& tenant_id,
                                                                 const std::string& party_id,
                                                                 const std::string& crm_name,
                                                                 bool inverted) {
    result out;

    auto fetched = rates_fn_(party_id, crm_name, inverted);
    if (!fetched.success) {
        out.error = fetched.error;
        return out;
    }

    std::vector<crm_rate_format_request> requests;
    requests.reserve(fetched.rates.size());
    for (const auto& item : fetched.rates) {
        crm_rate_format_request req;
        req.item = &item;
        if (const auto direct =
                lookup_fn_(tenant_id, item.base_currency_code + "/" + item.quote_currency_code))
            req.convention = direct;
        else if (const auto reverse = lookup_fn_(
                     tenant_id, item.quote_currency_code + "/" + item.base_currency_code)) {
            req.convention = reverse;
            req.convention_reversed = true;
        }
        requests.push_back(std::move(req));
    }

    const auto displays = crm_rate_formatter::format(requests);

    out.success = true;
    out.rows.reserve(fetched.rates.size());
    for (std::size_t i = 0; i < fetched.rates.size(); ++i) {
        const auto& item = fetched.rates[i];
        row r;
        r.crm_name = item.crm_name;
        r.base_currency_code = item.base_currency_code;
        r.quote_currency_code = item.quote_currency_code;
        r.status = item.status;
        r.inverted = item.inverted;
        r.delta_pct = item.delta_pct;
        r.rate = item.rate;
        r.as_of = item.as_of;
        r.display = displays[i];
        out.rows.push_back(std::move(r));
    }

    return out;
}

}
