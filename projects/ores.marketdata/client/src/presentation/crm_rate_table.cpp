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
#include "ores.marketdata.client/presentation/crm_rate_table.hpp"
#include <fort.hpp>
#include <map>
#include <set>

namespace ores::marketdata::client::presentation {

std::string convert_to_table(const std::vector<crm_rate_display_service::row>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "CRM" << "Pair" << "Rate" << "Change" << "Status" << "As Of"
          << fort::endr;

    for (const auto& r : v) {
        table << r.crm_name << r.base_currency_code + "/" + r.quote_currency_code
              << r.display.rate_text << r.display.change_text << r.status << r.as_of
              << fort::endr;
    }
    return table.to_string();
}

std::string convert_to_matrix_table(const std::vector<crm_rate_display_service::row>& v) {
    // Row/column headers are exactly the currencies that actually appear
    // -- not a fixed/curated list -- mirroring the Qt matrix window's own
    // rule (see CrmCrossRatesMatrixMdiWindow).
    std::set<std::string> currencies;
    for (const auto& r : v) {
        currencies.insert(r.base_currency_code);
        currencies.insert(r.quote_currency_code);
    }

    std::map<std::pair<std::string, std::string>, const crm_rate_display_service::row*> cells;
    for (const auto& r : v)
        cells[{r.base_currency_code, r.quote_currency_code}] = &r;

    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "";
    for (const auto& quote : currencies)
        table << quote;
    table << fort::endr;

    for (const auto& base : currencies) {
        table << base;
        for (const auto& quote : currencies) {
            if (base == quote) {
                table << "-";
                continue;
            }
            const auto it = cells.find({base, quote});
            table << (it == cells.end() ? "" : it->second->display.rate_text);
        }
        table << fort::endr;
    }
    return table.to_string();
}

}
