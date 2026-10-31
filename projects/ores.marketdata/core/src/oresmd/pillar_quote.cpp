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
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include <format>
#include <stdexcept>
#include <vector>

namespace ores::marketdata::core {

namespace {

namespace d = datum;

// A tenor a curve template names its pillar by: a count of days, weeks, months or years,
// such as 1D, 3M or 2Y. A date is not one, and neither is a meeting code such as 1F.
bool named_as_a_period(const std::string& code) {
    if (code.size() < 2 || code.size() > 4)
        return false;
    if (code.front() < '1' || code.front() > '9')
        return false;
    for (std::size_t i = 1; i + 1 < code.size(); ++i)
        if (code[i] < '0' || code[i] > '9')
            return false;
    const auto unit = code.back();
    return unit == 'D' || unit == 'W' || unit == 'M' || unit == 'Y';
}

std::string date_text(std::chrono::year_month_day date) {
    return std::format("{:%Y%m%d}", std::chrono::sys_days{date});
}

// The start slot of the swap quote: spot for a spot-starting pillar, the pillar's own
// period for one that starts later, and the resolved start date for a meeting-dated one.
// The slot holds a period or a date and never one of each, so the test asks both ends.
std::string start_slot_text(const pillar_quote& quote) {
    if (quote.start_tenor_code == "SPOT")
        return "0D";
    if (named_as_a_period(quote.start_tenor_code) && named_as_a_period(quote.end_tenor_code))
        return quote.start_tenor_code;
    return date_text(quote.start_date);
}

}

std::string pillar_term_text(const std::string& start_tenor_code,
                             const std::string& end_tenor_code,
                             std::chrono::year_month_day start_date,
                             std::chrono::year_month_day end_date) {
    if (start_tenor_code == "SPOT") {
        if (!end_tenor_code.empty())
            return end_tenor_code;
        const auto days = std::chrono::sys_days{end_date} - std::chrono::sys_days{start_date};
        if (days.count() <= 0)
            throw std::invalid_argument(std::format(
                "pillar quote: a spot pillar must end after spot, but {:%Y-%m-%d} is not after "
                "{:%Y-%m-%d}",
                std::chrono::sys_days{end_date},
                std::chrono::sys_days{start_date}));
        return std::format("{}D", days.count());
    }
    if (named_as_a_period(start_tenor_code) && named_as_a_period(end_tenor_code))
        return end_tenor_code;
    return date_text(end_date);
}

std::expected<datum::market_datum, std::string> pillar_quote_datum(const pillar_quote& quote) {
    auto start = d::parse_value(d::field::fwd_start, start_slot_text(quote));
    if (!start)
        return std::unexpected("pillar quote: the start slot: " + start.error());
    auto tenor = d::parse_value(d::field::tenor, quote.index_tenor);
    if (!tenor)
        return std::unexpected("pillar quote: the index tenor: " + tenor.error());

    auto term = d::parse_value(
        d::field::term,
        pillar_term_text(
            quote.start_tenor_code, quote.end_tenor_code, quote.start_date, quote.end_date));
    if (!term)
        return std::unexpected("pillar quote: the term: " + term.error());

    std::vector<d::field_value> fields;
    fields.push_back({d::field::ccy, quote.currency_code});
    fields.push_back({d::field::index_name, d::none});
    fields.push_back({d::field::fwd_start, std::move(*start)});
    fields.push_back({d::field::tenor, std::move(*tenor)});
    fields.push_back({d::field::term, std::move(*term)});
    return d::market_datum::make(
        d::instrument_type::ir_swap, d::quote_type::rate, std::move(fields));
}

}
