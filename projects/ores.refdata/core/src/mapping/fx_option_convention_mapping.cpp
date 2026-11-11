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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.refdata.core/mapping/fx_option_convention_mapping.hpp"
#include <charconv>

namespace ores::refdata::mapping {

namespace qd = analytics::quant::domain;
namespace qm = analytics::quant::math;

std::expected<qm::fx_atm_type, std::string> parse_fx_atm_type_code(std::string_view code) {
    if (code == "AtmDeltaNeutral")
        return qm::fx_atm_type::delta_neutral;
    if (code == "AtmFwd")
        return qm::fx_atm_type::forward;
    return std::unexpected("the ATM type '" + std::string(code) +
                           "' is not AtmDeltaNeutral or AtmFwd");
}

std::expected<qm::fx_delta_type, std::string> parse_fx_delta_type_code(std::string_view code) {
    if (code == "Spot")
        return qm::fx_delta_type::spot;
    if (code == "Fwd")
        return qm::fx_delta_type::forward;
    if (code == "PaSpot")
        return qm::fx_delta_type::premium_adjusted_spot;
    if (code == "PaFwd")
        return qm::fx_delta_type::premium_adjusted_forward;
    return std::unexpected("the delta type '" + std::string(code) +
                           "' is not Spot, Fwd, PaSpot or PaFwd");
}

std::expected<qd::date_period, std::string> parse_tenor_text(std::string_view text) {
    const auto refused = [&] {
        return std::unexpected("'" + std::string(text) + "' is not a tenor such as 1W, 6M or 2Y");
    };
    if (text.size() < 2)
        return refused();
    int count = 0;
    const auto digits_end = text.data() + text.size() - 1;
    const auto [end, ec] = std::from_chars(text.data(), digits_end, count);
    if (ec != std::errc{} || end != digits_end || count < 0)
        return refused();
    switch (text.back()) {
        case 'D':
            return qd::date_period{qd::period_unit::day, count};
        case 'W':
            return qd::date_period{qd::period_unit::week, count};
        case 'M':
            return qd::date_period{qd::period_unit::month, count};
        case 'Y':
            return qd::date_period{qd::period_unit::year, count};
        default:
            return refused();
    }
}

std::expected<qm::fx_quote_convention, std::string>
to_quote_convention(const domain::fx_option_convention& convention, qd::date_period expiry) {
    std::string_view atm = convention.atm_type;
    std::string_view delta = convention.delta_type;
    if (convention.switch_tenor) {
        const auto switch_at = parse_tenor_text(*convention.switch_tenor);
        if (!switch_at)
            return std::unexpected("the switch tenor of " + convention.id + ": " + switch_at.error());
        if (switch_at->multiplier > 0) {
            const auto order = qd::compare_periods(expiry, *switch_at);
            if (order == std::partial_ordering::unordered)
                return std::unexpected("the expiry cannot be ordered against the switch tenor " +
                                       *convention.switch_tenor + " of " + convention.id);
            if (order >= 0) {
                if (convention.long_term_atm_type)
                    atm = *convention.long_term_atm_type;
                if (convention.long_term_delta_type)
                    delta = *convention.long_term_delta_type;
            }
        }
    }
    const auto atm_type = parse_fx_atm_type_code(atm);
    if (!atm_type)
        return std::unexpected(atm_type.error());
    const auto delta_type = parse_fx_delta_type_code(delta);
    if (!delta_type)
        return std::unexpected(delta_type.error());
    const std::string favour = convention.risk_reversal_in_favor_of.value_or("Call");
    if (favour != "Call" && favour != "Put")
        return std::unexpected("a risk reversal is in favour of Call or Put, not '" + favour + "'");
    const std::string style = convention.butterfly_style.value_or("Smile");
    if (style != "Broker" && style != "Smile")
        return std::unexpected("a butterfly is Broker or Smile, not '" + style + "'");
    return qm::fx_quote_convention{.atm = *atm_type,
                                   .delta = *delta_type,
                                   .favour = favour == "Call" ? qm::fx_risk_reversal_favour::call :
                                                                qm::fx_risk_reversal_favour::put,
                                   .butterfly = style == "Broker" ? qm::fx_butterfly_style::broker :
                                                                    qm::fx_butterfly_style::smile};
}

}
