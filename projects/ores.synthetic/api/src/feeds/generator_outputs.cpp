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
#include "ores.synthetic.api/feeds/generator_outputs.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_emitter.hpp"
#include "ores.synthetic.api/feeds/index_fixings.hpp"
#include "ores.synthetic.api/feeds/ir_curve_template_resolver.hpp"
#include <algorithm>
#include <format>
#include <stdexcept>

namespace ores::synthetic::feed {

namespace datum = ores::marketdata::datum;

std::expected<std::string, std::string>
ir_pillar_quote_uri(const ores::marketdata::core::pillar_quote& pillar) {
    const auto made = [&pillar]() -> std::expected<datum::market_datum, std::string> {
        try {
            return ores::marketdata::core::pillar_quote_datum(pillar);
        } catch (const std::invalid_argument& e) {
            return std::unexpected(e.what());
        }
    }();
    if (!made)
        return std::unexpected("the pillar " + pillar.start_tenor_code + " to " +
                               pillar.end_tenor_code + " names no datum: " + made.error());
    return datum::oresmd_uri_codec::write(*made);
}

std::expected<std::vector<std::string>, std::string>
ir_curve_outputs(const domain::ir_curve_generation_config& cfg,
                 const std::vector<domain::ir_curve_template_entry>& entries) {
    const auto index_tenor = ir_curve_index_tenor(cfg);
    std::vector<std::string> out;
    for (const auto& e : entries) {
        if (e.ir_curve_config_id != cfg.id ||
            ores::marketdata::core::pillar_quote_needs_dates(e.start_tenor_code, e.end_tenor_code))
            continue;
        auto uri = ir_pillar_quote_uri({.currency_code = cfg.currency_code,
                                        .index_tenor = index_tenor,
                                        .start_tenor_code = e.start_tenor_code,
                                        .end_tenor_code = e.end_tenor_code});
        if (!uri)
            return std::unexpected(uri.error());
        if (std::ranges::find(out, *uri) == out.end())
            out.push_back(std::move(*uri));
    }
    if (const auto fixing = curve_fixing_uri(cfg))
        out.push_back(*fixing);
    return out;
}

std::expected<std::string, std::string> fx_spot_quote_uri(const std::string& quote_uri) {
    const auto read = datum::oresmd_uri_codec::read(quote_uri);
    if (!read)
        return std::unexpected("'" + quote_uri + "' names no datum: " + read.error());
    if (read->type() != datum::instrument_type::fx_spot || read->is_series())
        return std::unexpected("'" + quote_uri + "' is not an fx_spot quote.");
    return datum::oresmd_uri_codec::write(*read);
}

std::string fx_vol_point_uri(std::string_view base,
                             std::string_view quote,
                             std::string_view expiry,
                             std::string_view label) {
    return std::format("oresmd://fx/{}?type=quote&instrument=fx_option&quote=rate_lnvol&"
                       "ccy={}&expiry={}&strike_label={}",
                       base,
                       quote,
                       expiry,
                       label);
}

std::vector<std::string> fx_vol_surface_outputs(std::string_view base,
                                                std::string_view quote,
                                                const std::vector<std::string>& expiries,
                                                std::span<const double> deltas) {
    std::vector<std::string> out;
    out.reserve(expiries.size() * (1 + 2 * deltas.size()));
    for (const auto& expiry : expiries) {
        out.push_back(fx_vol_point_uri(base, quote, expiry, fx_vol_atm_label));
        for (const double delta : deltas)
            for (const bool butterfly : {false, true})
                out.push_back(
                    fx_vol_point_uri(base, quote, expiry, fx_vol_point_label(delta, butterfly)));
    }
    return out;
}

}
