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
#ifndef ORES_SYNTHETIC_API_FEEDS_DERIVED_FX_VOL_HPP
#define ORES_SYNTHETIC_API_FEEDS_DERIVED_FX_VOL_HPP

#include "ores.analytics.quant/math/fx_vol_derivation.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.refdata.api/domain/fx_option_convention.hpp"
#include "ores.synthetic.api/export.hpp"
#include <array>
#include <chrono>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::feed {

/// The derived source's own notation: its scheme, and the field of every quote.
inline constexpr std::string_view derived_fx_id_scheme = "derived_fx_vol";
inline constexpr std::string_view derived_fx_field = "rate_lnvol";

/// A pair the derived source quotes, in its market base/quote order.
struct derived_fx_pair final {
    std::string base;
    std::string quote;

    friend bool operator==(const derived_fx_pair&, const derived_fx_pair&) = default;
};

/// An expiry the derived source quotes: its tenor label and its length in years.
struct derived_fx_expiry final {
    std::string_view label;
    double years;
};

inline constexpr std::array<derived_fx_expiry, 8> derived_fx_expiries{{{"1W", 7.0 / 365.0},
                                                                       {"1M", 1.0 / 12.0},
                                                                       {"2M", 2.0 / 12.0},
                                                                       {"3M", 0.25},
                                                                       {"6M", 0.5},
                                                                       {"9M", 0.75},
                                                                       {"1Y", 1.0},
                                                                       {"2Y", 2.0}}};

/// The labels of an expiry's quotes, the strike_label of their fx_option items.
inline constexpr std::array<std::string_view, 5> derived_fx_labels{
    "ATM", "25RR", "25BF", "10RR", "10BF"};

/// One quote the derived source publishes: a pair, an expiry and a label.
struct derived_fx_quote_id final {
    derived_fx_pair pair;
    std::string expiry;
    std::string label;

    friend bool operator==(const derived_fx_quote_id&, const derived_fx_quote_id&) = default;
};

/// The vendor id of a quote, such as EURUSD/1Y/25RR.
ORES_SYNTHETIC_API_EXPORT std::string derived_fx_external_id(const derived_fx_quote_id& id);

/// The quote a vendor id names, or nothing for an id outside the derived source's notation.
ORES_SYNTHETIC_API_EXPORT std::optional<derived_fx_quote_id>
parse_derived_fx_external_id(std::string_view external_id);

/// The id of a pair's fx_option_convention in refdata, such as EUR-USD-FXOPTION.
ORES_SYNTHETIC_API_EXPORT std::string fx_option_convention_id(const derived_fx_pair& pair);

/// The ECB reference fixing item of the euro in a currency.
ORES_SYNTHETIC_API_EXPORT std::string ecb_fixing_item(std::string_view currency);

/**
 * @brief The ECB fixings a pair's log returns are read from.
 *
 * The ECB quotes every currency against the euro, so base/quote is EUR/quote over
 * EUR/base and its log return is the return of the first less that of the second. A
 * side that is the euro itself has no leg. The legs are inputs to our computation;
 * the pair's returns are never presented as an ECB statistic.
 */
struct pair_legs final {
    std::optional<std::string> plus;
    std::optional<std::string> minus;
};

ORES_SYNTHETIC_API_EXPORT pair_legs ecb_legs_of(const derived_fx_pair& pair);

/// A fixing series as the reader returns it: dates and values in step.
struct dated_series final {
    std::vector<std::chrono::year_month_day> dates;
    std::vector<double> values;
};

/// A pair's daily log returns, oldest first, and the last date they reach.
struct pair_returns final {
    std::vector<double> returns;
    std::optional<std::chrono::year_month_day> last;
};

/**
 * @brief The pair's daily log returns, over the dates every leg holds.
 *
 * A date one leg lacks is skipped in both, so a cross never mixes two days. A value
 * that is not positive is skipped as no observation. Refuses a pair with no leg.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<pair_returns, std::string>
pair_log_returns(const std::optional<dated_series>& plus, const std::optional<dated_series>& minus);

/**
 * @brief The quoting convention of an expiry of a pair.
 *
 * An expiry at or past the convention's switch tenor uses its long-term ATM and delta
 * types. ORE's spellings are read: AtmDeltaNeutral and AtmFwd; Spot, Fwd, PaSpot and
 * PaFwd; a risk reversal in favour of Call or Put, Call when unstated; a Broker or Smile
 * butterfly, Smile when unstated. Refuses a type the derivation cannot quote.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<analytics::quant::math::fx_quote_convention, std::string>
quote_convention_at(const refdata::domain::fx_option_convention& convention, double years);

/**
 * @brief The ticks of a pair's derived quotes on a day, one for each label of each
 * expiry the derivation could quote, stamped at the end of that day. A quote that is
 * not finite is left out rather than sent.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<marketdata::messaging::adapter_tick>
derived_fx_ticks(const derived_fx_pair& pair,
                 const analytics::quant::math::fx_derivation& derivation,
                 std::chrono::year_month_day day,
                 std::uint64_t first_sequence);

}

#endif
