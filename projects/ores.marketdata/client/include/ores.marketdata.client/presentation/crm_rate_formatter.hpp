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
#ifndef ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_FORMATTER_HPP
#define ORES_MARKETDATA_CLIENT_PRESENTATION_CRM_RATE_FORMATTER_HPP

#include "ores.marketdata.api/messaging/crm_protocol.hpp"
#include "ores.marketdata.client/export.hpp"
#include "ores.refdata.api/domain/currency_pair_convention.hpp"
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::client::presentation {

/**
 * @brief Ready-to-bind display strings for a single CRM cell. Plain data --
 * consumers (Qt, Wt, shell, HTTP) bind these directly with no further
 * formatting of their own.
 */
struct crm_rate_display {
    std::string rate_text;
    std::string change_text;
    std::string tooltip_text;
};

/**
 * @brief One crm_rate_item paired with its already-resolved convention
 * (std::nullopt when none is available for either pair-code direction),
 * ready to hand to crm_rate_formatter::format() in a batch.
 */
struct crm_rate_format_request {
    const ores::marketdata::messaging::crm_rate_item* item;
    std::optional<ores::refdata::domain::currency_pair_convention> convention;

    /**
     * @brief True when @c convention was resolved against the *reverse* of
     * this item's own base/quote pair-code (e.g. item is USD/EUR but the
     * stored convention is EUR/USD) -- a real market convention only ever
     * quotes one direction of a pair, so the reciprocal direction is
     * always derived, never separately stored. decimal_places/tick_size
     * are calibrated for the convention's own direction and don't
     * transfer as-is to the reciprocal's magnitude; format_rate()
     * re-derives an equivalent precision instead of reusing them blindly.
     */
    bool convention_reversed = false;
};

/**
 * @brief Stateless, batch mapper from raw CRM rates + resolved conventions
 * to display-ready strings. No NATS or Qt/Wt dependency. Batched rather
 * than one call per cell -- a CRM matrix can have hundreds of cells per
 * reload, and formatting them as a single pass avoids per-cell call
 * overhead and lets the output vector be sized once.
 */
class ORES_MARKETDATA_CLIENT_EXPORT crm_rate_formatter final {
public:
    /**
     * @brief Formats a batch of CRM rates, one crm_rate_display per input
     * request, in the same order. Each rate is snapped to its convention's
     * nearest tick (tick_size * pip_factor) before being rendered at
     * decimal_places precision; a request with no convention is shown
     * unsnapped at a fixed default precision. A reversed-convention
     * request (see crm_rate_format_request::convention_reversed) skips
     * tick-snapping and instead derives decimal_places that preserve the
     * same number of significant figures the convention encodes for its
     * own direction.
     */
    static std::vector<crm_rate_display>
    format(const std::vector<crm_rate_format_request>& requests);

private:
    static std::string format_rate(double rate,
        const std::optional<ores::refdata::domain::currency_pair_convention>& convention,
        bool convention_reversed);
};

}

#endif
