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
#include <optional>
#include <string>

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
 * @brief Stateless mapper from a raw crm_rate_item to display-ready strings.
 * No NATS or Qt/Wt dependency -- the caller resolves the convention (via
 * ores.refdata.client's currency_pair_convention_cache, trying both
 * pair-code directions since a convention may only be stored in one) and
 * passes its decimal_places down.
 */
class ORES_MARKETDATA_CLIENT_EXPORT crm_rate_formatter final {
public:
    /**
     * @brief Formats one CRM rate. decimal_places comes from the resolved
     * currency_pair_convention; pass std::nullopt when no convention is
     * available for either direction of the pair, in which case a fixed
     * default precision is used.
     */
    static crm_rate_display
    format(const ores::marketdata::messaging::crm_rate_item& item,
        std::optional<int> decimal_places);

private:
    static std::string format_rate(double rate, int decimal_places);
};

}

#endif
