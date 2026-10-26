/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_XML_EXPORTER_HPP
#define ORES_ORE_CORE_XML_EXPORTER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/messaging/calendar_adjustment_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include <functional>
#include <string>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief Exports domain objects to their ORE XML representation.
 */
class ORES_ORE_CORE_EXPORT exporter {
private:
    inline static std::string_view logger_name = "ores.ore.xml.exporter";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief The ORE name a bond's reference curve is written under, from the
     * oresmd URI the trade states it as.
     *
     * A trade names the curve it is discounted on by the market series' own
     * URI, and ORE resolves that name through the market configuration the run
     * is priced under. The two name the same curve differently, so a run
     * supplies the mapping rather than the trade carrying ORE's spelling: it
     * is a fact about the market the run reads, not about the trade.
     *
     * A curve the map does not hold is written as the trade states it, so a run
     * that supplies no map exports exactly what its trades hold.
     */
    using reference_curve_names = std::function<std::string(const std::string&)>;

    static std::string export_currency_config(const std::vector<refdata::domain::currency>& v);

    /**
     * @brief Writes one pricing engine configuration as the ORE document
     * PricingEngines.
     *
     * The products come first, in the position their rows carry, and each
     * carries its model, its model parameters, its engine and its engine
     * parameters in that order. One GlobalParameters block follows them,
     * holding the parameters no product owns.
     *
     * @param v The configuration's header, products and parameters, as
     *          analytics' own messages answer them.
     * @throws std::runtime_error for a parameter row the document has no place
     * for; see pricing_engine_projection::reverse.
     */
    static std::string
    export_pricing_engines(const analytics::messaging::pricing_engines_document& v);

    static std::string
    export_calendar_adjustments(const std::vector<refdata::messaging::calendar_adjustment>& v);

    static std::string export_conventions(const domain::conventions_package& package);

    /**
     * @brief Reconstructs an ORE portfolio XML from the trades and the
     * instrument batch an export_portfolio_response carries.
     *
     * Each item's instrument is rebuilt from the batch by its trade id and
     * reverse-mapped through the appropriate instrument mapper. Items whose
     * trade has no instrument row, or whose type no reverse mapper handles,
     * are written as their type and envelope alone.
     *
     * @param reference_curves the ORE name of a bond's reference curve, from
     *                        the series URI the trade states. Absent for an
     *                        export that writes the trade's own value.
     */
    static std::string
    export_portfolio(const std::vector<trading::messaging::trade_export_item>& items,
                     const trading::domain::instrument_batch& instruments,
                     const reference_curve_names& reference_curves = {});
};

}

#endif
