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

#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.ore.core/domain/pricing_engine_projection.hpp"
#include "ores.ore.core/export.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/messaging/calendar_adjustment_protocol.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include <string>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief One pricing engines document as the projection holds it: the rows of a
 * configuration regrouped into the shape the document states.
 *
 * It is the type pricing_engine_projection::reverse answers with and that
 * pricing_engine_projection::write takes, so a caller that holds rows and one
 * that holds a document reach the same writer.
 */
using pricing_engines_document = domain::pricing_engine_projection::document;

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
    static std::string export_currency_config(const std::vector<refdata::domain::currency>& v);

    /**
     * @brief Writes a pricing engines document as the ORE document ORE reads.
     *
     * The products come first, in the order the document holds them, and each
     * carries its model, its model parameters, its engine and its engine
     * parameters in that order. One GlobalParameters block follows them,
     * holding the parameters no product owns. An empty section is written as
     * an empty element, because the document states every section.
     *
     * This is the one writer of this document: both entry points below reach
     * it, and pricing_engine_projection::write delegates to it, so the export
     * and the round trip cannot drift apart.
     *
     * @param v The document, as pricing_engine_projection::reverse regroups the
     *          rows into it, or as a reader bound it.
     */
    static std::string export_pricing_engines(const pricing_engines_document& v);

    /**
     * @brief Writes one pricing engine configuration as the ORE document
     * PricingEngines.
     *
     * The thin caller of the writer above: it regroups the configuration's rows
     * with pricing_engine_projection::reverse, which also refuses a parameter
     * row the document has no place for.
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
     * @param projection writes the oresmd identifier a trade holds as ORE
     * names it. A reference that is not an address is written as the trade
     * states it, and so is a curve this projection gives no ORE name, so a
     * default projection writes every curve as its trade holds it.
     */
    static std::string
    export_portfolio(const std::vector<trading::messaging::trade_export_item>& items,
                     const trading::domain::instrument_batch& instruments,
                     const ores::marketdata::projection::ore_projection& projection = {});

    /**
     * @brief Reconstructs an ORE portfolio XML as export_portfolio does, leaving
     * out each trade whose XML cannot be written and naming it.
     *
     * A run prices what it can: one trade the writer refuses would otherwise
     * refuse the whole portfolio, and with it every other trade of the run.
     */
    static domain::written_portfolio export_portfolio_leaving_out(
        const std::vector<trading::messaging::trade_export_item>& items,
        const trading::domain::instrument_batch& instruments,
        const ores::marketdata::projection::ore_projection& projection = {});
};

}

#endif
