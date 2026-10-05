/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.ore.core/xml/importer.hpp"
#include "ores.ore.core/domain/calendar_adjustment_mapper.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/currency_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/nil_generator.hpp>
#include <sstream>

namespace ores::ore::xml {

using refdata::domain::currency;
using namespace ores::logging;

std::string importer::validate_currency(const currency& c) {
    std::ostringstream errors;

    // Required fields per XSD
    if (c.name.empty())
        errors << "Name is required\n";

    if (c.iso_code.empty())
        errors << "ISO code is required\n";

    // Symbol and FractionSymbol are required elements in the XSD but may be
    // empty, and ORE's own example files ship them empty, so emptiness is
    // not an error.

    if (c.fractions_per_unit <= 0)
        errors << "Fractions per unit must be positive\n";

    if (c.rounding_type.empty())
        errors << "Rounding type is required\n";

    if (c.rounding_precision < 0)
        errors << "Rounding precision must be non-negative\n";

    return errors.str();
}

std::vector<currency> importer::import_currency_config(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::currencyConfig ccy_cfg;
    domain::load_data(c, ccy_cfg);
    const auto r = domain::currency_mapper::map(ccy_cfg);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size() << " currencies. Result: " << r;

    return r;
}

std::string
importer::validate_calendar_adjustment(const refdata::messaging::calendar_adjustment& ca) {
    std::ostringstream errors;

    if (ca.calendar_name.empty())
        errors << "Calendar name is required\n";

    return errors.str();
}

std::vector<refdata::messaging::calendar_adjustment>
importer::import_calendar_adjustments(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::calendaradjustment ca;
    domain::load_data(c, ca);
    const auto r = domain::calendar_adjustment_mapper::map(ca);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size() << " calendar adjustments.";
    return r;
}

std::vector<trade_import_item>
importer::import_portfolio_with_context(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started portfolio import with context: "
                               << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::portfolio p;
    domain::load_data(c, p);

    std::vector<trade_import_item> r;
    r.reserve(p.Trade.size());

    for (const auto& t : p.Trade) {
        const auto nil = boost::uuids::nil_uuid();
        trade_import_item item;
        item.ore_id = std::string(t.id);
        item.anchor.id = nil;
        item.anchor.party_id = nil;
        item.anchor.trade_type = to_string(t.TradeType);
        item.booking.trade_id = nil;
        item.booking.party_id = nil;
        item.booking.book_id = nil;
        item.envelope = domain::trade_mapper::map_envelope(t);
        item.source_file = path;

        try {
            item.instrument = domain::trade_mapper::map_instrument(t);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to map instrument for trade " << std::string(t.id) << ": " << e.what();
        }

        r.push_back(std::move(item));
    }

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size() << " trades with context.";
    return r;
}


ores::refdata::messaging::conventions_document
importer::import_conventions(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::conventions conv;
    domain::load_data(c, conv);
    const auto r = domain::conventions_mapper::map(conv);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing conventions from " << path.generic_string();
    return r;
}

}
