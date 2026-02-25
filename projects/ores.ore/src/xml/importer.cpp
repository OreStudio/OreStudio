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
#include "ores.ore/xml/importer.hpp"

#include <sstream>
#include "ores.platform/filesystem/file.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.ore/domain/domain.hpp"
#include "ores.ore/domain/currency_mapper.hpp"
#include "ores.ore/domain/trade_mapper.hpp"

namespace ores::ore::xml {

using refdata::domain::currency;
using trading::domain::trade;
using namespace ores::logging;

std::string importer::validate_currency(const currency& c) {
    std::ostringstream errors;

    // Required fields per XSD
    if (c.name.empty())
        errors << "Name is required\n";

    if (c.iso_code.empty())
        errors << "ISO code is required\n";

    // if (c.symbol.empty())
    //     errors << "Symbol is required\n";

    // if (c.fraction_symbol.empty())
    //     errors << "Fraction symbol is required\n";

    if (c.fractions_per_unit <= 0)
        errors << "Fractions per unit must be positive\n";

    if (c.rounding_type.empty())
        errors << "Rounding type is required\n";

    if (c.rounding_precision < 0)
        errors << "Rounding precision must be non-negative\n";

    return errors.str();
}

std::vector<currency>
importer::import_currency_config(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started import: " << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::currencyConfig ccy_cfg;
    domain::load_data(c, ccy_cfg);
    const auto r = domain::currency_mapper::map(ccy_cfg);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size()
                               << " currencies. Result: " << r;

    return r;
}

std::string importer::validate_trade(const trade& t) {
    std::ostringstream errors;

    if (t.external_id.empty())
        errors << "External ID is required\n";

    if (t.trade_type.empty())
        errors << "Trade type is required\n";

    return errors.str();
}

std::vector<trade>
importer::import_portfolio(const std::filesystem::path& path) {
    BOOST_LOG_SEV(lg(), debug) << "Started portfolio import: "
                               << path.generic_string();

    using namespace ores::platform::filesystem;
    const std::string c(file::read_content(path));
    BOOST_LOG_SEV(lg(), trace) << "File content: " << c;

    domain::portfolio p;
    domain::load_data(c, p);
    const auto r = domain::trade_mapper::map(p);

    BOOST_LOG_SEV(lg(), debug) << "Finished importing " << r.size()
                               << " trades.";

    return r;
}

}
