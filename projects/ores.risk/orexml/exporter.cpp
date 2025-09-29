/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.utility/log/logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.risk/orexml/CurrencyConfig.hpp"
#include "ores.risk/orexml/currency_mapper.hpp"
#include "ores.risk/orexml/exporter.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.xml.exporter"));

}

namespace ores::risk::orexml {

using domain::currency;

std::string
exporter::export_currency_config(const std::vector<currency>& v) {
    BOOST_LOG_SEV(lg, debug) << "Started import. Total: " << v.size();
    BOOST_LOG_SEV(lg, trace) << "Currencies: " << v;

    const auto mapped = currency_mapper::map(v);
    std::string r = CurrencyConfig::to_xml(mapped);
    BOOST_LOG_SEV(lg, trace) << "XML: " << v;

    BOOST_LOG_SEV(lg, debug) << "Finished importing. Result: " << r;
    return r;
}

}
