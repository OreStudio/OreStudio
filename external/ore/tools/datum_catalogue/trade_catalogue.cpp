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
#include <boost/core/demangle.hpp>
#include <iostream>
#include <ored/portfolio/scriptedtrade.hpp>
#include <ored/portfolio/tradefactory.hpp>
#include <ored/utilities/databuilders.hpp>
#include <string>
#include <typeinfo>

/**
 * @file trade_catalogue.cpp
 * @brief Prints every trade type ORE registers and the class it builds.
 *
 * ORE reads a trade type through the builder its trade factory holds for it.
 * The class that builder makes decides how ORE reads the product: a class
 * derived from ScriptedTrade prices it through a script, any other class
 * natively. Asking the factory is truer than reading the registrations as text,
 * because it is what the engine does.
 *
 * One line per registered type, sorted by type: the type, a tab, the class's
 * qualified name, a tab, and "scripted" or "native". A type whose builder
 * throws prints ORE's error in place of the class.
 *
 *   trade_catalogue
 *   Accumulator	ore::data::Accumulator	scripted
 */

int main() {
    ore::data::dataBuilders();
    const auto& factory = ore::data::TradeFactory::instance();
    for (const auto& [type, builder] : factory.getBuilders()) {
        std::cout << type;
        try {
            const auto trade = factory.build(type);
            const auto& object = *trade;
            const bool scripted =
                QuantLib::ext::dynamic_pointer_cast<ore::data::ScriptedTrade>(trade) != nullptr;
            std::cout << '\t' << boost::core::demangle(typeid(object).name()) << '\t'
                      << (scripted ? "scripted" : "native");
        } catch (const std::exception& e) {
            std::cout << "\t!" << e.what() << '\t';
        }
        std::cout << '\n';
    }
    return 0;
}
