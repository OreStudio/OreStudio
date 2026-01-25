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
#include "ores.ore/domain/CurrencyConfig.hpp"

#include <rfl.hpp>
#include <rfl/xml.hpp>
#include <rfl/json.hpp>

namespace ores::ore::domain {

std::string CurrencyConfig::to_xml(const CurrencyConfig& v) {
    return rfl::xml::write(v);
}

CurrencyConfig CurrencyConfig::from_xml(const std::string& xml) {
    return rfl::xml::read<CurrencyConfig>(xml).value();
}

std::ostream& operator<<(std::ostream& s, const CurrencyConfig& v) {
    rfl::json::write(v, s);
    return s;
}

}
