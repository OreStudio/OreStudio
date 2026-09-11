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
#include "ores.synthetic.api/domain/fx_spot_generation_config_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::synthetic::domain {


std::string convert_to_table(const std::vector<fx_spot_generation_config>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Base" << "Quote" << "Source" << "ORE Key" << "Price Source"
          << "Ticks/Hour" << "Enabled" << "Modified By" << "Version" << fort::endr;

    for ([[maybe_unused]] const auto& c : v) {
        table << c.base_currency_code << c.quote_currency_code << c.source_name << c.ore_key
              << c.price_source << c.ticks_per_hour << (c.enabled ? "true" : "false")
              << c.modified_by << c.version << fort::endr;
    }
    return table.to_string();
}

}
