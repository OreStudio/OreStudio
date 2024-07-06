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
#include "ores.utility/filesystem/file.hpp"
#include "ores.ore/xml/xml_importer.hpp"
#include "ores.ore/model/currency_config.hpp"
#include "ores.ore/xml/currency_config_serialiser.hpp"

namespace ores::ore::xml {

model::currency_config
xml_importer::import_currency_config(std::filesystem::path path) {
    currency_config_serialiser ser;
    using namespace ores::utility::filesystem;
    const std::string c(read_file_content(std::move(path)));
    auto r(ser.deserialise(c));
    return r;
}

}
