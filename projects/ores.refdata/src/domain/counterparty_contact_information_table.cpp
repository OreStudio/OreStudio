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
#include "ores.refdata/domain/counterparty_contact_information_table.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>

namespace ores::refdata::domain {

std::string convert_to_table(const std::vector<counterparty_contact_information>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Counterparty" << "Type" << "City" << "Country" << "Phone" << "Email" << "Modified By" << "Version" << fort::endr;

    for (const auto& cci : v) {
        table << cci.counterparty_id << cci.contact_type << cci.city << cci.country_code << cci.phone << cci.email << cci.modified_by << cci.version << fort::endr;
    }
    return table.to_string();
}

}
