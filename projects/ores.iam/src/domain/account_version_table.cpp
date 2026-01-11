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
#include "ores.iam/domain/account_version_table.hpp"

#include <fort.hpp>

namespace ores::iam::domain {

std::string convert_to_table(const std::vector<account_version>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Version" << "Username" << "Email" << "Change Reason"
          << "Recorded By" << "Recorded At" << "Change Summary" << fort::endr;

    for (const auto& av : v) {
        table << av.version_number << av.data.username << av.data.email
              << av.data.change_reason_code << av.recorded_by << av.recorded_at
              << av.change_summary << fort::endr;
    }
    return table.to_string();
}

}
