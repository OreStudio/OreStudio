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
#ifndef ORES_REPORTING_API_DOMAIN_FILE_TYPE_LOOKUP_HPP
#define ORES_REPORTING_API_DOMAIN_FILE_TYPE_LOOKUP_HPP

#include "ores.reporting.api/domain/file_type.hpp"
#include <algorithm>
#include <string_view>
#include <vector>

namespace ores::reporting::domain {

/**
 * @brief The file type that names the document of a configuration type in the Setup
 * block, or nothing when no file type does.
 *
 * The table holds one such file type for each configuration type, so the answer does
 * not depend on the order of the rows.
 */
inline const file_type* setup_file_type_of(const std::vector<file_type>& types,
                                           std::string_view configuration_type_code) {
    const auto it = std::ranges::find_if(types, [&](const file_type& t) {
        return !t.analytic_type_code && t.configuration_type_code == configuration_type_code;
    });
    return it == types.end() ? nullptr : &*it;
}

}

#endif
