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

/// The kind of file that holds a run's own data rather than a configuration.
inline constexpr std::string_view data_file_kind = "data_file";

/// The kind of file that is one ORE document: the file ORE reads as a whole.
inline constexpr std::string_view input_document_kind = "input_document";

/// The direction of a file the run reads.
inline constexpr std::string_view input_direction = "input";

/**
 * @brief The file type that names the document a configuration type is written
 * as: the one that is the configuration's whole ORE document (RS-ORE-01).
 *
 * A configuration type may name several files, so the row that is the default
 * answers, and a row that is not the default answers only when no row is. The
 * row names the document through its =name_pattern=, which is the name an
 * export writes the file under.
 */
inline const file_type* input_document_file_type_of(const std::vector<file_type>& types,
                                                    std::string_view configuration_type_code) {
    const file_type* found = nullptr;
    for (const auto& type : types) {
        if (type.kind != input_document_kind || type.direction != input_direction)
            continue;
        if (type.configuration_type_code != configuration_type_code)
            continue;
        if (type.is_default)
            return &type;
        if (found == nullptr)
            found = &type;
    }
    return found;
}

/**
 * @brief The file types that name the run's own data files in the Setup block: the
 * portfolio, the market data and the fixings.
 *
 * These files belong to the run rather than to a configuration, so no
 * configuration type names them. The table still states their names, so a run
 * document is built without a file name written in code.
 */
inline std::vector<const file_type*> setup_data_file_types(const std::vector<file_type>& types) {
    std::vector<const file_type*> out;
    for (const auto& t : types)
        if (!t.analytic_type_code && t.kind == data_file_kind && t.direction == input_direction)
            out.push_back(&t);
    return out;
}

}

#endif
