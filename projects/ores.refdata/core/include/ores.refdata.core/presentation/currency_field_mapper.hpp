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
#ifndef ORES_REFDATA_CORE_PRESENTATION_CURRENCY_FIELD_MAPPER_HPP
#define ORES_REFDATA_CORE_PRESENTATION_CURRENCY_FIELD_MAPPER_HPP

#include "ores.diff/domain/field_value.hpp"
#include "ores.refdata.api/domain/currency.hpp"
#include "ores.refdata.api/domain/currency_version.hpp"
#include "ores.refdata.core/export.hpp"
#include <vector>

namespace ores::refdata::presentation {

/**
 * @brief Renders a currency into the ordered field list used by
 * history responses.
 *
 * The mapper owns the display decisions: field labels, field order,
 * and how each value renders as a string. The diff engine and the
 * frontends consume its output without interpreting it.
 */
class ORES_REFDATA_CORE_EXPORT currency_field_mapper final {
public:
    /**
     * @brief Maps a currency to its ordered, rendered field list.
     */
    [[nodiscard]] static std::vector<ores::diff::domain::field_value>
    map(const domain::currency& c);

    /**
     * @brief Wraps currencies (newest first) into versions carrying
     * rendered fields and the diff against the previous version.
     *
     * The oldest version has empty changes.
     */
    [[nodiscard]] static std::vector<domain::currency_version>
    build_versions(const std::vector<domain::currency>& currencies);
};

}

#endif
