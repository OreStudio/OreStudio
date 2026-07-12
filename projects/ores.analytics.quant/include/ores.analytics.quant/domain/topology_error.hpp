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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_TOPOLOGY_ERROR_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_TOPOLOGY_ERROR_HPP

#include <string>

namespace ores::analytics::quant::domain {

enum class topology_error_kind {
    /// A currency in the required-majors list is unreachable from the pivot.
    missing_major,
    /// Two edges would connect the same pair of currencies a second time --
    /// the graph would gain a cycle, i.e. two disagreeing paths.
    cycle_conflict,
    /// A currency mentioned in the input pairs is not connected to the pivot.
    disconnected_currency,
    /// The same base/quote pair appears more than once in the input.
    duplicate_edge,
};

/// One structured, human-readable diagnosis of why a topology could not be
/// built. @c topology_builder collects every violation across the whole
/// input rather than stopping at the first one.
struct topology_error {
    topology_error_kind kind;
    std::string base_code;
    std::string quote_code;

    [[nodiscard]] std::string describe() const {
        switch (kind) {
        case topology_error_kind::missing_major:
            return base_code + ": required major is unreachable from the pivot";
        case topology_error_kind::cycle_conflict:
            return base_code + "/" + quote_code +
                ": conflicts with an existing path -- would create a cycle";
        case topology_error_kind::disconnected_currency:
            return base_code + ": not connected to the pivot";
        case topology_error_kind::duplicate_edge:
            return base_code + "/" + quote_code + ": duplicate pair in input";
        }
        return base_code + "/" + quote_code + ": unknown topology error";
    }
};

} // namespace ores::analytics::quant::domain

#endif
