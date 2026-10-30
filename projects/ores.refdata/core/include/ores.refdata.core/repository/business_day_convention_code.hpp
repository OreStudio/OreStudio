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
#ifndef ORES_REFDATA_CORE_REPOSITORY_BUSINESS_DAY_CONVENTION_CODE_HPP
#define ORES_REFDATA_CORE_REPOSITORY_BUSINESS_DAY_CONVENTION_CODE_HPP

#include <map>
#include <optional>
#include <string>

namespace ores::refdata::repository {

/**
 * @brief The code the store keeps for the business day convention a caller states.
 *
 * ORE's parser accepts twenty-seven spellings for seven conventions, so two
 * callers that say =F= and =Following= mean the same adjustment. The mapping
 * layer converts the stated spelling to the code before a row is written, so
 * the table holds one representation and a query that groups by the column
 * answers seven rows and not twenty-seven.
 *
 * A spelling the map does not know is returned as it stands: the value set
 * refuses it with a sentence that names the set, which is a better answer than
 * a silent substitution.
 */
inline std::string stored_business_day_convention(const std::string& stated) {
    static const std::map<std::string, std::string> conventions = {
        {"F", "Following"},
        {"Following", "Following"},
        {"FOLLOWING", "Following"},
        {"MF", "ModifiedFollowing"},
        {"ModifiedFollowing", "ModifiedFollowing"},
        {"Modified Following", "ModifiedFollowing"},
        {"MODIFIEDF", "ModifiedFollowing"},
        {"MODFOLLOWING", "ModifiedFollowing"},
        {"P", "Preceding"},
        {"Preceding", "Preceding"},
        {"PRECEDING", "Preceding"},
        {"MP", "ModifiedPreceding"},
        {"ModifiedPreceding", "ModifiedPreceding"},
        {"Modified Preceding", "ModifiedPreceding"},
        {"MODIFIEDP", "ModifiedPreceding"},
        {"U", "Unadjusted"},
        {"Unadjusted", "Unadjusted"},
        {"INDIFF", "Unadjusted"},
        {"NONE", "Unadjusted"},
        {"NotApplicable", "Unadjusted"},
        {"HalfMonthModifiedFollowing", "HalfMonthModifiedFollowing"},
        {"HMMF", "HalfMonthModifiedFollowing"},
        {"Half Month Modified Following", "HalfMonthModifiedFollowing"},
        {"HALFMONTHMF", "HalfMonthModifiedFollowing"},
        {"HalfMonthMF", "HalfMonthModifiedFollowing"},
        {"NEAREST", "Nearest"},
        {"Nearest", "Nearest"},
    };
    const auto known = conventions.find(stated);
    return known == conventions.end() ? stated : known->second;
}

/** The code for a convention a caller states, or empty when it states none. */
inline std::string stored_business_day_convention(const std::optional<std::string>& stated) {
    return stated.has_value() ? stored_business_day_convention(*stated) : std::string{};
}

}

#endif
