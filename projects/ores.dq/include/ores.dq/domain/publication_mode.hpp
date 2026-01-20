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
#ifndef ORES_DQ_DOMAIN_PUBLICATION_MODE_HPP
#define ORES_DQ_DOMAIN_PUBLICATION_MODE_HPP

#include <string>
#include <optional>
#include <ostream>

namespace ores::dq::domain {

/**
 * @brief Defines how conflicts are handled during dataset publication.
 *
 * When publishing a dataset to production tables, this mode determines
 * the behavior when records already exist in the target table.
 */
enum class publication_mode {
    /**
     * @brief Insert new records and update existing ones.
     *
     * This is the default mode. New records are inserted, and existing
     * records (matching by key) are updated with the new values.
     */
    upsert,

    /**
     * @brief Only insert new records, skip existing ones.
     *
     * Existing records are left untouched. Only records that don't
     * already exist in the target table are inserted.
     */
    insert_only,

    /**
     * @brief Delete all existing records before inserting.
     *
     * All existing records in the target table are soft-deleted,
     * then all records from the dataset are inserted fresh.
     * Use with caution as this removes all previous data.
     */
    replace_all
};

/**
 * @brief Convert publication_mode to its string representation.
 * @param mode The publication mode to convert.
 * @return String representation: "upsert", "insert_only", or "replace_all".
 */
std::string to_string(publication_mode mode);

/**
 * @brief Parse a string to publication_mode.
 * @param s The string to parse.
 * @return The parsed mode, or std::nullopt if the string is invalid.
 */
std::optional<publication_mode> publication_mode_from_string(const std::string& s);

/**
 * @brief Stream output operator for publication_mode.
 */
std::ostream& operator<<(std::ostream& s, publication_mode mode);

}

#endif
