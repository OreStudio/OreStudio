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
#ifndef ORES_ORE_CORE_XML_RECORD_DIFF_HPP
#define ORES_ORE_CORE_XML_RECORD_DIFF_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include <optional>
#include <string>

namespace ores::ore::xml {

/**
 * @brief The first place two records disagree, and how.
 */
struct record_difference {
    /// Where the disagreement is, as the elements a reader would walk:
    /// "Calendar[0].AdditionalHolidays.Date[1]".
    std::string path;
    /// What was found in each record.
    std::string detail;
};

/**
 * @brief Compares two records the table describes, field by field.
 *
 * A round trip is only proved when the values that came out are the values
 * that went in, and comparing the written text proves something weaker: a
 * field the writing dropped is absent from both writings. This walk compares
 * the records themselves, so a member that the writing loses or alters is a
 * named difference rather than a missing line in a document.
 *
 * The comparison is exact. An amount is the decimal the reading stored, not
 * text that happens to look the same; a continuous quantity is the same
 * double; a presence bit must agree, so a field that was absent cannot come
 * back as an empty string.
 *
 * @param type The record type both records are laid out by.
 * @param left One record.
 * @param right The other.
 * @return The first difference in table order, or nothing when they agree.
 */
ORES_ORE_CORE_EXPORT auto compare_records(const record_type& type,
                                          const void* left,
                                          const void* right) -> std::optional<record_difference>;

}

#endif
