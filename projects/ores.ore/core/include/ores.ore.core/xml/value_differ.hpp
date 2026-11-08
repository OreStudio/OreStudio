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
#ifndef ORES_ORE_CORE_XML_VALUE_DIFFER_HPP
#define ORES_ORE_CORE_XML_VALUE_DIFFER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief How a value differs between what was expected and what was found.
 */
enum class value_change : std::uint8_t {
    /// The expected document states the value and the actual one does not.
    lost,
    /// The actual document states a value the expected one does not.
    added,
    /// Both state the path, with different values.
    changed,
};

/**
 * @brief One value that differs between two fingerprints.
 */
struct value_difference {
    std::string path;
    value_change change = value_change::changed;
    /// The value the expected document states; empty for an added value.
    std::string expected;
    /// The value the actual document states; empty for a lost value.
    std::string actual;
};

/**
 * @brief Every value that differs between two fingerprints, in path order.
 *
 * A round trip is proven by the absence of differences, and when one fails the
 * reader needs every field that went missing, not the first. The fingerprint's
 * paths carry no index, so the values at one path are compared as a multiset:
 * the values both sides state cancel out, the rest are paired in order as
 * changes, and whatever is left over is lost or added.
 *
 * @param expected The fingerprint of the document that states the truth, such
 * as the one a round trip read.
 * @param actual The fingerprint of the document under test, such as the one it
 * wrote.
 */
ORES_ORE_CORE_EXPORT auto
diff_values(const std::vector<value_entry>& expected,
            const std::vector<value_entry>& actual) -> std::vector<value_difference>;

/**
 * @brief One difference as a line a person reads.
 *
 * For example =lost Trade/FxOptionData/OptionData/Style: 'European'= or
 * =changed Trade/FxOptionData/BoughtAmount: '100' became '200'=.
 */
ORES_ORE_CORE_EXPORT auto describe(const value_difference& difference) -> std::string;

}

#endif
