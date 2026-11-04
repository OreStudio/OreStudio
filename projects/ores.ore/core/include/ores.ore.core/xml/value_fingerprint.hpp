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
#ifndef ORES_ORE_CORE_XML_VALUE_FINGERPRINT_HPP
#define ORES_ORE_CORE_XML_VALUE_FINGERPRINT_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/document.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief One value a document states, and where it states it.
 */
struct value_entry {
    /// The element path, with =/@name= for an attribute and =/text()= for text
    /// that sits beside a child element.
    std::string path;
    /// The value, with every number written as the decimal it means.
    std::string value;
};

/**
 * @brief Every value a document carries, as a sorted list of path and value.
 *
 * This is the reading a comparison can trust, because it consults nothing but
 * the document: no schema, no table, no record. A number is reduced to the
 * decimal it means, so the same value spelled =1e-10= in one document and
 * =0.0000000001= in another is one value and not two.
 *
 * The order the entries are listed in is the sorted order of the pair, so two
 * fingerprints compare equal exactly when the documents state the same values
 * at the same paths. Sibling order inside an =xs:all= is not part of that,
 * because the schema says it carries no meaning.
 *
 * @param doc The document to read.
 * @return The values, sorted.
 */
ORES_ORE_CORE_EXPORT auto value_fingerprint(const document& doc) -> std::vector<value_entry>;

/**
 * @brief The values of one occurrence of an element, and the key it states.
 */
struct keyed_fingerprint {
    /// The value of the element's key attribute, or empty when it states none.
    std::string key;
    /// The element's values, sorted, with paths that start at the element.
    std::vector<value_entry> values;
};

/**
 * @brief The fingerprint of each occurrence of an element, in document order.
 *
 * A portfolio holds many trades, and a difference is only useful when it says
 * which trade it is in. Fingerprinting each trade on its own, keyed by its id,
 * lets two portfolios be compared trade by trade even when the writing orders
 * the trades differently.
 *
 * An occurrence nested inside another occurrence of the same element belongs
 * to the outer one.
 *
 * Unlike the whole document's fingerprint, an element that repeats among its
 * siblings is labelled with its position, from one, such as =LegData[2]=, so
 * the values of one leg are not compared with those of another.
 *
 * @param doc The document to read.
 * @param element The name of the element to fingerprint, such as =Trade=.
 * @param key_attribute The attribute that names an occurrence, such as =id=.
 * @return One fingerprint per occurrence, in the order the document states them.
 */
ORES_ORE_CORE_EXPORT auto
value_fingerprints_of(const document& doc, std::string_view element, std::string_view key_attribute)
    -> std::vector<keyed_fingerprint>;

}

#endif
