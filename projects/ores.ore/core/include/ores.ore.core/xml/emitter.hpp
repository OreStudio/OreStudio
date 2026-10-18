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
#ifndef ORES_ORE_CORE_XML_EMITTER_HPP
#define ORES_ORE_CORE_XML_EMITTER_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include <expected>
#include <string>

namespace ores::ore::xml {

/**
 * @brief Why a record could not be written as a document.
 */
struct emit_error {
    std::string message;
};

/**
 * @brief Writes one record as an XML document, from the same table the binder
 * reads it with.
 *
 * The write direction does not mirror the read direction. It walks the table
 * in schema order, writes only the fields whose presence bit is set, and
 * refuses a required field that is absent, so a record that cannot produce a
 * valid document fails here rather than producing one that a validator
 * rejects.
 *
 * An exact decimal is written as the canonical plain decimal text of its
 * value, never as an exponent. A continuous quantity is written as the
 * shortest text that reads back to the same double, which may be an exponent
 * because xs:float and xs:double accept one.
 */
class ORES_ORE_CORE_EXPORT emitter {
public:
    /**
     * @brief Writes a record as a document rooted at its own element.
     *
     * @param doc The document type: the root element's name and its record.
     * @param root The root record, which must be the one the type describes.
     * @param out Replaced with the document, and left empty on failure.
     * @return Nothing on success, or the first element that could not be
     * written and why.
     */
    static auto emit(const document_type& doc,
                     const void* root,
                     std::string& out) -> std::expected<void, emit_error>;
};

}

#endif
