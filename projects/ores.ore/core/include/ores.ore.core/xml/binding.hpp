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
#ifndef ORES_ORE_CORE_XML_BINDING_HPP
#define ORES_ORE_CORE_XML_BINDING_HPP

#include "ores.ore.core/export.hpp"
#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>

namespace ores::ore::xml {

/**
 * @brief How one element's or attribute's text becomes a field.
 *
 * The kind is a decision about our model, not a reading of the schema. ORE
 * types the same money concept as xs:float, xs:double and xs:string in three
 * different places, so following the schema would put a binary float under a
 * money amount. The generator takes the shape from the schema and the kind
 * from this project's money vocabulary.
 */
enum class field_kind : std::uint8_t {
    /// A string field, filled from the element's text.
    text,
    /// An integer field.
    integer,
    /// A money amount or any other exact decimal.
    decimal,
    /// A continuous quantity, which stays binary.
    real,
    /// An index into the field's spelling table.
    enumeration,
    /// A required or optional child of a complex type.
    nested,
    /// A repeated leaf, appended to a vector of strings.
    repeated_text,
    /// A repeated child of a complex type, appended to a vector.
    repeated_nested,
};

struct record_type;

/**
 * @brief One element or attribute of one schema type.
 *
 * The offsets and the presence bit mean the engine needs no knowledge of the
 * record layout. The one function pointer covers the only thing an offset
 * cannot do: grow a repeated member, whose element type only the record knows.
 */
struct field {
    std::string_view name;
    std::uint32_t name_hash;
    field_kind kind;
    std::uint16_t offset;
    std::uint8_t bit;
    bool is_attribute;
    /// 1 when the schema requires the element or attribute, 0 otherwise.
    bool required;
    /// Spelling table for an enumeration, null otherwise.
    const char* const* tokens;
    /// Element type for nested and repeated_nested.
    const record_type* type;
    /// Appends a default-constructed element to a vector and hands back its
    /// address. Null for every other kind.
    void (*append)(void* container, void** slot);
};

/**
 * @brief The fields of one schema type.
 */
struct record_type {
    std::string_view name;
    const field* fields;
    std::uint8_t field_count;
};

/**
 * @brief One document type: a root element and the record it fills.
 */
struct document_type {
    std::string_view root_name;
    const record_type* root;
};

/**
 * @brief Where a document failed to bind.
 */
struct bind_error {
    std::string message;
    std::uint32_t line = 0;
    std::uint32_t column = 0;
};

/**
 * @brief Fills one record from one document, as the scanner walks past it.
 *
 * The engine holds a stack of at most max_depth frames, each a type and the
 * record being filled. Nothing is retained after a frame closes, so the
 * document is never materialised: a repeated child is appended to the vector
 * the table names and the event loop moves on. That is the whole difference
 * between this and the tape, and the tape costs between four and eleven times
 * the first-level cache misses of a scan.
 *
 * The root record is built into the caller's storage. Nothing is allocated
 * beyond what the record's own members allocate.
 */
class ORES_ORE_CORE_EXPORT binder {
public:
    /// The deepest nesting the engine will follow.
    static constexpr std::uint8_t max_depth = 32;

    /**
     * @brief Binds a document into a root record.
     *
     * On failure the record is left in whatever state the engine reached, so a
     * caller that wants to keep partial results must decide that itself. The
     * usual contract is to discard the record.
     */
    static auto bind(const document_type& doc, std::string_view text, void* root, bind_error& error)
        -> bool;
};

}

#endif
