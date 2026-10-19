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
#include <expected>
#include <optional>
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
/**
 * @brief The element type of a repeated leaf.
 */
enum class scalar_kind : std::uint8_t { text, integer, decimal, real, enumeration };

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
    /// A repeated leaf, appended to a vector of scalars. `scalar` says which.
    repeated_leaf,
    /// A bag of content the schema does not name, captured rather than lost.
    residual,
    /// A repeated child of a complex type, appended to a vector.
    repeated_nested,
    /**
     * A child of a complex type that holds the type again.
     *
     * A record is a fixed layout and cannot hold itself, so a recursive member
     * is an index into an arena the parent owns: `offset` is a
     * `std::vector<std::uint32_t>` of indices and `arena_offset` a
     * `std::vector<T>` of the rows they name, `T` being the type the row points
     * at. The builder appends to the arena and pushes the index, and the writer
     * follows the indices in order.
     */
    indexed_nested,
};

struct record_type;

/**
 * @brief The operations a repeated member needs from the table.
 *
 * A repeated member is a std::vector, or a std::deque when its element is a
 * wide record that must not move. Only the generated code knows the container
 * and the element type, so growing it, measuring it and reaching into it are
 * three functions the table carries rather than switches the engine makes on a
 * member's kind. The reader appends, the writer measures and indexes.
 */
struct vector_ops {
    /// Appends a default-constructed element to the container and hands back
    /// its address.
    void (*append)(void* container, void** slot);
    /// How many elements the container holds.
    auto (*count)(const void* container) -> std::size_t;
    /// The address of one element, which the caller bounds.
    auto (*at)(const void* container, std::size_t index) -> const void*;
};

/**
 * @brief One element the schema does not name, kept so it is not lost.
 *
 * Only text content is captured. An unknown element with children cannot be
 * represented, so the binding refuses the document rather than store half of
 * it.
 */
struct residual_element {
    std::string name;
    std::string text;
};

/// The value field::arena_offset holds when a member indexes no arena.
inline constexpr std::uint32_t no_arena = 0xFFFFFFFFu;

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
    std::uint32_t offset;
    std::uint8_t bit;
    bool is_attribute;
    /// 1 when the schema requires the element or attribute, 0 otherwise.
    bool required;
    /// Spelling table for an enumeration, null otherwise.
    const char* const* tokens;
    /// Element type for nested and repeated_nested.
    const record_type* type;
    /// The element type of a repeated_leaf, ignored otherwise.
    scalar_kind scalar;
    /// Operations for a repeated member, null for every other kind. For
    /// indexed_nested these operate on the arena.
    const vector_ops* repeated = nullptr;
    /// The offset of the arena an indexed_nested member indexes into.
    std::uint32_t arena_offset = no_arena;
    /// True when an indexed_nested member's element may appear once, so a
    /// second row is refused rather than appended.
    bool once = false;
};

/**
 * @brief The fields of one schema type.
 */
struct record_type {
    std::string_view name;
    const field* fields;
    std::uint8_t field_count;
    /// Index of the field that receives the element's own text, for a type
    /// written as xs:simpleContent with attributes. 0xFF when the element has
    /// no text of its own.
    std::uint8_t text_field;
};

/**
 * @brief One document type: a root element and the record it fills.
 */
struct document_type {
    std::string_view root_name;
    const record_type* root;
};

/// The value record_type::text_field holds when a type has no text of its own.
inline constexpr std::uint8_t no_text = 0xFF;

/**
 * @brief Whether a record's presence mask states the field's bit.
 *
 * The mask is the first member of every record the table describes, which is
 * what lets an offset and a bit describe a record the engine never names. It is
 * one 64-bit word per 64 fields: a type with more fields states its mask as an
 * array, and the word a field's bit falls in is the bit divided by 64. Both the
 * engine and the projections that read records use these two functions, so the
 * layout is stated once.
 */
inline auto bit_is_set(const void* record, const field& f) -> bool {
    const auto* words = static_cast<const std::uint64_t*>(record);
    return (words[f.bit / 64] & (1ull << (f.bit % 64))) != 0;
}

/// States the field's bit in the record's presence mask.
inline void set_bit(void* record, const field& f) {
    auto* words = static_cast<std::uint64_t*>(record);
    words[f.bit / 64] |= (1ull << (f.bit % 64));
}

/**
 * @brief Where a document failed to bind.
 */
struct bind_error {
    std::string message;
    std::uint32_t line = 0;
    std::uint32_t column = 0;
};

/**
 * @brief The field a type declares under a name, or null when it declares none.
 *
 * A projection from a record into a domain type asks the table which fields
 * exist rather than repeating the schema, so a renamed element is a generated
 * change and not a silent disagreement.
 */
ORES_ORE_CORE_EXPORT auto find_field(const record_type& type, std::string_view name, bool attribute)
    -> const field*;

/**
 * @brief The table a named child element of a type is described by, or null.
 *
 * A projection that walks into a nested record names the element, not the
 * table: the table is the type the caller's column points at, and asking for
 * it keeps the schema's shape in the generated code.
 */
ORES_ORE_CORE_EXPORT auto find_child_type(const record_type& type, std::string_view name)
    -> const record_type*;

/**
 * @brief Marks the element's own text as present.
 *
 * A type written as xs:simpleContent holds text beside its attributes, and
 * that text is a row of its own with no name: the table names it by index.
 * A projection that sets the text states it here.
 *
 * @return False when the type holds no text of its own.
 */
ORES_ORE_CORE_EXPORT auto mark_text_present(const record_type& type, void* record) -> bool;

/**
 * @brief Whether a record states a value for the named field.
 *
 * An element that holds an empty string is present and empty, and one that
 * never arrived is absent: the presence bit is the difference, and a
 * projection that reads a member without it cannot tell the two apart.
 */
ORES_ORE_CORE_EXPORT auto
is_present(const record_type& type, const void* record, std::string_view name) -> bool;

/**
 * @brief Marks the named field of a record as present.
 *
 * The reader sets the bit when the element arrives. A record built by a
 * projection states that itself, because the writer writes only the fields
 * whose bit is set -- a value set without its bit is written nowhere.
 *
 * @return False when the type declares no such field.
 */
ORES_ORE_CORE_EXPORT auto mark_present(const record_type& type, void* record, std::string_view name)
    -> bool;

/**
 * @brief The spelling a record states for an enumerated field.
 *
 * A projection of ours speaks the schema's spelling, not the index the table
 * stores, and the index means nothing without the table that produced it.
 *
 * @return The spelling, or nothing when the field is absent or holds no
 * spelling.
 */
ORES_ORE_CORE_EXPORT auto enumeration_spelling(const record_type& type,
                                               const void* record,
                                               std::string_view name) -> std::optional<std::string>;

/**
 * @brief States an enumerated field from the spelling a caller holds.
 *
 * @return False when the type declares no such field or the spelling is not
 * one it allows.
 */
ORES_ORE_CORE_EXPORT auto set_enumeration(const record_type& type,
                                          void* record,
                                          std::string_view name,
                                          std::string_view spelling) -> bool;

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
     *
     * @return Nothing on success, or where and why the document could not be
     * bound.
     */
    static auto bind(const document_type& doc, std::string_view text, void* root)
        -> std::expected<void, bind_error>;
};

}

#endif
