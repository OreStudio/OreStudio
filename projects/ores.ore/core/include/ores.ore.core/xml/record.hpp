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
#ifndef ORES_ORE_CORE_XML_RECORD_HPP
#define ORES_ORE_CORE_XML_RECORD_HPP

#include "ores.ore.core/export.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::xml {

/**
 * @brief One record read through its table, by the names the document writes.
 *
 * A projection that reads a document names the elements it takes and says
 * nothing about offsets or presence bits: the table it was handed already
 * describes the record, and the reader asks it. The distinction the codec
 * exists to keep is preserved here -- an element that never arrived is
 * `std::nullopt`, and one that arrived empty is an empty string -- because a
 * projection that cannot tell the two apart cannot write a null column.
 *
 * The reader holds a pointer, so it lives no longer than the record it reads.
 */
class ORES_ORE_CORE_EXPORT record_reader {
public:
    record_reader(const record_type& type, const void* record);

    auto type() const -> const record_type&;

    /// Whether the document states this element or attribute at all.
    auto has(std::string_view name) const -> bool;

    /**
     * @brief Whether the table declares this element or attribute.
     *
     * A record read from a document may state nothing for a member the schema
     * allows it to omit, and a projection asks this before it asks `has`: a
     * holder type that has no slot for an element at all is a different
     * question from one that has the slot and did not fill it.
     */
    auto declares(std::string_view name) const -> bool;

    /// The text of a string element or attribute.
    auto text(std::string_view name) const -> std::optional<std::string>;

    /// The value of an integer element.
    auto integer(std::string_view name) const -> std::optional<std::int64_t>;

    /// The value of a continuous element, which stays binary.
    auto real(std::string_view name) const -> std::optional<double>;

    /// The value of a money or other exact decimal element.
    auto decimal(std::string_view name) const -> std::optional<ores::utility::decimal::decimal>;

    /// The schema's spelling of an enumerated element.
    auto spelling(std::string_view name) const -> std::optional<std::string>;

    /**
     * @brief The value of a boolean element, as the document spelled it.
     *
     * ORE writes a boolean as any of Y, YES, TRUE, True, true or 1, so the
     * reading accepts every spelling the schema allows rather than one.
     */
    auto flag(std::string_view name) const -> std::optional<bool>;

    /// The text an element of a type written as xs:simpleContent carries itself.
    auto own_text() const -> std::optional<std::string>;

    /// A child element the document states, or nothing when it is absent.
    auto child(std::string_view name) const -> std::optional<record_reader>;

    /// Every iteration of a repeated child, in the order the document wrote them.
    auto children(std::string_view name) const -> std::vector<record_reader>;

    /// Every iteration of a repeated leaf, in the order the document wrote them.
    auto values(std::string_view name) const -> std::vector<std::string>;

private:
    const record_type* type_;
    const void* record_;
};

/**
 * @brief One record written through its table, by the names the document writes.
 *
 * The writer states the rows it fills, because the emitter writes only the rows
 * whose presence bit is set: a value set without its bit is written nowhere.
 * Every accessor refuses a name its table does not have, so a schema rename
 * stops the export instead of dropping a field from it.
 *
 * An enumerated value is written from the spelling the projection holds, and a
 * spelling the schema does not allow stops with the name of the field. A
 * boolean is written as Y or N, which every ORE schema allows and which reads
 * back as the same flag.
 */
class ORES_ORE_CORE_EXPORT record_writer {
public:
    record_writer(const record_type& type, void* record);

    auto type() const -> const record_type&;

    /// Whether the table declares this element or attribute.
    auto declares(std::string_view name) const -> bool;

    /// States that this row holds a value. Needed before a nested row is filled.
    void state(std::string_view name);

    void set_text(std::string_view name, std::string_view value);
    void set_integer(std::string_view name, std::int64_t value);
    void set_real(std::string_view name, double value);
    void set_decimal(std::string_view name, const ores::utility::decimal::decimal& value);

    /// States the row only when the caller holds a value.
    void set_optional_text(std::string_view name, const std::optional<std::string>& value);

    /// States the row only when the caller holds a value.
    void set_optional_integer(std::string_view name, const std::optional<std::int64_t>& value);

    /// States the row only when the caller holds a value.
    void set_optional_real(std::string_view name, const std::optional<double>& value);

    /// States the row only when the caller holds a value.
    void set_optional_decimal(std::string_view name,
                              const std::optional<ores::utility::decimal::decimal>& value);

    /**
     * @brief States an enumerated element from the spelling the caller holds.
     *
     * @param what Names the enumeration in the refusal, because the field alone
     * says where the value goes and not what it means.
     */
    void set_spelling(std::string_view name, std::string_view value, std::string_view what);

    /// States an enumerated element only when the caller holds a spelling.
    void set_optional_spelling(std::string_view name,
                               const std::optional<std::string>& value,
                               std::string_view what);

    void set_flag(std::string_view name, bool value);

    /// States a flag only when the caller holds one.
    void set_optional_flag(std::string_view name, const std::optional<bool>& value);

    /// States the text an element of a xs:simpleContent type carries itself.
    void set_own_text(std::string_view value);

    /// States a child element and returns the writer over it.
    auto child(std::string_view name) -> record_writer;

    /// States a repeated child, appends one iteration and returns its writer.
    auto append(std::string_view name) -> record_writer;

    /// States a repeated leaf and appends one value to it.
    void append_value(std::string_view name, std::string_view value);

private:
    const field& field_of(std::string_view name) const;
    void* slot_of(const field& f) const;

    const record_type* type_;
    void* record_;
};

}

#endif
