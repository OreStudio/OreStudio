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
#include "ores.ore.core/xml/record.hpp"
#include "ores.ore.core/xml/lexical.hpp"
#include <charconv>
#include <format>
#include <stdexcept>

namespace ores::ore::xml {

namespace {

constexpr std::string_view field_kind_name(field_kind kind) {
    switch (kind) {
        case field_kind::text:
            return "text";
        case field_kind::integer:
            return "an integer";
        case field_kind::decimal:
            return "an exact decimal";
        case field_kind::real:
            return "a real number";
        case field_kind::enumeration:
            return "an enumeration";
        case field_kind::nested:
            return "a child element";
        case field_kind::repeated_leaf:
            return "a repeated value";
        case field_kind::residual:
            return "unnamed content";
        case field_kind::repeated_nested:
            return "a repeated child element";
        case field_kind::indexed_nested:
            return "an indexed child element";
        case field_kind::column_nested:
            return "an alternative held in a column";
    }
    return "an unknown shape";
}

const field& require_field(const record_type& type, std::string_view name) {
    const auto* found = find_field(type, name, false);
    if (found == nullptr)
        found = find_field(type, name, true);
    if (found == nullptr)
        throw std::runtime_error(std::format("the {} table has no '{}' field", type.name, name));
    return *found;
}

const field& require_kind(const record_type& type,
                          std::string_view name,
                          field_kind kind,
                          std::string_view expected) {
    const auto& f = require_field(type, name);
    if (f.kind != kind)
        throw std::runtime_error(std::format("'{}' of the {} table holds {}, not {}",
                                             name,
                                             type.name,
                                             field_kind_name(f.kind),
                                             expected));
    return f;
}

const void* slot_of(const void* record, const field& f) {
    return static_cast<const std::byte*>(record) + f.offset;
}

/// The columns struct a record keeps for the type that member `f` holds, or null.
const void* columns_held(const void* record, const field& f) {
    if (f.columns_offset == no_arena)
        return nullptr;
    return static_cast<const std::byte*>(record) + f.columns_offset;
}

void* columns_held(void* record, const field& f) {
    return const_cast<void*>(columns_held(static_cast<const void*>(record), f));
}

/// The index a column_nested member's row holds, checked against its column.
const void* column_row(const void* record, const void* columns, const field& f) {
    if (columns == nullptr || f.repeated == nullptr || f.type == nullptr ||
        f.arena_offset == no_arena)
        throw std::runtime_error(std::format(
            "'{}' names an alternative held in a column the record does not carry", f.name));
    const auto* rows = static_cast<const std::byte*>(columns) + f.arena_offset;
    const auto index = *static_cast<const std::uint32_t*>(slot_of(record, f));
    if (index >= f.repeated->count(rows))
        throw std::runtime_error(std::format("'{}' names a row its column does not hold", f.name));
    return f.repeated->at(rows, index);
}

}

record_reader::record_reader(const record_type& type, const void* record, const void* columns)
    : type_(&type)
    , record_(record)
    , columns_(columns) {}

auto record_reader::type() const -> const record_type& {
    return *type_;
}

auto record_reader::declares(std::string_view name) const -> bool {
    return find_field(*type_, name, false) != nullptr || find_field(*type_, name, true) != nullptr;
}

auto record_reader::has(std::string_view name) const -> bool {
    return bit_is_set(record_, require_field(*type_, name));
}

auto record_reader::text(std::string_view name) const -> std::optional<std::string> {
    const auto& f = require_kind(*type_, name, field_kind::text, "text");
    if (!bit_is_set(record_, f))
        return std::nullopt;
    return *static_cast<const std::string*>(slot_of(record_, f));
}

auto record_reader::integer(std::string_view name) const -> std::optional<std::int64_t> {
    const auto& f = require_kind(*type_, name, field_kind::integer, "an integer");
    if (!bit_is_set(record_, f))
        return std::nullopt;
    return *static_cast<const std::int64_t*>(slot_of(record_, f));
}

auto record_reader::real(std::string_view name) const -> std::optional<double> {
    const auto& f = require_kind(*type_, name, field_kind::real, "a real number");
    if (!bit_is_set(record_, f))
        return std::nullopt;
    return *static_cast<const double*>(slot_of(record_, f));
}

auto record_reader::decimal(std::string_view name) const
    -> std::optional<ores::utility::decimal::decimal> {
    const auto& f = require_kind(*type_, name, field_kind::decimal, "a decimal");
    if (!bit_is_set(record_, f))
        return std::nullopt;
    return *static_cast<const ores::utility::decimal::decimal*>(slot_of(record_, f));
}

auto record_reader::spelling(std::string_view name) const -> std::optional<std::string> {
    require_kind(*type_, name, field_kind::enumeration, "an enumeration");
    return enumeration_spelling(*type_, record_, name);
}

auto record_reader::flag(std::string_view name) const -> std::optional<bool> {
    const auto spelling_value = spelling(name);
    if (!spelling_value)
        return std::nullopt;
    const auto& s = *spelling_value;
    const bool on = s == "Y" || s == "YES" || s == "TRUE" || s == "True" || s == "true" || s == "1";
    return on;
}

auto record_reader::own_text() const -> std::optional<std::string> {
    if (type_->text_field == no_text)
        return std::nullopt;
    const auto& f = type_->fields[type_->text_field];
    if (!bit_is_set(record_, f))
        return std::nullopt;
    switch (f.kind) {
        case field_kind::text:
            return *static_cast<const std::string*>(slot_of(record_, f));
        case field_kind::integer:
            return std::to_string(*static_cast<const std::int64_t*>(slot_of(record_, f)));
        case field_kind::decimal:
            return static_cast<const utility::decimal::decimal*>(slot_of(record_, f))->to_string();
        case field_kind::real: {
            char buffer[64]{};
            const auto value = *static_cast<const double*>(slot_of(record_, f));
            const auto [stop, ec] = std::to_chars(buffer, buffer + sizeof(buffer), value);
            if (ec != std::errc{})
                return std::nullopt;
            return std::string(buffer, stop);
        }
        case field_kind::enumeration: {
            const auto index = *static_cast<const std::int16_t*>(slot_of(record_, f));
            if (index < 0 || f.tokens == nullptr)
                return std::nullopt;
            for (std::int16_t i = 0; i <= index; ++i)
                if (f.tokens[i] == nullptr)
                    return std::nullopt;
            return std::string(f.tokens[index]);
        }
        default:
            return std::nullopt;
    }
}

auto record_reader::child(std::string_view name) const -> std::optional<record_reader> {
    const auto& f = require_field(*type_, name);
    if (f.kind == field_kind::column_nested) {
        if (!bit_is_set(record_, f))
            return std::nullopt;
        return record_reader(*f.type, column_row(record_, columns_, f));
    }
    require_kind(*type_, name, field_kind::nested, "a child element");
    if (!bit_is_set(record_, f) || f.type == nullptr)
        return std::nullopt;
    return record_reader(*f.type, slot_of(record_, f), columns_held(record_, f));
}

auto record_reader::children(std::string_view name) const -> std::vector<record_reader> {
    const auto& f = require_kind(*type_, name, field_kind::repeated_nested, "a repeated child");
    std::vector<record_reader> out;
    if (!bit_is_set(record_, f) || f.repeated == nullptr || f.type == nullptr)
        return out;
    const auto count = f.repeated->count(slot_of(record_, f));
    out.reserve(count);
    for (std::size_t i = 0; i < count; ++i)
        out.emplace_back(*f.type, f.repeated->at(slot_of(record_, f), i), columns_held(record_, f));
    return out;
}

auto record_reader::values(std::string_view name) const -> std::vector<std::string> {
    const auto& f = require_kind(*type_, name, field_kind::repeated_leaf, "a repeated value");
    if (f.scalar != scalar_kind::text)
        throw std::runtime_error(
            std::format("'{}' of the {} table does not hold text values", name, type_->name));
    std::vector<std::string> out;
    if (!bit_is_set(record_, f) || f.repeated == nullptr)
        return out;
    const auto count = f.repeated->count(slot_of(record_, f));
    out.reserve(count);
    for (std::size_t i = 0; i < count; ++i)
        out.push_back(*static_cast<const std::string*>(f.repeated->at(slot_of(record_, f), i)));
    return out;
}

auto record_reader::spellings(std::string_view name) const -> std::vector<std::string> {
    const auto& f = require_kind(*type_, name, field_kind::repeated_leaf, "a repeated value");
    if (f.scalar != scalar_kind::enumeration || f.tokens == nullptr)
        throw std::runtime_error(std::format(
            "'{}' of the {} table does not hold enumerated values", name, type_->name));
    std::vector<std::string> out;
    if (!bit_is_set(record_, f) || f.repeated == nullptr)
        return out;
    const auto count = f.repeated->count(slot_of(record_, f));
    out.reserve(count);
    for (std::size_t i = 0; i < count; ++i) {
        const auto index = *static_cast<const std::int16_t*>(f.repeated->at(slot_of(record_, f), i));
        if (index < 0)
            throw std::runtime_error(std::format(
                "'{}' of the {} table holds a value with no spelling", name, type_->name));
        for (std::int16_t k = 0; k <= index; ++k)
            if (f.tokens[k] == nullptr)
                throw std::runtime_error(std::format(
                    "'{}' of the {} table holds a value with no spelling", name, type_->name));
        out.emplace_back(f.tokens[index]);
    }
    return out;
}

auto record_reader::decimals(std::string_view name) const
    -> std::vector<ores::utility::decimal::decimal> {
    const auto& f = require_kind(*type_, name, field_kind::repeated_leaf, "a repeated value");
    if (f.scalar != scalar_kind::decimal)
        throw std::runtime_error(
            std::format("'{}' of the {} table does not hold exact decimals", name, type_->name));
    std::vector<ores::utility::decimal::decimal> out;
    if (!bit_is_set(record_, f) || f.repeated == nullptr)
        return out;
    const auto count = f.repeated->count(slot_of(record_, f));
    out.reserve(count);
    for (std::size_t i = 0; i < count; ++i)
        out.push_back(*static_cast<const ores::utility::decimal::decimal*>(
            f.repeated->at(slot_of(record_, f), i)));
    return out;
}

auto record_reader::indexed(std::string_view name) const -> std::vector<record_reader> {
    const auto& f = require_kind(*type_, name, field_kind::indexed_nested, "an indexed child");
    if (f.repeated == nullptr || f.type == nullptr || f.arena_offset == no_arena)
        throw std::runtime_error(std::format(
            "'{}' of the {} table names an indexed member with no arena", name, type_->name));
    std::vector<record_reader> out;
    if (!bit_is_set(record_, f))
        return out;
    const auto* indices = static_cast<const std::vector<std::uint32_t>*>(slot_of(record_, f));
    const auto* arena = static_cast<const std::byte*>(record_) + f.arena_offset;
    const auto rows = f.repeated->count(arena);
    for (const auto index : *indices) {
        if (index >= rows)
            throw std::runtime_error(std::format(
                "'{}' of the {} table names a row the arena does not hold", name, type_->name));
        out.emplace_back(*f.type, f.repeated->at(arena, index), columns_held(record_, f));
    }
    return out;
}

record_writer::record_writer(const record_type& type, void* record, void* columns)
    : type_(&type)
    , record_(record)
    , columns_(columns) {}

auto record_writer::type() const -> const record_type& {
    return *type_;
}

auto record_writer::declares(std::string_view name) const -> bool {
    return find_field(*type_, name, false) != nullptr || find_field(*type_, name, true) != nullptr;
}

auto record_writer::field_of(std::string_view name) const -> const field& {
    return require_field(*type_, name);
}

auto record_writer::slot_of(const field& f) const -> void* {
    return static_cast<std::byte*>(record_) + f.offset;
}

void record_writer::state(std::string_view name) {
    set_bit(record_, require_field(*type_, name));
}

void record_writer::set_text(std::string_view name, std::string_view value) {
    require_kind(*type_, name, field_kind::text, "text");
    *static_cast<std::string*>(slot_of(field_of(name))) = std::string(value);
    set_bit(record_, field_of(name));
}

void record_writer::set_optional_text(std::string_view name,
                                      const std::optional<std::string>& value) {
    require_kind(*type_, name, field_kind::text, "text");
    if (value)
        set_text(name, *value);
}

void record_writer::set_integer(std::string_view name, std::int64_t value) {
    require_kind(*type_, name, field_kind::integer, "an integer");
    *static_cast<std::int64_t*>(slot_of(field_of(name))) = value;
    set_bit(record_, field_of(name));
}

void record_writer::set_optional_integer(std::string_view name,
                                         const std::optional<std::int64_t>& value) {
    require_kind(*type_, name, field_kind::integer, "an integer");
    if (value)
        set_integer(name, *value);
}

void record_writer::set_real(std::string_view name, double value) {
    require_kind(*type_, name, field_kind::real, "a real number");
    *static_cast<double*>(slot_of(field_of(name))) = value;
    set_bit(record_, field_of(name));
}

void record_writer::set_optional_real(std::string_view name, const std::optional<double>& value) {
    require_kind(*type_, name, field_kind::real, "a real number");
    if (value)
        set_real(name, *value);
}

void record_writer::set_decimal(std::string_view name,
                                const ores::utility::decimal::decimal& value) {
    require_kind(*type_, name, field_kind::decimal, "a decimal");
    *static_cast<ores::utility::decimal::decimal*>(slot_of(field_of(name))) = value;
    set_bit(record_, field_of(name));
}

void record_writer::set_optional_decimal(
    std::string_view name, const std::optional<ores::utility::decimal::decimal>& value) {
    require_kind(*type_, name, field_kind::decimal, "a decimal");
    if (value)
        set_decimal(name, *value);
}

void record_writer::set_spelling(std::string_view name,
                                 std::string_view value,
                                 std::string_view what) {
    require_kind(*type_, name, field_kind::enumeration, "an enumeration");
    if (!set_enumeration(*type_, record_, name, value))
        throw std::runtime_error(std::format("'{}' is not a valid {}", value, what));
}

void record_writer::set_optional_spelling(std::string_view name,
                                          const std::optional<std::string>& value,
                                          std::string_view what) {
    require_kind(*type_, name, field_kind::enumeration, "an enumeration");
    if (value)
        set_spelling(name, *value, what);
}

void record_writer::set_flag(std::string_view name, bool value) {
    set_spelling(name, value ? "Y" : "N", "ORE boolean");
}

void record_writer::set_optional_flag(std::string_view name, const std::optional<bool>& value) {
    require_kind(*type_, name, field_kind::enumeration, "an enumeration");
    if (value)
        set_flag(name, *value);
}

void record_writer::set_own_text(std::string_view value) {
    if (type_->text_field == no_text)
        throw std::runtime_error(std::format("the {} table holds no text of its own", type_->name));
    const auto& f = type_->fields[type_->text_field];
    switch (f.kind) {
        case field_kind::text:
            *static_cast<std::string*>(slot_of(f)) = std::string(value);
            break;
        case field_kind::integer: {
            const auto digits = trim_space(value);
            auto* slot = static_cast<std::int64_t*>(slot_of(f));
            const auto [stop, ec] =
                std::from_chars(digits.data(), digits.data() + digits.size(), *slot);
            if (ec != std::errc{} || stop != digits.data() + digits.size())
                throw std::runtime_error(
                    std::format("'{}' is not an integer for the {} table", value, type_->name));
            break;
        }
        case field_kind::decimal: {
            const auto parsed = utility::decimal::decimal::from_string(trim_space(value));
            if (!parsed)
                throw std::runtime_error(std::format(
                    "'{}' is not an exact decimal for the {} table", value, type_->name));
            *static_cast<utility::decimal::decimal*>(slot_of(f)) = *parsed;
            break;
        }
        case field_kind::real: {
            const auto digits = trim_space(value);
            auto* slot = static_cast<double*>(slot_of(f));
            const auto [stop, ec] =
                std::from_chars(digits.data(), digits.data() + digits.size(), *slot);
            if (ec != std::errc{} || stop != digits.data() + digits.size())
                throw std::runtime_error(
                    std::format("'{}' is not a real number for the {} table", value, type_->name));
            break;
        }
        case field_kind::enumeration: {
            auto* slot = static_cast<std::int16_t*>(slot_of(f));
            *slot = -1;
            for (std::int16_t i = 0; f.tokens != nullptr && f.tokens[i] != nullptr; ++i) {
                if (value == f.tokens[i]) {
                    *slot = i;
                    break;
                }
            }
            if (*slot < 0)
                throw std::runtime_error(
                    std::format("'{}' is not a spelling the {} table allows", value, type_->name));
            break;
        }
        default:
            throw std::runtime_error(
                std::format("the {} table's own text is not a value", type_->name));
    }
    set_bit(record_, f);
}

auto record_reader::residuals() const -> std::vector<residual_element> {
    std::vector<residual_element> out;
    for (std::uint8_t i = 0; i < type_->field_count; ++i) {
        const auto& f = type_->fields[i];
        if (f.kind != field_kind::residual || !bit_is_set(record_, f))
            continue;
        const auto* bag = static_cast<const std::vector<residual_element>*>(slot_of(record_, f));
        out.insert(out.end(), bag->begin(), bag->end());
    }
    return out;
}

auto record_writer::child(std::string_view name) -> record_writer {
    const auto& f = require_field(*type_, name);
    if (f.kind == field_kind::column_nested) {
        if (columns_ == nullptr || f.repeated == nullptr || f.arena_offset == no_arena)
            throw std::runtime_error(std::format(
                "'{}' of the {} table names an alternative held in a column the record does not "
                "carry",
                name,
                type_->name));
        for (std::uint8_t i = 0; i < type_->field_count; ++i) {
            const auto& other = type_->fields[i];
            if (other.kind == field_kind::column_nested && bit_is_set(record_, other))
                throw std::runtime_error(std::format(
                    "'{}' of the {} table follows another alternative, and a row holds one",
                    name,
                    type_->name));
        }
        auto* rows = static_cast<std::byte*>(columns_) + f.arena_offset;
        const auto index = static_cast<std::uint32_t>(f.repeated->count(rows));
        void* slot = nullptr;
        f.repeated->append(rows, &slot);
        *static_cast<std::uint32_t*>(slot_of(f)) = index;
        set_bit(record_, f);
        return record_writer(*f.type, slot);
    }
    require_kind(*type_, name, field_kind::nested, "a child element");
    set_bit(record_, f);
    return record_writer(*f.type, slot_of(f), columns_held(record_, f));
}

void record_writer::append_value(std::string_view name, std::string_view value) {
    const auto& f = require_kind(*type_, name, field_kind::repeated_leaf, "a repeated value");
    if (f.scalar != scalar_kind::text)
        throw std::runtime_error(
            std::format("'{}' of the {} table does not hold text values", name, type_->name));
    set_bit(record_, f);
    void* slot = nullptr;
    f.repeated->append(slot_of(f), &slot);
    *static_cast<std::string*>(slot) = std::string(value);
}

void record_writer::append_spelling(std::string_view name,
                                    std::string_view value,
                                    std::string_view what) {
    const auto& f = require_kind(*type_, name, field_kind::repeated_leaf, "a repeated value");
    if (f.scalar != scalar_kind::enumeration || f.tokens == nullptr)
        throw std::runtime_error(std::format(
            "'{}' of the {} table does not hold enumerated values", name, type_->name));
    for (std::int16_t i = 0; f.tokens[i] != nullptr; ++i) {
        if (value != f.tokens[i])
            continue;
        set_bit(record_, f);
        void* slot = nullptr;
        f.repeated->append(slot_of(f), &slot);
        *static_cast<std::int16_t*>(slot) = i;
        return;
    }
    throw std::runtime_error(std::format("'{}' is not a valid {}", value, what));
}

void record_writer::append_decimal(std::string_view name,
                                   const ores::utility::decimal::decimal& value) {
    const auto& f = require_kind(*type_, name, field_kind::repeated_leaf, "a repeated value");
    if (f.scalar != scalar_kind::decimal)
        throw std::runtime_error(
            std::format("'{}' of the {} table does not hold exact decimals", name, type_->name));
    set_bit(record_, f);
    void* slot = nullptr;
    f.repeated->append(slot_of(f), &slot);
    *static_cast<ores::utility::decimal::decimal*>(slot) = value;
}

auto record_writer::append_indexed(std::string_view name) -> record_writer {
    const auto& f = require_kind(*type_, name, field_kind::indexed_nested, "an indexed child");
    if (f.repeated == nullptr || f.type == nullptr || f.arena_offset == no_arena)
        throw std::runtime_error(std::format(
            "'{}' of the {} table names an indexed member with no arena", name, type_->name));
    auto* indices = static_cast<std::vector<std::uint32_t>*>(slot_of(f));
    if (f.once && !indices->empty())
        throw std::runtime_error(
            std::format("'{}' of the {} table may appear once, and a row is already stated",
                        name,
                        type_->name));
    set_bit(record_, f);
    auto* arena = static_cast<std::byte*>(record_) + f.arena_offset;
    const auto index = static_cast<std::uint32_t>(f.repeated->count(arena));
    void* slot = nullptr;
    f.repeated->append(arena, &slot);
    indices->push_back(index);
    return record_writer(*f.type, slot, columns_held(record_, f));
}

void record_writer::append_residual(std::string_view name, std::string_view value) {
    for (std::uint8_t i = 0; i < type_->field_count; ++i) {
        const auto& f = type_->fields[i];
        if (f.kind != field_kind::residual)
            continue;
        set_bit(record_, f);
        static_cast<std::vector<residual_element>*>(slot_of(f))->push_back(
            residual_element{std::string(name), std::string(value)});
        return;
    }
    throw std::runtime_error(
        std::format("the {} table keeps no content the schema does not name", type_->name));
}

auto record_writer::append(std::string_view name) -> record_writer {
    const auto& f = require_kind(*type_, name, field_kind::repeated_nested, "a repeated child");
    set_bit(record_, f);
    void* slot = nullptr;
    f.repeated->append(slot_of(f), &slot);
    return record_writer(*f.type, slot, columns_held(record_, f));
}

}
