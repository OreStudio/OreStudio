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
#include <format>
#include <stdexcept>

namespace ores::ore::xml {

namespace {

/// The record_type states no text of its own with this index.
constexpr std::uint8_t no_text = 0xFF;

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
    }
    return "an unknown shape";
}

const field& require_field(const record_type& type, std::string_view name) {
    const auto* found = find_field(type, name, false);
    if (found == nullptr)
        found = find_field(type, name, true);
    if (found == nullptr)
        throw std::runtime_error(
            std::format("the {} table has no '{}' field", type.name, name));
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

/// The presence mask is the first member of every record the table describes.
auto present(const void* record, const field& f) -> bool {
    return (*static_cast<const std::uint64_t*>(record) & (1ull << f.bit)) != 0;
}

void mark(void* record, const field& f) {
    *static_cast<std::uint64_t*>(record) |= (1ull << f.bit);
}

const void* slot_of(const void* record, const field& f) {
    return static_cast<const std::byte*>(record) + f.offset;
}

}

record_reader::record_reader(const record_type& type, const void* record)
    : type_(&type)
    , record_(record) {}

auto record_reader::type() const -> const record_type& { return *type_; }

auto record_reader::has(std::string_view name) const -> bool {
    return present(record_, require_field(*type_, name));
}

auto record_reader::text(std::string_view name) const -> std::optional<std::string> {
    const auto& f = require_kind(*type_, name, field_kind::text, "text");
    if (!present(record_, f))
        return std::nullopt;
    return *static_cast<const std::string*>(slot_of(record_, f));
}

auto record_reader::integer(std::string_view name) const -> std::optional<std::int64_t> {
    const auto& f = require_kind(*type_, name, field_kind::integer, "an integer");
    if (!present(record_, f))
        return std::nullopt;
    return *static_cast<const std::int64_t*>(slot_of(record_, f));
}

auto record_reader::real(std::string_view name) const -> std::optional<double> {
    const auto& f = require_kind(*type_, name, field_kind::real, "a real number");
    if (!present(record_, f))
        return std::nullopt;
    return *static_cast<const double*>(slot_of(record_, f));
}

auto record_reader::decimal(std::string_view name) const
    -> std::optional<ores::utility::decimal::decimal> {
    const auto& f = require_kind(*type_, name, field_kind::decimal, "a decimal");
    if (!present(record_, f))
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
    if (!present(record_, f))
        return std::nullopt;
    return *static_cast<const std::string*>(slot_of(record_, f));
}

auto record_reader::child(std::string_view name) const -> std::optional<record_reader> {
    const auto& f = require_kind(*type_, name, field_kind::nested, "a child element");
    if (!present(record_, f) || f.type == nullptr)
        return std::nullopt;
    return record_reader(*f.type, slot_of(record_, f));
}

auto record_reader::children(std::string_view name) const -> std::vector<record_reader> {
    const auto& f = require_kind(*type_, name, field_kind::repeated_nested, "a repeated child");
    std::vector<record_reader> out;
    if (!present(record_, f) || f.repeated == nullptr || f.type == nullptr)
        return out;
    const auto count = f.repeated->count(slot_of(record_, f));
    out.reserve(count);
    for (std::size_t i = 0; i < count; ++i)
        out.emplace_back(*f.type, f.repeated->at(slot_of(record_, f), i));
    return out;
}

record_writer::record_writer(const record_type& type, void* record)
    : type_(&type)
    , record_(record) {}

auto record_writer::type() const -> const record_type& { return *type_; }

auto record_writer::field_of(std::string_view name) const -> const field& {
    return require_field(*type_, name);
}

auto record_writer::slot_of(const field& f) const -> void* {
    return static_cast<std::byte*>(record_) + f.offset;
}

void record_writer::state(std::string_view name) {
    mark(record_, require_field(*type_, name));
}

void record_writer::set_text(std::string_view name, std::string_view value) {
    require_kind(*type_, name, field_kind::text, "text");
    *static_cast<std::string*>(slot_of(field_of(name))) = std::string(value);
    mark(record_, field_of(name));
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
    mark(record_, field_of(name));
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
    mark(record_, field_of(name));
}

void record_writer::set_optional_real(std::string_view name,
                                      const std::optional<double>& value) {
    require_kind(*type_, name, field_kind::real, "a real number");
    if (value)
        set_real(name, *value);
}

void record_writer::set_decimal(std::string_view name,
                                const ores::utility::decimal::decimal& value) {
    require_kind(*type_, name, field_kind::decimal, "a decimal");
    *static_cast<ores::utility::decimal::decimal*>(slot_of(field_of(name))) = value;
    mark(record_, field_of(name));
}

void record_writer::set_optional_decimal(
    std::string_view name, const std::optional<ores::utility::decimal::decimal>& value) {
    require_kind(*type_, name, field_kind::decimal, "a decimal");
    if (value)
        set_decimal(name, *value);
}

void record_writer::set_spelling(std::string_view name, std::string_view value, std::string_view what) {
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

void record_writer::set_optional_flag(std::string_view name,
                                      const std::optional<bool>& value) {
    require_kind(*type_, name, field_kind::enumeration, "an enumeration");
    if (value)
        set_flag(name, *value);
}

void record_writer::set_own_text(std::string_view value) {
    if (type_->text_field == no_text)
        throw std::runtime_error(
            std::format("the {} table holds no text of its own", type_->name));
    const auto& f = type_->fields[type_->text_field];
    *static_cast<std::string*>(slot_of(f)) = std::string(value);
    mark(record_, f);
}

auto record_writer::child(std::string_view name) -> record_writer {
    const auto& f = require_kind(*type_, name, field_kind::nested, "a child element");
    mark(record_, f);
    return record_writer(*f.type, slot_of(f));
}

auto record_writer::append(std::string_view name) -> record_writer {
    const auto& f =
        require_kind(*type_, name, field_kind::repeated_nested, "a repeated child");
    mark(record_, f);
    void* slot = nullptr;
    f.repeated->append(slot_of(f), &slot);
    return record_writer(*f.type, slot);
}

}
