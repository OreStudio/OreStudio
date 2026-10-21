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
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <cstddef>
#include <string_view>
#include <vector>

namespace ores::ore::xml {

namespace {

auto present(const void* record, const field& f) -> bool {
    return bit_is_set(record, f);
}

auto slot_of(const void* record, const field& f) -> const void* {
    return static_cast<const std::byte*>(record) + f.offset;
}

/// The columns struct a record keeps for the type member `f` holds, or null.
auto columns_held(const void* record, const field& f) -> const void* {
    if (f.columns_offset == no_arena)
        return nullptr;
    return static_cast<const std::byte*>(record) + f.columns_offset;
}

auto describe(std::string_view value) -> std::string {
    return "'" + std::string(value) + "'";
}

template <typename T>
auto describe_number(const T& value) -> std::string {
    return std::to_string(value);
}

/**
 * @brief Compares one leaf value, which the row names the type of.
 */
auto compare_value(std::string& path,
                   const field& f,
                   const void* left,
                   const void* right) -> std::optional<record_difference> {
    switch (f.kind) {
        case field_kind::text: {
            const auto& lhs = *static_cast<const std::string*>(left);
            const auto& rhs = *static_cast<const std::string*>(right);
            if (lhs != rhs)
                return record_difference{path, describe(lhs) + " became " + describe(rhs)};
            return {};
        }
        case field_kind::integer: {
            const auto lhs = *static_cast<const std::int64_t*>(left);
            const auto rhs = *static_cast<const std::int64_t*>(right);
            if (lhs != rhs)
                return record_difference{path,
                                         describe_number(lhs) + " became " + describe_number(rhs)};
            return {};
        }
        case field_kind::decimal: {
            const auto& lhs = *static_cast<const utility::decimal::decimal*>(left);
            const auto& rhs = *static_cast<const utility::decimal::decimal*>(right);
            if (!(lhs == rhs))
                return record_difference{path, lhs.to_string() + " became " + rhs.to_string()};
            return {};
        }
        case field_kind::real: {
            const auto lhs = *static_cast<const double*>(left);
            const auto rhs = *static_cast<const double*>(right);
            if (lhs != rhs)
                return record_difference{path,
                                         describe_number(lhs) + " became " + describe_number(rhs)};
            return {};
        }
        case field_kind::enumeration: {
            const auto lhs = *static_cast<const std::int16_t*>(left);
            const auto rhs = *static_cast<const std::int16_t*>(right);
            if (lhs != rhs) {
                const auto spelling = [&f](std::int16_t index) -> std::string {
                    if (index < 0 || f.tokens == nullptr)
                        return std::to_string(index);
                    for (std::int16_t i = 0; i <= index; ++i)
                        if (f.tokens[i] == nullptr)
                            return std::to_string(index);
                    return f.tokens[index];
                };
                return record_difference{
                    path, describe(spelling(lhs)) + " became " + describe(spelling(rhs))};
            }
            return {};
        }
        case field_kind::nested:
        case field_kind::repeated_leaf:
        case field_kind::repeated_nested:
        case field_kind::indexed_nested:
        case field_kind::column_nested:
        case field_kind::residual:
            return record_difference{path, "a row that names no single value reached the walk"};
    }
    return record_difference{path, "a row of an unknown kind reached the walk"};
}

auto compare_record(std::string& path,
                    const record_type& type,
                    const void* left,
                    const void* right,
                    const void* left_columns,
                    const void* right_columns) -> std::optional<record_difference>;

/**
 * @brief Compares one repeated member, element by element.
 */
auto compare_repeated(std::string& path,
                      const field& f,
                      const void* left,
                      const void* right,
                      const void* left_columns,
                      const void* right_columns) -> std::optional<record_difference> {
    const auto count_left = f.repeated->count(left);
    const auto count_right = f.repeated->count(right);
    if (count_left != count_right)
        return record_difference{
            path, std::to_string(count_left) + " values became " + std::to_string(count_right)};

    for (std::size_t i = 0; i < count_left; ++i) {
        const std::string here = path + "[" + std::to_string(i) + "]";
        const auto* lhs = f.repeated->at(left, i);
        const auto* rhs = f.repeated->at(right, i);
        if (f.kind == field_kind::repeated_nested) {
            auto nested = here;
            const auto found =
                compare_record(nested, *f.type, lhs, rhs, left_columns, right_columns);
            if (found)
                return found;
            continue;
        }
        auto value = f;
        value.kind = f.scalar == scalar_kind::text    ? field_kind::text :
                     f.scalar == scalar_kind::integer ? field_kind::integer :
                     f.scalar == scalar_kind::decimal ? field_kind::decimal :
                     f.scalar == scalar_kind::real    ? field_kind::real :
                                                        field_kind::enumeration;
        auto leaf = here;
        const auto found = compare_value(leaf, value, lhs, rhs);
        if (found)
            return found;
    }
    return {};
}

auto compare_record(std::string& path,
                    const record_type& type,
                    const void* left,
                    const void* right,
                    const void* left_columns,
                    const void* right_columns) -> std::optional<record_difference> {
    for (std::uint8_t i = 0; i < type.field_count; ++i) {
        const auto& f = type.fields[i];
        const bool here = present(left, f);
        if (here != present(right, f))
            return record_difference{path,
                                     std::string(f.name) + (here ? " was lost" : " was added")};
        if (!here)
            continue;

        std::string member = path;
        if (!f.name.empty()) {
            if (!member.empty())
                member.push_back('.');
            member.append(f.name);
        }

        switch (f.kind) {
            case field_kind::nested:
                if (const auto found = compare_record(member,
                                                      *f.type,
                                                      slot_of(left, f),
                                                      slot_of(right, f),
                                                      columns_held(left, f),
                                                      columns_held(right, f)))
                    return found;
                break;
            case field_kind::repeated_leaf:
            case field_kind::repeated_nested:
                if (const auto found = compare_repeated(member,
                                                        f,
                                                        slot_of(left, f),
                                                        slot_of(right, f),
                                                        columns_held(left, f),
                                                        columns_held(right, f)))
                    return found;
                break;
            case field_kind::column_nested: {
                // Each side keeps the alternative in its own column, so the
                // rows are compared, not the indices that name them.
                if (left_columns == nullptr || right_columns == nullptr || f.repeated == nullptr ||
                    f.type == nullptr || f.arena_offset == no_arena)
                    return record_difference{member,
                                             "the record carries no column for an alternative"};
                const auto* left_rows =
                    static_cast<const std::byte*>(left_columns) + f.arena_offset;
                const auto* right_rows =
                    static_cast<const std::byte*>(right_columns) + f.arena_offset;
                const auto lhs_index = *static_cast<const std::uint32_t*>(slot_of(left, f));
                const auto rhs_index = *static_cast<const std::uint32_t*>(slot_of(right, f));
                if (lhs_index >= f.repeated->count(left_rows) ||
                    rhs_index >= f.repeated->count(right_rows))
                    return record_difference{member,
                                             "an index names a row its column does not hold"};
                if (const auto found = compare_record(member,
                                                      *f.type,
                                                      f.repeated->at(left_rows, lhs_index),
                                                      f.repeated->at(right_rows, rhs_index),
                                                      nullptr,
                                                      nullptr))
                    return found;
                break;
            }
            case field_kind::indexed_nested: {
                // The indices and the rows they name are compared in order, so
                // a difference inside a second-level row is named by its path.
                if (f.repeated == nullptr || f.type == nullptr || f.arena_offset == no_arena)
                    return record_difference{member,
                                             "the table names no arena for an indexed member"};
                const auto& left_indices =
                    *static_cast<const std::vector<std::uint32_t>*>(slot_of(left, f));
                const auto& right_indices =
                    *static_cast<const std::vector<std::uint32_t>*>(slot_of(right, f));
                if (left_indices.size() != right_indices.size())
                    return record_difference{member,
                                             std::to_string(left_indices.size()) + " rows became " +
                                                 std::to_string(right_indices.size())};
                const auto* left_arena = static_cast<const std::byte*>(left) + f.arena_offset;
                const auto* right_arena = static_cast<const std::byte*>(right) + f.arena_offset;
                const auto left_rows = f.repeated->count(left_arena);
                const auto right_rows = f.repeated->count(right_arena);
                for (std::size_t at = 0; at < left_indices.size(); ++at) {
                    const auto lhs_index = left_indices[at];
                    const auto rhs_index = right_indices[at];
                    if (lhs_index >= left_rows || rhs_index >= right_rows)
                        return record_difference{member + "[" + std::to_string(at) + "]",
                                                 "an index names a row the arena does not hold"};
                    auto nested = member + "[" + std::to_string(at) + "]";
                    if (const auto found = compare_record(nested,
                                                          *f.type,
                                                          f.repeated->at(left_arena, lhs_index),
                                                          f.repeated->at(right_arena, rhs_index),
                                                          columns_held(left, f),
                                                          columns_held(right, f)))
                        return found;
                }
                break;
            }
            case field_kind::residual: {
                const auto& lhs =
                    *static_cast<const std::vector<residual_element>*>(slot_of(left, f));
                const auto& rhs =
                    *static_cast<const std::vector<residual_element>*>(slot_of(right, f));
                if (lhs.size() != rhs.size())
                    return record_difference{member,
                                             std::to_string(lhs.size()) +
                                                 " unnamed elements became " +
                                                 std::to_string(rhs.size())};
                for (std::size_t at = 0; at < lhs.size(); ++at) {
                    if (lhs[at].name != rhs[at].name || lhs[at].text != rhs[at].text)
                        return record_difference{member + "[" + std::to_string(at) + "]",
                                                 describe(lhs[at].name + "=" + lhs[at].text) +
                                                     " became " +
                                                     describe(rhs[at].name + "=" + rhs[at].text)};
                }
                break;
            }
            default:
                if (const auto found =
                        compare_value(member, f, slot_of(left, f), slot_of(right, f)))
                    return found;
                break;
        }
    }
    return {};
}

}

auto compare_records(const record_type& type,
                     const void* left,
                     const void* right) -> std::optional<record_difference> {
    std::string path(type.name);
    return compare_record(path, type, left, right, nullptr, nullptr);
}

}
