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
#ifndef ORES_DATABASE_QUERY_BUILDER_COLUMNS_HPP
#define ORES_DATABASE_QUERY_BUILDER_COLUMNS_HPP

#include <cstddef>
#include <string_view>
#include <utility>

/*
 * The columns of a repository entity: each one's name, the member that holds
 * it, and whether it is part of the key. A generated entity states them in
 * columns(), so the query builder reads and writes a row without reflection.
 *
 * A column is binary unless it was stated as text. A binary column travels in
 * the format its member's codec defines and the statement asks the server for
 * it as it is stored. A text column is one the layer has no codec for, such as
 * jsonb or inet, or one the row keeps in a type the database does not store:
 * the statement asks the server for it cast to text, and the row reads it
 * through the text form.
 */
namespace ores::database::query_builder {

template <typename Entity, typename Member>
struct column_binding {
    std::string_view name;
    Member Entity::* member;
    bool key = false;
    static constexpr bool as_text = false;
};

template <typename Entity, typename Member>
struct text_column_binding {
    std::string_view name;
    Member Entity::* member;
    bool key = false;
    static constexpr bool as_text = true;
};

template <typename Entity, typename Member>
constexpr column_binding<Entity, Member> bind(std::string_view name, Member Entity::* member) {
    return {name, member, false};
}

/**
 * @brief A column the row holds as its text, such as jsonb or inet.
 */
template <typename Entity, typename Member>
constexpr text_column_binding<Entity, Member> bind_text(std::string_view name,
                                                        Member Entity::* member) {
    return {name, member, false};
}

template <typename Entity, typename Member>
constexpr column_binding<Entity, Member> key(std::string_view name, Member Entity::* member) {
    return {name, member, true};
}

/**
 * @brief A key column the row holds as its text, such as a uuid the entity
 * keeps as a string.
 */
template <typename Entity, typename Member>
constexpr text_column_binding<Entity, Member> key_text(std::string_view name,
                                                       Member Entity::* member) {
    return {name, member, true};
}

namespace detail {

template <std::size_t Index, typename Binding>
struct column_slot {
    Binding binding;
};

/*
 * Every slot is a direct base, so a list of n columns instantiates n slots and
 * nothing deeper. A std::tuple grown one column at a time instantiates every
 * prefix, and the MSVC library's recursive tuple makes that quadratic: both
 * Windows compilers gave up on the larger entities.
 */
template <typename Indices, typename... Bindings>
struct column_slots;

template <std::size_t... Indices, typename... Bindings>
struct column_slots<std::index_sequence<Indices...>, Bindings...>
    : column_slot<Indices, Bindings>... {};

}

/**
 * @brief The columns of an entity, gathered one at a time with operator|, so a
 * generated list needs no separator rule for its last entry.
 */
template <typename... Bindings>
struct column_list {
    detail::column_slots<std::index_sequence_for<Bindings...>, Bindings...> slots;

    /**
     * @brief Calls f with every binding, in the order the entity states them.
     */
    template <typename F>
    constexpr decltype(auto) apply(F&& f) const {
        return apply_at(std::forward<F>(f), std::index_sequence_for<Bindings...>{});
    }

    template <typename Binding>
    constexpr column_list<Bindings..., Binding> append(Binding b) const {
        return append_at(b, std::index_sequence_for<Bindings...>{});
    }

private:
    template <typename F, std::size_t... Indices>
    constexpr decltype(auto) apply_at(F&& f, std::index_sequence<Indices...>) const {
        return std::forward<F>(f)(
            static_cast<const detail::column_slot<Indices, Bindings>&>(slots).binding...);
    }

    template <typename Binding, std::size_t... Indices>
    constexpr column_list<Bindings..., Binding> append_at(Binding b,
                                                          std::index_sequence<Indices...>) const {
        return {
            {{static_cast<const detail::column_slot<Indices, Bindings>&>(slots).binding}..., {b}}};
    }
};

template <typename Entity>
constexpr column_list<> columns() {
    return {};
}

template <typename... Bindings, typename Binding>
constexpr column_list<Bindings..., Binding> operator|(const column_list<Bindings...>& list,
                                                      Binding b) {
    return list.append(b);
}

}

#endif
