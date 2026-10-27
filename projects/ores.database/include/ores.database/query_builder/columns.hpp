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

#include <string_view>
#include <tuple>

/*
 * The columns of a repository entity: each one's name, the member that holds
 * it, and whether it is part of the key. A generated entity states them in
 * columns(), so the query builder reads and writes a row without reflection.
 */
namespace ores::database::query_builder {

template <typename Entity, typename Member>
struct column_binding {
    std::string_view name;
    Member Entity::* member;
    bool key = false;
};

template <typename Entity, typename Member>
constexpr column_binding<Entity, Member> bind(std::string_view name, Member Entity::* member) {
    return {name, member, false};
}

template <typename Entity, typename Member>
constexpr column_binding<Entity, Member> key(std::string_view name, Member Entity::* member) {
    return {name, member, true};
}

/**
 * @brief The columns of an entity, gathered one at a time with operator|, so a
 * generated list needs no separator rule for its last entry.
 */
template <typename... Bindings>
struct column_list {
    std::tuple<Bindings...> items;
};

template <typename Entity>
constexpr column_list<> columns() {
    return {};
}

template <typename... Bindings, typename Entity, typename Member>
constexpr column_list<Bindings..., column_binding<Entity, Member>>
operator|(column_list<Bindings...> list, column_binding<Entity, Member> b) {
    return {std::tuple_cat(list.items, std::make_tuple(b))};
}

}

#endif
