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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_DATABASE_QUERY_BUILDER_JSON_HPP
#define ORES_DATABASE_QUERY_BUILDER_JSON_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <cstddef>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <yyjson.h>

namespace ores::database::query_builder {


/**
 * @brief The row as a JSON object, written from the columns the entity states.
 *
 * A repository entity is a plain aggregate of the columns a table holds, and it
 * already states them through =columns()=: a name and the member that holds it.
 * That list is enough to print the row, so a component that logs one needs no
 * second statement of the row's shape and no reflection library.
 *
 * Each member is written by the codec its type selects, which is the same writer
 * the wire uses, so a printed row and a sent row agree about every value.
 *
 * A column the row leaves unstated is left out of the object, which is what the
 * wire's object writer does, so an absent value reads back as absent either way.
 *
 * The members come out in the order the entity declares them in =columns()=,
 * which is the order the query binds them in. It is not necessarily the order
 * the struct declares them in, so a caller that compares the text against
 * another writer's output compares the values rather than the bytes.
 */
template <typename Entity>
std::string write_json(const Entity& v) {
    yyjson_mut_doc* doc = yyjson_mut_doc_new(nullptr);
    if (doc == nullptr)
        throw std::runtime_error("Could not make a JSON document");
    auto* object = yyjson_mut_obj(doc);
    std::apply(
        [&](const auto&... binding) {
            (
                [&](const auto& b) {
                    using member_type = std::remove_cvref_t<decltype(v.*(b.member))>;
                    if constexpr (ores::utility::codec::is_optional_member<member_type>) {
                        if (!(v.*(b.member)))
                            return;
                    }
                    auto* name = yyjson_mut_strncpy(doc, b.name.data(), b.name.size());
                    auto* value =
                        ores::utility::codec::json_codec<member_type>::write(doc, v.*(b.member));
                    if (!yyjson_mut_obj_add(object, name, value)) {
                        yyjson_mut_doc_free(doc);
                        throw std::runtime_error("Could not add a member to the document");
                    }
                }(binding),
                ...);
        },
        Entity::columns().items);
    yyjson_mut_doc_set_root(doc, object);
    char* text = yyjson_mut_write(doc, 0, nullptr);
    if (text == nullptr) {
        yyjson_mut_doc_free(doc);
        throw std::runtime_error("Could not render a JSON document");
    }
    std::string out(text);
    free(text);
    yyjson_mut_doc_free(doc);
    return out;
}

}

#endif
