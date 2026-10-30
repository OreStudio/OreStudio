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
#ifndef ORES_UTILITY_CODEC_JSON_CODEC_HPP
#define ORES_UTILITY_CODEC_JSON_CODEC_HPP

#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include <cstdint>
#include <limits>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>
#include <yyjson.h>

namespace ores::utility::codec {

template <typename T>
struct json_codec;

/**
 * @brief An object, over a list of fields: one value carrying each field.
 *
 * This is where the recursion lives. A field's own value is whatever its
 * member's carrier gives it, so a member that is itself a type with a wire form
 * carries another object, a vector carries an array of them and an optional
 * carries one or nothing -- no carrier knows any of those types by name.
 */
namespace detail {

template <typename T, typename... Members>
yyjson_mut_val*
write_object(yyjson_mut_doc* doc, const T& v, const std::tuple<field<T, Members>...>& fields) {
    auto* object = yyjson_mut_obj(doc);
    if (object == nullptr)
        throw std::runtime_error("Could not make a JSON object");
    std::apply(
        [&](const auto&... f) {
            (
                [&] {
                    using member_type = std::remove_cvref_t<decltype(v.*(f.member))>;
                    if constexpr (is_optional_member<member_type>) {
                        if (!(v.*(f.member)))
                            return;
                    }
                    // The key is copied into the document: the mutable API
                    // stores the pointer it is given, so a key held anywhere
                    // shorter-lived than the document would dangle between the
                    // add and the render.
                    auto* key = yyjson_mut_strncpy(doc, f.name.data(), f.name.size());
                    auto* value = json_codec<Members>::write(doc, v.*(f.member));
                    if (!yyjson_mut_obj_add(object, key, value))
                        throw std::runtime_error("Could not add " + std::string(f.name) +
                                                 " to a JSON object");
                }(),
                ...);
        },
        fields);
    return object;
}

template <typename T, typename... Members>
void read_object(yyjson_val* v, T& out, const std::tuple<field<T, Members>...>& fields) {
    if (!yyjson_is_obj(v))
        throw std::invalid_argument("Not an object");
    std::apply(
        [&](const auto&... f) {
            (
                [&] {
                    using member_type = std::remove_cvref_t<decltype(out.*(f.member))>;
                    auto* value = yyjson_obj_getn(v, f.name.data(), f.name.size());
                    if (value == nullptr) {
                        if constexpr (is_optional_member<member_type>)
                            return;
                        else
                            throw std::invalid_argument("Missing member: " + std::string(f.name));
                    }
                    out.*(f.member) = json_codec<Members>::read(value);
                }(),
                ...);
        },
        fields);
}

}

/**
 * @brief The JSON form of a type: one value appended to a document, and one
 * value read back out of one.
 *
 * A type that states its own shape through =wire_fields= is an object, and its
 * members are encoded by theirs. Every other type states its form through
 * =text_form=, or has a specialisation further down for the shape a string does
 * not cover: a number, a truth value, an absent value, an array.
 *
 * Writing takes the document because a value is built against it; reading takes
 * the immutable value the parser produced.
 */
template <typename T>
struct json_codec {
    static yyjson_mut_val* write(yyjson_mut_doc* doc, const T& v) {
        if constexpr (states_wire_form<T>) {
            return detail::write_object(doc, v, wire_fields(v));
        } else {
            const auto text = text_form<T>::to_text(v);
            return yyjson_mut_strncpy(doc, text.data(), text.size());
        }
    }

    static T read(yyjson_val* v) {
        if constexpr (states_wire_form<T>) {
            T out;
            detail::read_object(v, out, wire_fields(out));
            return out;
        } else {
            if (!yyjson_is_str(v))
                throw std::invalid_argument("Not " + std::string(text_form<T>::wire_name) +
                                            ": the JSON value is not a string");
            return text_form<T>::from_text(std::string_view(yyjson_get_str(v), yyjson_get_len(v)));
        }
    }
};

/**
 * @brief An error code travels as the underlying integer, which is the type
 * its reflector names: the JSON carries the integer and not its name. The
 * sentinel past the last real value is refused, as the reflector refuses it.
 */
template <>
struct json_codec<serialization::error_code> {
    static yyjson_mut_val* write(yyjson_mut_doc* doc, const serialization::error_code& v) {
        return yyjson_mut_uint(doc, static_cast<std::uint16_t>(v));
    }

    static serialization::error_code read(yyjson_val* v) {
        if (!yyjson_is_uint(v))
            throw std::invalid_argument("Not an error code: the JSON value is not a number");
        const auto number = yyjson_get_uint(v);
        if (number > static_cast<std::uint16_t>(serialization::error_code::limit_exceeded))
            throw std::invalid_argument("Not an error code: " + std::to_string(number));
        return static_cast<serialization::error_code>(number);
    }
};

/**
 * @brief An integer is the second shape the wire states: a number, with no
 * text in between.
 *
 * A type whose value is a number has no text form to state, and a generated
 * field list holds every member a model declares -- a version number, a
 * display order -- so the carriers have to cover numbers as well as text.
 * The number is the one reflect-cpp writes, because that is the encoding this
 * replaces.
 */
template <typename T>
    requires std::is_integral_v<T> && (!std::is_same_v<T, bool>)
struct json_codec<T> {
    static yyjson_mut_val* write(yyjson_mut_doc* doc, const T& v) {
        if constexpr (std::is_signed_v<T>)
            return yyjson_mut_sint(doc, static_cast<std::int64_t>(v));
        else
            return yyjson_mut_uint(doc, static_cast<std::uint64_t>(v));
    }

    static T read(yyjson_val* v) {
        if (!yyjson_is_int(v))
            throw std::invalid_argument("Not an integer: the JSON value is not one");
        if constexpr (std::is_signed_v<T>) {
            const auto number = yyjson_get_sint(v);
            if (number < static_cast<std::int64_t>(std::numeric_limits<T>::min()) ||
                number > static_cast<std::int64_t>(std::numeric_limits<T>::max()))
                throw std::invalid_argument("Not an integer: " + std::to_string(number) +
                                            " is out of range");
            return static_cast<T>(number);
        } else {
            if (yyjson_is_sint(v) && yyjson_get_sint(v) < 0)
                throw std::invalid_argument("Not an unsigned integer: the number is negative");
            const auto number = yyjson_get_uint(v);
            if (number > static_cast<std::uint64_t>(std::numeric_limits<T>::max()))
                throw std::invalid_argument("Not an unsigned integer: " + std::to_string(number) +
                                            " is out of range");
            return static_cast<T>(number);
        }
    }
};

/**
 * @brief A truth value is the third shape the wire states: the format's own
 * =true= and =false=, not the word.
 */
template <>
struct json_codec<bool> {
    static yyjson_mut_val* write(yyjson_mut_doc* doc, const bool& v) {
        return yyjson_mut_bool(doc, v);
    }

    static bool read(yyjson_val* v) {
        if (!yyjson_is_bool(v))
            throw std::invalid_argument("Not a truth value: the JSON value is not one");
        return yyjson_get_bool(v);
    }
};

/**
 * @brief A value that may be absent is the null of the format, which is the
 * one shape a string does not cover.
 */
template <typename T>
struct json_codec<std::optional<T>> {
    static yyjson_mut_val* write(yyjson_mut_doc* doc, const std::optional<T>& v) {
        if (!v)
            return yyjson_mut_null(doc);
        return json_codec<T>::write(doc, *v);
    }

    static std::optional<T> read(yyjson_val* v) {
        if (yyjson_is_null(v))
            return std::nullopt;
        return json_codec<T>::read(v);
    }
};

/**
 * @brief An array of values, each in its element's form.
 */
template <typename T>
struct json_codec<std::vector<T>> {
    static yyjson_mut_val* write(yyjson_mut_doc* doc, const std::vector<T>& v) {
        auto* array = yyjson_mut_arr(doc);
        for (const auto& e : v)
            yyjson_mut_arr_append(array, json_codec<T>::write(doc, e));
        return array;
    }

    static std::vector<T> read(yyjson_val* v) {
        if (!yyjson_is_arr(v))
            throw std::invalid_argument("Not an array: the JSON value is not an array");
        std::vector<T> out;
        out.reserve(yyjson_arr_size(v));
        yyjson_val* e = nullptr;
        yyjson_arr_iter iter = yyjson_arr_iter_with(v);
        while ((e = yyjson_arr_iter_next(&iter)) != nullptr)
            out.push_back(json_codec<T>::read(e));
        return out;
    }
};

/**
 * @brief The two conveniences a caller and a test reach for: a whole document
 * from a value, and a value from a whole document.
 */
namespace json {

template <typename T>
std::string write(const T& v) {
    yyjson_mut_doc* doc = yyjson_mut_doc_new(nullptr);
    if (doc == nullptr)
        throw std::runtime_error("Could not make a JSON document");
    yyjson_mut_doc_set_root(doc, json_codec<T>::write(doc, v));
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

template <typename T>
T read(std::string_view text) {
    yyjson_doc* doc = yyjson_read(text.data(), text.size(), 0);
    if (doc == nullptr)
        throw std::invalid_argument("Not JSON: " + std::string(text));
    try {
        const auto v = json_codec<T>::read(yyjson_doc_get_root(doc));
        yyjson_doc_free(doc);
        return v;
    } catch (...) {
        yyjson_doc_free(doc);
        throw;
    }
}

}

}

#endif
