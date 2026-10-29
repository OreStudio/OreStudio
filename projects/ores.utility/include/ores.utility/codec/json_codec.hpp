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
#include "ores.utility/serialization/error_code.hpp"
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>
#include <yyjson.h>

namespace ores::utility::codec {

/**
 * @brief The JSON form of a type: one value appended to a document, and one
 * value read back out of one.
 *
 * A type whose wire form is a string states it through =text_form= and takes
 * the implementation below; a type with another shape states its own
 * specialisation. Everything this estate puts on the wire is a string, so the
 * generic case is the one that is used.
 *
 * Writing takes the document because a value is built against it; reading takes
 * the immutable value the parser produced.
 */
template <typename T>
struct json_codec {
    static yyjson_mut_val* write(yyjson_mut_doc* doc, const T& v) {
        const auto text = text_form<T>::to_text(v);
        return yyjson_mut_strncpy(doc, text.data(), text.size());
    }

    static T read(yyjson_val* v) {
        if (!yyjson_is_str(v))
            throw std::invalid_argument("Not " + std::string(text_form<T>::wire_name) +
                                        ": the JSON value is not a string");
        return text_form<T>::from_text(std::string_view(yyjson_get_str(v), yyjson_get_len(v)));
    }
};

/**
 * @brief An error code is the one type the wire states as a number: its
 * reflector names the underlying integer, so the JSON carries the integer and
 * not its name. The sentinel past the last real value is refused, as the
 * reflector refuses it.
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
