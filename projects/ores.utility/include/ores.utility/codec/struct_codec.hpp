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
#ifndef ORES_UTILITY_CODEC_STRUCT_CODEC_HPP
#define ORES_UTILITY_CODEC_STRUCT_CODEC_HPP

#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/msgpack_codec.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>

namespace ores::utility::codec {

/**
 * @brief A struct in either format, as a whole value: the two conveniences a
 * caller and a test reach for.
 *
 * A type that states its own shape through =wire_fields= needs nothing here --
 * the carriers find it. These overloads take a list explicitly, for a caller
 * holding one and for a type whose shape is stated somewhere other than its own
 * header.
 */
namespace object {

template <typename T, typename... Members>
std::string write_json(const T& v, const std::tuple<field<T, Members>...>& fields) {
    yyjson_mut_doc* doc = yyjson_mut_doc_new(nullptr);
    if (doc == nullptr)
        throw std::runtime_error("Could not make a JSON document");
    yyjson_mut_doc_set_root(doc, detail::write_object(doc, v, fields));
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
std::string write_json(const T& v) {
    return write_json(v, wire_fields(v));
}

template <typename T, typename... Members>
T read_json(std::string_view text, const std::tuple<field<T, Members>...>& fields) {
    yyjson_doc* doc = yyjson_read(text.data(), text.size(), 0);
    if (doc == nullptr)
        throw std::invalid_argument("Not JSON: " + std::string(text));
    try {
        T out;
        detail::read_object(yyjson_doc_get_root(doc), out, fields);
        yyjson_doc_free(doc);
        return out;
    } catch (...) {
        yyjson_doc_free(doc);
        throw;
    }
}

template <typename T>
T read_json(std::string_view text) {
    return json::read<T>(text);
}

template <typename T, typename... Members>
std::string write_msgpack(const T& v, const std::tuple<field<T, Members>...>& fields) {
    msgpack_sbuffer sbuf;
    msgpack_sbuffer_init(&sbuf);
    msgpack_packer pk;
    msgpack_packer_init(&pk, &sbuf, msgpack_sbuffer_write);
    try {
        detail::write_object(&pk, v, fields);
    } catch (...) {
        msgpack_sbuffer_destroy(&sbuf);
        throw;
    }
    std::string out(sbuf.data, sbuf.size);
    msgpack_sbuffer_destroy(&sbuf);
    return out;
}

template <typename T>
std::string write_msgpack(const T& v) {
    return write_msgpack(v, wire_fields(v));
}

template <typename T, typename... Members>
T read_msgpack(std::span<const std::byte> data, const std::tuple<field<T, Members>...>& fields) {
    msgpack_unpacked result;
    msgpack_unpacked_init(&result);
    std::size_t offset = 0;
    if (!msgpack_unpack_next(
            &result, reinterpret_cast<const char*>(data.data()), data.size(), &offset) ||
        offset != data.size()) {
        msgpack_unpacked_destroy(&result);
        throw std::invalid_argument("Not one MessagePack value");
    }
    try {
        T out;
        detail::read_object(result.data, out, fields);
        msgpack_unpacked_destroy(&result);
        return out;
    } catch (...) {
        msgpack_unpacked_destroy(&result);
        throw;
    }
}

template <typename T>
T read_msgpack(std::span<const std::byte> data) {
    return msgpack::read<T>(data);
}

}

}

#endif
