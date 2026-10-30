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
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::utility::codec {

/**
 * @brief One member of a struct, named as it is on the wire.
 *
 * The generated code states its type's members as a list of these, so the
 * shape of the object is in the generator's hands and the encoding is in this
 * one. A field's own form is the carrier's for its member's type, so a nested
 * generated type recurses into its own functions.
 */
template <typename T, typename Member>
struct field {
    std::string_view name;
    Member T::* member;
};

/**
 * @brief The two formats, over a list of fields: an object whose members are
 * the fields, in the order the list states them.
 */
namespace object {

template <typename T, typename... Members>
std::string write_json(const T& v, const std::tuple<field<T, Members>...>& fields) {
    yyjson_mut_doc* doc = yyjson_mut_doc_new(nullptr);
    if (doc == nullptr)
        throw std::runtime_error("Could not make a JSON document");
    auto* root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);
    std::apply(
        [&](const auto&... f) {
            (yyjson_mut_obj_add_val(doc,
                                    root,
                                    std::string(f.name).c_str(),
                                    json_codec<Members>::write(doc, v.*(f.member))),
             ...);
        },
        fields);
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

template <typename T, typename... Members>
T read_json(std::string_view text, const std::tuple<field<T, Members>...>& fields) {
    yyjson_doc* doc = yyjson_read(text.data(), text.size(), 0);
    if (doc == nullptr)
        throw std::invalid_argument("Not JSON: " + std::string(text));
    try {
        auto* root = yyjson_doc_get_root(doc);
        if (!yyjson_is_obj(root))
            throw std::invalid_argument("Not an object");
        T out;
        std::apply(
            [&](const auto&... f) {
                (
                    [&] {
                        auto* value = yyjson_obj_getn(root, f.name.data(), f.name.size());
                        if (value == nullptr)
                            throw std::invalid_argument("Missing member: " + std::string(f.name));
                        out.*(f.member) = json_codec<Members>::read(value);
                    }(),
                    ...);
            },
            fields);
        yyjson_doc_free(doc);
        return out;
    } catch (...) {
        yyjson_doc_free(doc);
        throw;
    }
}

template <typename T, typename... Members>
std::string write_msgpack(const T& v, const std::tuple<field<T, Members>...>& fields) {
    msgpack_sbuffer sbuf;
    msgpack_sbuffer_init(&sbuf);
    msgpack_packer pk;
    msgpack_packer_init(&pk, &sbuf, msgpack_sbuffer_write);
    try {
        if (msgpack_pack_map(&pk, sizeof...(Members)) != 0)
            throw std::runtime_error("Could not pack an object");
        std::apply(
            [&](const auto&... f) {
                (
                    [&] {
                        msgpack_codec<std::string>::write(&pk, std::string(f.name));
                        msgpack_codec<Members>::write(&pk, v.*(f.member));
                    }(),
                    ...);
            },
            fields);
    } catch (...) {
        msgpack_sbuffer_destroy(&sbuf);
        throw;
    }
    std::string out(sbuf.data, sbuf.size);
    msgpack_sbuffer_destroy(&sbuf);
    return out;
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
        const auto& v = result.data;
        if (v.type != MSGPACK_OBJECT_MAP)
            throw std::invalid_argument("Not an object");
        T out;
        std::apply(
            [&](const auto&... f) {
                (
                    [&] {
                        bool found = false;
                        for (std::uint32_t i = 0; i < v.via.map.size; ++i) {
                            const auto& key = v.via.map.ptr[i].key;
                            if (key.type != MSGPACK_OBJECT_STR)
                                continue;
                            if (std::string_view(key.via.str.ptr, key.via.str.size) != f.name)
                                continue;
                            out.*(f.member) = msgpack_codec<Members>::read(v.via.map.ptr[i].val);
                            found = true;
                            break;
                        }
                        if (!found)
                            throw std::invalid_argument("Missing member: " + std::string(f.name));
                    }(),
                    ...);
            },
            fields);
        msgpack_unpacked_destroy(&result);
        return out;
    } catch (...) {
        msgpack_unpacked_destroy(&result);
        throw;
    }
}

}

}

#endif
