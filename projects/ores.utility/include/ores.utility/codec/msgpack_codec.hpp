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
#ifndef ORES_UTILITY_CODEC_MSGPACK_CODEC_HPP
#define ORES_UTILITY_CODEC_MSGPACK_CODEC_HPP

#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include <cstddef>
#include <msgpack.h>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::utility::codec {

/**
 * @brief The MessagePack form of a type: one value packed into a buffer, and
 * one value read back out of a parsed object.
 *
 * The contract is the JSON carrier's, one format over: a type whose wire form is
 * a string states it through =text_form= and takes the implementation below.
 * The text is the same text, so the two formats cannot disagree about a value --
 * only about how they frame it.
 */
template <typename T>
struct msgpack_codec {
    static void write(msgpack_packer* pk, const T& v) {
        const auto text = text_form<T>::to_text(v);
        if (msgpack_pack_str(pk, text.size()) != 0 ||
            msgpack_pack_str_body(pk, text.data(), text.size()) != 0)
            throw std::runtime_error("Could not pack " + std::string(text_form<T>::wire_name));
    }

    static T read(const msgpack_object& v) {
        if (v.type != MSGPACK_OBJECT_STR)
            throw std::invalid_argument("Not " + std::string(text_form<T>::wire_name) +
                                        ": the MessagePack value is not a string");
        return text_form<T>::from_text(std::string_view(v.via.str.ptr, v.via.str.size));
    }
};

/**
 * @brief An error code travels as the underlying integer, which is what its
 * reflector names, and the sentinel past the last real value is refused.
 */
template <>
struct msgpack_codec<serialization::error_code> {
    static void write(msgpack_packer* pk, const serialization::error_code& v) {
        if (msgpack_pack_uint16(pk, static_cast<std::uint16_t>(v)) != 0)
            throw std::runtime_error("Could not pack an error code");
    }

    static serialization::error_code read(const msgpack_object& v) {
        if (v.type != MSGPACK_OBJECT_POSITIVE_INTEGER)
            throw std::invalid_argument("Not an error code: the MessagePack value is not a number");
        if (v.via.u64 > static_cast<std::uint64_t>(serialization::error_code::limit_exceeded))
            throw std::invalid_argument("Not an error code: " + std::to_string(v.via.u64));
        return static_cast<serialization::error_code>(v.via.u64);
    }
};

/**
 * @brief A value that may be absent is the format's nil.
 */
template <typename T>
struct msgpack_codec<std::optional<T>> {
    static void write(msgpack_packer* pk, const std::optional<T>& v) {
        if (!v) {
            if (msgpack_pack_nil(pk) != 0)
                throw std::runtime_error("Could not pack nil");
            return;
        }
        msgpack_codec<T>::write(pk, *v);
    }

    static std::optional<T> read(const msgpack_object& v) {
        if (v.type == MSGPACK_OBJECT_NIL)
            return std::nullopt;
        return msgpack_codec<T>::read(v);
    }
};

/**
 * @brief An array of values, each in its element's form.
 */
template <typename T>
struct msgpack_codec<std::vector<T>> {
    static void write(msgpack_packer* pk, const std::vector<T>& v) {
        if (msgpack_pack_array(pk, v.size()) != 0)
            throw std::runtime_error("Could not pack an array");
        for (const auto& e : v)
            msgpack_codec<T>::write(pk, e);
    }

    static std::vector<T> read(const msgpack_object& v) {
        if (v.type != MSGPACK_OBJECT_ARRAY)
            throw std::invalid_argument("Not an array: the MessagePack value is not one");
        std::vector<T> out;
        out.reserve(v.via.array.size);
        for (std::uint32_t i = 0; i < v.via.array.size; ++i)
            out.push_back(msgpack_codec<T>::read(v.via.array.ptr[i]));
        return out;
    }
};

/**
 * @brief The two conveniences a caller and a test reach for: a whole buffer
 * from a value, and a value from a whole buffer.
 */
namespace msgpack {

template <typename T>
std::string write(const T& v) {
    msgpack_sbuffer sbuf;
    msgpack_sbuffer_init(&sbuf);
    msgpack_packer pk;
    msgpack_packer_init(&pk, &sbuf, msgpack_sbuffer_write);
    try {
        msgpack_codec<T>::write(&pk, v);
    } catch (...) {
        msgpack_sbuffer_destroy(&sbuf);
        throw;
    }
    std::string out(sbuf.data, sbuf.size);
    msgpack_sbuffer_destroy(&sbuf);
    return out;
}

template <typename T>
T read(std::string_view text) {
    msgpack_unpacked result;
    msgpack_unpacked_init(&result);
    std::size_t offset = 0;
    if (!msgpack_unpack_next(&result, text.data(), text.size(), &offset)) {
        msgpack_unpacked_destroy(&result);
        throw std::invalid_argument("Not a MessagePack value");
    }
    try {
        const auto v = msgpack_codec<T>::read(result.data);
        msgpack_unpacked_destroy(&result);
        return v;
    } catch (...) {
        msgpack_unpacked_destroy(&result);
        throw;
    }
}

}

}

#endif
