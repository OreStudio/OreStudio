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
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include <cstddef>
#include <limits>
#include <msgpack.h>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <vector>

namespace ores::utility::codec {

template <typename T>
struct msgpack_codec;

/**
 * @brief An object, over a list of fields: one map carrying each field.
 *
 * The contract is the JSON carrier's, one format over, recursion included: a
 * field's own value is whatever its member's carrier gives it, so a member that
 * is itself a type with a wire form carries another map, a vector carries an
 * array of them and an optional carries one or nil.
 */
namespace detail {

template <typename T, typename... Members>
void write_object(msgpack_packer* pk, const T& v, const std::tuple<field<T, Members>...>& fields) {
    if (msgpack_pack_map(pk, sizeof...(Members)) != 0)
        throw std::runtime_error("Could not pack an object");
    std::apply(
        [&](const auto&... f) {
            (
                [&] {
                    const auto key = std::string(f.name);
                    if (msgpack_pack_str(pk, key.size()) != 0 ||
                        msgpack_pack_str_body(pk, key.data(), key.size()) != 0)
                        throw std::runtime_error("Could not pack a member name");
                    msgpack_codec<Members>::write(pk, v.*(f.member));
                }(),
                ...);
        },
        fields);
}

template <typename T, typename... Members>
void read_object(const msgpack_object& v, T& out, const std::tuple<field<T, Members>...>& fields) {
    if (v.type != MSGPACK_OBJECT_MAP)
        throw std::invalid_argument("Not an object");
    std::apply(
        [&](const auto&... f) {
            (
                [&] {
                    using member_type = std::remove_cvref_t<decltype(out.*(f.member))>;
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
                    if (!found) {
                        if constexpr (is_optional_member<member_type>)
                            return;
                        else
                            throw std::invalid_argument("Missing member: " + std::string(f.name));
                    }
                }(),
                ...);
        },
        fields);
}

}

/**
 * @brief The MessagePack form of a type: one value packed into a buffer, and
 * one value read back out of a parsed object.
 *
 * A type that states its own shape through =wire_fields= is an object, and its
 * members are encoded by theirs. Every other type states its form through
 * =text_form=, or has a specialisation further down for the shape a string does
 * not cover. The text is the same text, so the two formats cannot disagree
 * about a value -- only about how they frame it.
 */
template <typename T>
struct msgpack_codec {
    static void write(msgpack_packer* pk, const T& v) {
        if constexpr (states_wire_form<T>) {
            detail::write_object(pk, v, wire_fields(v));
        } else {
            const auto text = text_form<T>::to_text(v);
            if (msgpack_pack_str(pk, text.size()) != 0 ||
                msgpack_pack_str_body(pk, text.data(), text.size()) != 0)
                throw std::runtime_error("Could not pack " + std::string(text_form<T>::wire_name));
        }
    }

    static T read(const msgpack_object& v) {
        if constexpr (states_wire_form<T>) {
            T out;
            detail::read_object(v, out, wire_fields(out));
            return out;
        } else {
            if (v.type != MSGPACK_OBJECT_STR)
                throw std::invalid_argument("Not " + std::string(text_form<T>::wire_name) +
                                            ": the MessagePack value is not a string");
            return text_form<T>::from_text(std::string_view(v.via.str.ptr, v.via.str.size));
        }
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
 * @brief An integer is a number here too, packed in the frame that holds it.
 *
 * The packer picks the narrowest of the integer frames for the value, which is
 * what the reflector this replaces asks of the same packer, so the two agree
 * byte for byte rather than only in value.
 */
template <typename T>
    requires std::is_integral_v<T> && (!std::is_same_v<T, bool>)
struct msgpack_codec<T> {
    static void write(msgpack_packer* pk, const T& v) {
        if constexpr (std::is_signed_v<T>) {
            if (msgpack_pack_int64(pk, static_cast<std::int64_t>(v)) != 0)
                throw std::runtime_error("Could not pack an integer");
        } else {
            if (msgpack_pack_uint64(pk, static_cast<std::uint64_t>(v)) != 0)
                throw std::runtime_error("Could not pack an unsigned integer");
        }
    }

    static T read(const msgpack_object& v) {
        const bool positive = v.type == MSGPACK_OBJECT_POSITIVE_INTEGER;
        if (!positive && v.type != MSGPACK_OBJECT_NEGATIVE_INTEGER)
            throw std::invalid_argument("Not an integer: the MessagePack value is not one");
        if constexpr (std::is_signed_v<T>) {
            if (positive) {
                if (v.via.u64 > static_cast<std::uint64_t>(std::numeric_limits<T>::max()))
                    throw std::invalid_argument("Not an integer: it is out of range");
                return static_cast<T>(v.via.u64);
            }
            if (v.via.i64 < static_cast<std::int64_t>(std::numeric_limits<T>::min()))
                throw std::invalid_argument("Not an integer: it is out of range");
            return static_cast<T>(v.via.i64);
        } else {
            if (!positive)
                throw std::invalid_argument("Not an unsigned integer: the number is negative");
            if (v.via.u64 > static_cast<std::uint64_t>(std::numeric_limits<T>::max()))
                throw std::invalid_argument("Not an unsigned integer: it is out of range");
            return static_cast<T>(v.via.u64);
        }
    }
};

/**
 * @brief A truth value is the format's own =true= and =false=.
 */
template <>
struct msgpack_codec<bool> {
    static void write(msgpack_packer* pk, const bool& v) {
        if ((v ? msgpack_pack_true(pk) : msgpack_pack_false(pk)) != 0)
            throw std::runtime_error("Could not pack a truth value");
    }

    static bool read(const msgpack_object& v) {
        if (v.type != MSGPACK_OBJECT_BOOLEAN)
            throw std::invalid_argument("Not a truth value: the MessagePack value is not one");
        return v.via.boolean;
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
    if (offset != text.size()) {
        msgpack_unpacked_destroy(&result);
        throw std::invalid_argument("Not one MessagePack value: " + std::to_string(offset) +
                                    " of " + std::to_string(text.size()) + " bytes read");
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

/**
 * @brief The same, over the bytes a caller holds rather than over text.
 *
 * MessagePack is bytes, so this is the form most callers have; the text one
 * above is the same thing with the characters already.
 */
template <typename T>
T read(std::span<const std::byte> data) {
    return read<T>(std::string_view(reinterpret_cast<const char*>(data.data()), data.size()));
}

}

}

#endif
