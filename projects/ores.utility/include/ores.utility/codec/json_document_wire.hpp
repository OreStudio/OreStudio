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
#ifndef ORES_UTILITY_CODEC_JSON_DOCUMENT_WIRE_HPP
#define ORES_UTILITY_CODEC_JSON_DOCUMENT_WIRE_HPP

#include "ores.utility/codec/json_document.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <expected>
#include <msgpack.h>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::utility::codec {

/**
 * @file json_document_wire.hpp
 * @brief A document with no shape on the wire, in both formats.
 *
 * A message whose members a client learns only at run time cannot be a typed
 * message, so the NATS codec needs a value it can encode whichever format the
 * process speaks. This bridges a document to MessagePack and states the wire
 * traits that hand it to the same codec a typed message uses.
 */

namespace document_wire_detail {

inline void pack_value(msgpack_packer* packer, const json_value& value) {
    if (value.is_object()) {
        msgpack_pack_map(packer, value.size());
        for (const auto& [name, member] : value.members()) {
            msgpack_pack_str(packer, name.size());
            msgpack_pack_str_body(packer, name.data(), name.size());
            pack_value(packer, member);
        }
        return;
    }
    if (value.is_array()) {
        msgpack_pack_array(packer, value.size());
        for (const auto& element : value.elements())
            pack_value(packer, element);
        return;
    }
    if (value.is_string()) {
        const auto text = value.text_or();
        msgpack_pack_str(packer, text.size());
        msgpack_pack_str_body(packer, text.data(), text.size());
        return;
    }
    if (value.is_bool()) {
        value.flag().value_or(false) ? msgpack_pack_true(packer) : msgpack_pack_false(packer);
        return;
    }
    if (value.is_number()) {
        if (value.is_integer()) {
            if (const auto whole = value.integer())
                msgpack_pack_int64(packer, *whole);
        } else if (const auto real = value.real()) {
            msgpack_pack_double(packer, *real);
        }
        return;
    }
    msgpack_pack_nil(packer);
}

/// The key of one entry of a MessagePack map, as text.
inline std::string_view key_of(const msgpack_object& key) {
    if (key.type != MSGPACK_OBJECT_STR)
        return {};
    return std::string_view(key.via.str.ptr, key.via.str.size);
}

inline void fill_element(const msgpack_object& value, json_value out);

inline void fill_member(const msgpack_object& value, json_value parent, std::string_view name) {
    switch (value.type) {
        case MSGPACK_OBJECT_MAP: {
            auto child = parent.add_object(name);
            for (std::uint32_t i = 0; i < value.via.map.size; ++i) {
                const auto& entry = value.via.map.ptr[i];
                fill_member(entry.val, child, key_of(entry.key));
            }
            return;
        }
        case MSGPACK_OBJECT_ARRAY: {
            auto child = parent.add_array(name);
            for (std::uint32_t i = 0; i < value.via.array.size; ++i)
                fill_element(value.via.array.ptr[i], child);
            return;
        }
        case MSGPACK_OBJECT_STR:
            parent.set(name, std::string_view(value.via.str.ptr, value.via.str.size));
            return;
        case MSGPACK_OBJECT_POSITIVE_INTEGER:
            parent.set(name, static_cast<std::int64_t>(value.via.u64));
            return;
        case MSGPACK_OBJECT_NEGATIVE_INTEGER:
            parent.set(name, static_cast<std::int64_t>(value.via.i64));
            return;
        case MSGPACK_OBJECT_FLOAT:
            parent.set(name, value.via.f64);
            return;
        case MSGPACK_OBJECT_BOOLEAN:
            parent.set(name, value.via.boolean != 0);
            return;
        case MSGPACK_OBJECT_NIL:
            parent.set_null(name);
            return;
        default:
            // Anything a document has no kind for stays unstated.
            return;
    }
}

inline void fill_element(const msgpack_object& value, json_value out) {
    switch (value.type) {
        case MSGPACK_OBJECT_MAP: {
            auto child = out.push_object();
            for (std::uint32_t i = 0; i < value.via.map.size; ++i) {
                const auto& entry = value.via.map.ptr[i];
                fill_member(entry.val, child, key_of(entry.key));
            }
            return;
        }
        case MSGPACK_OBJECT_ARRAY: {
            auto child = out.push_array();
            for (std::uint32_t i = 0; i < value.via.array.size; ++i)
                fill_element(value.via.array.ptr[i], child);
            return;
        }
        case MSGPACK_OBJECT_STR:
            out.push(std::string_view(value.via.str.ptr, value.via.str.size));
            return;
        case MSGPACK_OBJECT_POSITIVE_INTEGER:
            out.push(static_cast<std::int64_t>(value.via.u64));
            return;
        case MSGPACK_OBJECT_NEGATIVE_INTEGER:
            out.push(static_cast<std::int64_t>(value.via.i64));
            return;
        case MSGPACK_OBJECT_FLOAT:
            out.push(value.via.f64);
            return;
        case MSGPACK_OBJECT_BOOLEAN:
            out.push(value.via.boolean != 0);
            return;
        case MSGPACK_OBJECT_NIL:
            out.push_null();
            return;
        default:
            return;
    }
}

}

/**
 * @brief The document as MessagePack bytes.
 */
[[nodiscard]] inline std::string write_document_msgpack(const json_document& document) {
    msgpack_sbuffer buffer;
    msgpack_sbuffer_init(&buffer);
    msgpack_packer packer;
    msgpack_packer_init(&packer, &buffer, msgpack_sbuffer_write);
    document_wire_detail::pack_value(&packer, document.root());
    std::string out(buffer.data, buffer.size);
    msgpack_sbuffer_destroy(&buffer);
    return out;
}

/**
 * @brief The document the MessagePack bytes state, or why they state none.
 */
[[nodiscard]] inline std::expected<json_document, std::string>
read_document_msgpack(std::span<const std::byte> data) {
    msgpack_unpacked unpacked;
    msgpack_unpacked_init(&unpacked);
    std::size_t offset = 0;
    if (!msgpack_unpack_next(
            &unpacked, reinterpret_cast<const char*>(data.data()), data.size(), &offset)) {
        msgpack_unpacked_destroy(&unpacked);
        return std::unexpected(std::string("Not a MessagePack value"));
    }
    if (offset != data.size()) {
        msgpack_unpacked_destroy(&unpacked);
        return std::unexpected(std::string("Not one MessagePack value"));
    }

    const auto& value = unpacked.data;
    if (value.type == MSGPACK_OBJECT_MAP) {
        auto document = json_document::make_object();
        for (std::uint32_t i = 0; i < value.via.map.size; ++i) {
            const auto& entry = value.via.map.ptr[i];
            document_wire_detail::fill_member(
                entry.val, document.root(), document_wire_detail::key_of(entry.key));
        }
        msgpack_unpacked_destroy(&unpacked);
        return document;
    }
    if (value.type == MSGPACK_OBJECT_ARRAY) {
        auto document = json_document::make_array();
        for (std::uint32_t i = 0; i < value.via.array.size; ++i)
            document_wire_detail::fill_element(value.via.array.ptr[i], document.root());
        msgpack_unpacked_destroy(&unpacked);
        return document;
    }
    msgpack_unpacked_destroy(&unpacked);
    return std::unexpected(std::string("Not a MessagePack object or array"));
}

/**
 * @brief A document on the wire, in the format the process speaks.
 */
template <>
struct wire_traits<json_document> {
    static std::string write_json(const json_document& v) {
        return v.write();
    }

    static json_document read_json(std::string_view text) {
        auto parsed = json_document::parse(text);
        if (!parsed)
            throw std::invalid_argument(parsed.error());
        return std::move(*parsed);
    }

    static std::string write_msgpack(const json_document& v) {
        return write_document_msgpack(v);
    }

    static json_document read_msgpack(std::span<const std::byte> data) {
        auto parsed = read_document_msgpack(data);
        if (!parsed)
            throw std::invalid_argument(parsed.error());
        return std::move(*parsed);
    }
};

}

#endif
