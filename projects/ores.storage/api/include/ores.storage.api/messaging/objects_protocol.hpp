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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_STORAGE_API_MESSAGING_OBJECTS_PROTOCOL_HPP
#define ORES_STORAGE_API_MESSAGING_OBJECTS_PROTOCOL_HPP

#include "ores.storage.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::storage::messaging {

/**
 * @brief Writes one object, replacing whatever was at the key.
 *
 * A put carries the whole object: there is no append and no partial write, so
 * an object is either the old value or the new one. The content travels in the
 * message only when it is small enough to; the bulk path is HTTP, and the two
 * leave the same object at the same bucket and key.
 */
struct put_objects_request {
    using response_type = struct put_objects_response;
    static constexpr std::string_view nats_subject = "storage.v1.objects.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The allocation the object lives in. Named by the caller.
     */
    std::string bucket;
    /**
     * @brief The object's name within the bucket. Opaque: storage never parses it.
     */
    std::string key;
    /**
     * @brief The object's bytes.
     */
    std::string content;
    /**
     * @brief Empty for raw bytes, or the encoding the caller applied.
     *
     * A caller may compress before it puts, and it must say so here, because
     * nothing on the wire marks an object as compressed.
     */
    std::string content_encoding;

    friend bool operator==(const put_objects_request&, const put_objects_request&) = default;
};

/**
 * @brief The outcome of the write.
 */
struct put_objects_response {
    bool success = true;
    std::string message;
    /**
     * @brief How many bytes the store now holds under the key.
     */
    std::uint64_t size_bytes = 0;

    friend bool operator==(const put_objects_response&, const put_objects_response&) = default;
};

/**
 * @brief Reads one object's metadata, and its value when asked for.
 *
 * The same operation answers existence: a key that is not there comes back as
 * a reply that says so, which is why the surface needs no separate verb for it.
 */
struct get_objects_request {
    using response_type = struct get_objects_response;
    static constexpr std::string_view nats_subject = "storage.v1.objects.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bucket;
    std::string key;
    /**
     * @brief Whether the reply should carry the object's bytes.
     */
    bool include_content = false;

    friend bool operator==(const get_objects_request&, const get_objects_request&) = default;
};

/**
 * @brief The object, or the news that it is not there.
 */
struct get_objects_response {
    bool success = true;
    std::string message;
    /**
     * @brief False when the bucket holds no object at the key.
     */
    bool found = false;
    std::uint64_t size_bytes = 0;
    /**
     * @brief The object's bytes, present only when the request asked for them.
     */
    std::string content;

    friend bool operator==(const get_objects_response&, const get_objects_response&) = default;
};

/**
 * @brief Removes one object.
 */
struct delete_objects_request {
    using response_type = struct delete_objects_response;
    static constexpr std::string_view nats_subject = "storage.v1.objects.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bucket;
    std::string key;

    friend bool operator==(const delete_objects_request&, const delete_objects_request&) = default;
};

/**
 * @brief The outcome of the removal.
 */
struct delete_objects_response {
    bool success = true;
    std::string message;
    /**
     * @brief False when there was nothing at the key to remove.
     */
    bool removed = false;

    friend bool operator==(const delete_objects_response&,
                           const delete_objects_response&) = default;
};

/**
 * @brief Asks for a page of the objects in one bucket.
 *
 * The prefix is a filter over keys, not a directory: storage treats a key as
 * opaque, so a caller that wants everything under a prefix asks for it by
 * prefix and never by path.
 */
struct list_objects_request {
    using response_type = struct list_objects_response;
    static constexpr std::string_view nats_subject = "storage.v1.objects.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bucket;
    /**
     * @brief Only keys that start with this are returned. Empty means every key.
     */
    std::string prefix;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;

    friend bool operator==(const list_objects_request&, const list_objects_request&) = default;
};

/**
 * @brief One object in a listing, without its value.
 */
struct object_summary {
    std::string key;
    std::uint64_t size_bytes = 0;

    friend bool operator==(const object_summary&, const object_summary&) = default;
};

/**
 * @brief The page of objects, with the total the caller is paging through.
 */
struct list_objects_response {
    bool success = true;
    std::string message;
    std::vector<object_summary> objects;
    std::uint64_t total_available_count = 0;

    friend bool operator==(const list_objects_response&, const list_objects_response&) = default;
};

/**
 * @brief One regular file a package holds.
 *
 * A directory or a link is not a file a person opens, so neither is stated.
 */
struct archive_entry {
    /**
     * @brief The file's path inside the package, as the package states it.
     */
    std::string name;
    std::uint64_t size_bytes = 0;

    friend bool operator==(const archive_entry&, const archive_entry&) = default;
};

/**
 * @brief What one object the caller named holds, or why it could not be read.
 *
 * A key that is missing, or that holds bytes no archive reader accepts, is
 * stated here rather than failing the whole reply, so one unreadable package
 * does not cost the caller every other package's listing.
 */
struct archive_listing {
    std::string key;
    /**
     * @brief The size of the object as the store holds it, packed.
     */
    std::uint64_t size_bytes = 0;
    bool success = true;
    std::string message;
    std::vector<archive_entry> entries;

    friend bool operator==(const archive_listing&, const archive_listing&) = default;
};

/**
 * @brief Asks what the files inside a set of packages are.
 *
 * The caller that wants one package's contents wants every package's contents,
 * because they are one screen: a run's inputs and outputs arrive together, and
 * a call per package would put the walk back in the caller. So the keys travel
 * as a set and the reply states each one.
 *
 * The unpacking happens here, in the service that holds the object, so no
 * caller needs an archive reader of its own.
 */
struct list_archive_entries_request {
    using response_type = struct list_archive_entries_response;
    static constexpr std::string_view nats_subject = "storage.v1.ops.list_archive_entries";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bucket;
    /**
     * @brief The objects to read, in one bucket. An empty set asks for nothing.
     */
    std::vector<std::string> keys;

    friend bool operator==(const list_archive_entries_request&,
                           const list_archive_entries_request&) = default;
};

/**
 * @brief What each object the caller named holds, in the order it named them.
 */
struct list_archive_entries_response {
    bool success = true;
    std::string message;
    std::vector<archive_listing> archives;

    friend bool operator==(const list_archive_entries_response&,
                           const list_archive_entries_response&) = default;
};

/**
 * @brief The members of put_objects_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_objects_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"bucket", &put_objects_request::bucket},
        ::ores::utility::codec::field{"key", &put_objects_request::key},
        ::ores::utility::codec::field{"content", &put_objects_request::content},
        ::ores::utility::codec::field{"content_encoding", &put_objects_request::content_encoding},
    };
}

/**
 * @brief The members of put_objects_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_objects_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &put_objects_response::success},
        ::ores::utility::codec::field{"message", &put_objects_response::message},
        ::ores::utility::codec::field{"size_bytes", &put_objects_response::size_bytes},
    };
}

/**
 * @brief The members of get_objects_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_objects_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"bucket", &get_objects_request::bucket},
        ::ores::utility::codec::field{"key", &get_objects_request::key},
        ::ores::utility::codec::field{"include_content", &get_objects_request::include_content},
    };
}

/**
 * @brief The members of get_objects_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_objects_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &get_objects_response::success},
        ::ores::utility::codec::field{"message", &get_objects_response::message},
        ::ores::utility::codec::field{"found", &get_objects_response::found},
        ::ores::utility::codec::field{"size_bytes", &get_objects_response::size_bytes},
        ::ores::utility::codec::field{"content", &get_objects_response::content},
    };
}

/**
 * @brief The members of delete_objects_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_objects_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"bucket", &delete_objects_request::bucket},
        ::ores::utility::codec::field{"key", &delete_objects_request::key},
    };
}

/**
 * @brief The members of delete_objects_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_objects_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &delete_objects_response::success},
        ::ores::utility::codec::field{"message", &delete_objects_response::message},
        ::ores::utility::codec::field{"removed", &delete_objects_response::removed},
    };
}

/**
 * @brief The members of list_objects_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_objects_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"bucket", &list_objects_request::bucket},
        ::ores::utility::codec::field{"prefix", &list_objects_request::prefix},
        ::ores::utility::codec::field{"offset", &list_objects_request::offset},
        ::ores::utility::codec::field{"limit", &list_objects_request::limit},
    };
}

/**
 * @brief The members of object_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<object_summary>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"key", &object_summary::key},
        ::ores::utility::codec::field{"size_bytes", &object_summary::size_bytes},
    };
}

/**
 * @brief The members of list_objects_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_objects_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_objects_response::success},
        ::ores::utility::codec::field{"message", &list_objects_response::message},
        ::ores::utility::codec::field{"objects", &list_objects_response::objects},
        ::ores::utility::codec::field{"total_available_count",
                                      &list_objects_response::total_available_count},
    };
}

/**
 * @brief The members of archive_entry, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<archive_entry>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"name", &archive_entry::name},
        ::ores::utility::codec::field{"size_bytes", &archive_entry::size_bytes},
    };
}

/**
 * @brief The members of archive_listing, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<archive_listing>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"key", &archive_listing::key},
        ::ores::utility::codec::field{"size_bytes", &archive_listing::size_bytes},
        ::ores::utility::codec::field{"success", &archive_listing::success},
        ::ores::utility::codec::field{"message", &archive_listing::message},
        ::ores::utility::codec::field{"entries", &archive_listing::entries},
    };
}

/**
 * @brief The members of list_archive_entries_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_archive_entries_request>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"bucket", &list_archive_entries_request::bucket},
        ::ores::utility::codec::field{"keys", &list_archive_entries_request::keys},
    };
}

/**
 * @brief The members of list_archive_entries_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_archive_entries_response>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"success", &list_archive_entries_response::success},
        ::ores::utility::codec::field{"message", &list_archive_entries_response::message},
        ::ores::utility::codec::field{"archives", &list_archive_entries_response::archives},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::put_objects_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::put_objects_request& v);
    static ores::storage::messaging::put_objects_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::put_objects_request& v);
    static ores::storage::messaging::put_objects_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::put_objects_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::put_objects_response& v);
    static ores::storage::messaging::put_objects_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::put_objects_response& v);
    static ores::storage::messaging::put_objects_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::get_objects_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::get_objects_request& v);
    static ores::storage::messaging::get_objects_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::get_objects_request& v);
    static ores::storage::messaging::get_objects_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::get_objects_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::get_objects_response& v);
    static ores::storage::messaging::get_objects_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::get_objects_response& v);
    static ores::storage::messaging::get_objects_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::delete_objects_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::delete_objects_request& v);
    static ores::storage::messaging::delete_objects_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::delete_objects_request& v);
    static ores::storage::messaging::delete_objects_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::delete_objects_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::delete_objects_response& v);
    static ores::storage::messaging::delete_objects_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::delete_objects_response& v);
    static ores::storage::messaging::delete_objects_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::list_objects_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::list_objects_request& v);
    static ores::storage::messaging::list_objects_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::list_objects_request& v);
    static ores::storage::messaging::list_objects_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::object_summary> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::object_summary& v);
    static ores::storage::messaging::object_summary read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::object_summary& v);
    static ores::storage::messaging::object_summary read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::list_objects_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::list_objects_response& v);
    static ores::storage::messaging::list_objects_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::list_objects_response& v);
    static ores::storage::messaging::list_objects_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::archive_entry> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::archive_entry& v);
    static ores::storage::messaging::archive_entry read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::archive_entry& v);
    static ores::storage::messaging::archive_entry read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::archive_listing> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::archive_listing& v);
    static ores::storage::messaging::archive_listing read_json(std::string_view text);
    static std::string write_msgpack(const ores::storage::messaging::archive_listing& v);
    static ores::storage::messaging::archive_listing read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT wire_traits<ores::storage::messaging::list_archive_entries_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::list_archive_entries_request& v);
    static ores::storage::messaging::list_archive_entries_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::storage::messaging::list_archive_entries_request& v);
    static ores::storage::messaging::list_archive_entries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_STORAGE_API_EXPORT
    wire_traits<ores::storage::messaging::list_archive_entries_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::storage::messaging::list_archive_entries_response& v);
    static ores::storage::messaging::list_archive_entries_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::storage::messaging::list_archive_entries_response& v);
    static ores::storage::messaging::list_archive_entries_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
