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
#ifndef ORES_DQ_API_MESSAGING_PUBLICATION_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_PUBLICATION_PROTOCOL_HPP

#include "ores.dq.api/domain/publication_codec.hpp"
#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::dq::messaging {

struct publication_key {
    boost::uuids::uuid id;

    friend bool operator==(const publication_key&, const publication_key&) = default;
};

struct publication_write {
    boost::uuids::uuid id;
    boost::uuids::uuid dataset_id;
    std::string dataset_code;
    std::string mode;
    std::string target_table;
    std::int64_t records_inserted{};
    std::int64_t records_updated{};
    std::int64_t records_skipped{};
    std::int64_t records_deleted{};
    std::string published_by;
    std::chrono::system_clock::time_point published_at;

    friend bool operator==(const publication_write&, const publication_write&) = default;
};

struct publication_change {
    publication_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const publication_change&, const publication_change&) = default;
};

struct publication_removal {
    publication_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const publication_removal&, const publication_removal&) = default;
};

struct publication_lookup {
    publication_key key;
    std::optional<ores::dq::domain::publication> publication;

    friend bool operator==(const publication_lookup&, const publication_lookup&) = default;
};

struct publications_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const publications_filter&, const publications_filter&) = default;
};

struct publication_event {
    boost::uuids::uuid event_id;
    publication_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const publication_event&, const publication_event&) = default;
};

struct list_publications_request {
    using response_type = struct list_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<publications_filter> filter;

    friend bool operator==(const list_publications_request&,
                           const list_publications_request&) = default;
};

struct list_publications_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::publication> publications;
    std::uint64_t total{};

    friend bool operator==(const list_publications_response&,
                           const list_publications_response&) = default;
};

struct get_publication_request {
    using response_type = struct get_publication_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    publication_key key;

    friend bool operator==(const get_publication_request&,
                           const get_publication_request&) = default;
};

struct get_publication_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::publication> publication;

    friend bool operator==(const get_publication_response&,
                           const get_publication_response&) = default;
};

struct get_many_publications_request {
    using response_type = struct get_many_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<publication_key> keys;

    friend bool operator==(const get_many_publications_request&,
                           const get_many_publications_request&) = default;
};

struct get_many_publications_response {
    ores::utility::domain::result result;
    std::vector<publication_lookup> entries;

    friend bool operator==(const get_many_publications_response&,
                           const get_many_publications_response&) = default;
};

struct put_publication_request {
    using response_type = struct put_publication_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    publication_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_publication_request&,
                           const put_publication_request&) = default;
};

struct put_publication_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::publication> publication;

    friend bool operator==(const put_publication_response&,
                           const put_publication_response&) = default;
};

struct put_many_publications_request {
    using response_type = struct put_many_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<publication_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_publications_request&,
                           const put_many_publications_request&) = default;
};

struct put_many_publications_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::publication> publications;

    friend bool operator==(const put_many_publications_response&,
                           const put_many_publications_response&) = default;
};

struct delete_publication_request {
    using response_type = struct delete_publication_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    publication_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_publication_request&,
                           const delete_publication_request&) = default;
};

struct delete_publication_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_publication_response&,
                           const delete_publication_response&) = default;
};

struct delete_many_publications_request {
    using response_type = struct delete_many_publications_response;
    static constexpr std::string_view nats_subject = "dq.v1.publications.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<publication_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_publications_request&,
                           const delete_many_publications_request&) = default;
};

struct delete_many_publications_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_publications_response&,
                           const delete_many_publications_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace publication_event_subjects {
inline constexpr std::string_view created = "dq.v1.events.publications.created";
inline constexpr std::string_view updated = "dq.v1.events.publications.updated";
inline constexpr std::string_view deleted = "dq.v1.events.publications.deleted";
}
/**
 * @brief The members of publication_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publication_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &publication_key::id},
    };
}

/**
 * @brief The members of publication_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publication_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &publication_write::id},
        utility::codec::field{"dataset_id", &publication_write::dataset_id},
        utility::codec::field{"dataset_code", &publication_write::dataset_code},
        utility::codec::field{"mode", &publication_write::mode},
        utility::codec::field{"target_table", &publication_write::target_table},
        utility::codec::field{"records_inserted", &publication_write::records_inserted},
        utility::codec::field{"records_updated", &publication_write::records_updated},
        utility::codec::field{"records_skipped", &publication_write::records_skipped},
        utility::codec::field{"records_deleted", &publication_write::records_deleted},
        utility::codec::field{"published_by", &publication_write::published_by},
        utility::codec::field{"published_at", &publication_write::published_at},
    };
}

/**
 * @brief The members of publication_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publication_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &publication_change::write},
        utility::codec::field{"precondition", &publication_change::precondition},
    };
}

/**
 * @brief The members of publication_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publication_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &publication_removal::key},
        utility::codec::field{"precondition", &publication_removal::precondition},
    };
}

/**
 * @brief The members of publication_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publication_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &publication_lookup::key},
        utility::codec::field{"publication", &publication_lookup::publication},
    };
}

/**
 * @brief The members of publications_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publications_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &publications_filter::id_one_of},
    };
}

/**
 * @brief The members of publication_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<publication_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &publication_event::event_id},
        utility::codec::field{"key", &publication_event::key},
        utility::codec::field{"action", &publication_event::action},
        utility::codec::field{"version", &publication_event::version},
        utility::codec::field{"occurred_at", &publication_event::occurred_at},
        utility::codec::field{"correlation_id", &publication_event::correlation_id},
    };
}

/**
 * @brief The members of list_publications_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_publications_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_publications_request::offset},
        utility::codec::field{"limit", &list_publications_request::limit},
        utility::codec::field{"order", &list_publications_request::order},
        utility::codec::field{"filter", &list_publications_request::filter},
    };
}

/**
 * @brief The members of list_publications_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_publications_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_publications_response::result},
        utility::codec::field{"publications", &list_publications_response::publications},
        utility::codec::field{"total", &list_publications_response::total},
    };
}

/**
 * @brief The members of get_publication_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_publication_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_publication_request::key},
    };
}

/**
 * @brief The members of get_publication_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_publication_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_publication_response::result},
        utility::codec::field{"publication", &get_publication_response::publication},
    };
}

/**
 * @brief The members of get_many_publications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_publications_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_publications_request::keys},
    };
}

/**
 * @brief The members of get_many_publications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_publications_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_publications_response::result},
        utility::codec::field{"entries", &get_many_publications_response::entries},
    };
}

/**
 * @brief The members of put_publication_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_publication_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_publication_request::change},
        utility::codec::field{"intent", &put_publication_request::intent},
    };
}

/**
 * @brief The members of put_publication_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_publication_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_publication_response::result},
        utility::codec::field{"publication", &put_publication_response::publication},
    };
}

/**
 * @brief The members of put_many_publications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_publications_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_publications_request::changes},
        utility::codec::field{"intent", &put_many_publications_request::intent},
    };
}

/**
 * @brief The members of put_many_publications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_publications_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_publications_response::result},
        utility::codec::field{"publications", &put_many_publications_response::publications},
    };
}

/**
 * @brief The members of delete_publication_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_publication_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_publication_request::removal},
        utility::codec::field{"intent", &delete_publication_request::intent},
    };
}

/**
 * @brief The members of delete_publication_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_publication_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_publication_response::result},
    };
}

/**
 * @brief The members of delete_many_publications_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_publications_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_publications_request::removals},
        utility::codec::field{"intent", &delete_many_publications_request::intent},
    };
}

/**
 * @brief The members of delete_many_publications_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_publications_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_publications_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publication_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publication_key& v);
    static ores::dq::messaging::publication_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publication_key& v);
    static ores::dq::messaging::publication_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publication_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publication_write& v);
    static ores::dq::messaging::publication_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publication_write& v);
    static ores::dq::messaging::publication_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publication_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publication_change& v);
    static ores::dq::messaging::publication_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publication_change& v);
    static ores::dq::messaging::publication_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publication_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publication_removal& v);
    static ores::dq::messaging::publication_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publication_removal& v);
    static ores::dq::messaging::publication_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publication_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publication_lookup& v);
    static ores::dq::messaging::publication_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publication_lookup& v);
    static ores::dq::messaging::publication_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publications_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publications_filter& v);
    static ores::dq::messaging::publications_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publications_filter& v);
    static ores::dq::messaging::publications_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publication_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publication_event& v);
    static ores::dq::messaging::publication_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publication_event& v);
    static ores::dq::messaging::publication_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_publications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_publications_request& v);
    static ores::dq::messaging::list_publications_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_publications_request& v);
    static ores::dq::messaging::list_publications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_publications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_publications_response& v);
    static ores::dq::messaging::list_publications_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_publications_response& v);
    static ores::dq::messaging::list_publications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_publication_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_publication_request& v);
    static ores::dq::messaging::get_publication_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_publication_request& v);
    static ores::dq::messaging::get_publication_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_publication_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_publication_response& v);
    static ores::dq::messaging::get_publication_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_publication_response& v);
    static ores::dq::messaging::get_publication_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_publications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_publications_request& v);
    static ores::dq::messaging::get_many_publications_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_publications_request& v);
    static ores::dq::messaging::get_many_publications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_publications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_publications_response& v);
    static ores::dq::messaging::get_many_publications_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_many_publications_response& v);
    static ores::dq::messaging::get_many_publications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_publication_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_publication_request& v);
    static ores::dq::messaging::put_publication_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_publication_request& v);
    static ores::dq::messaging::put_publication_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_publication_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_publication_response& v);
    static ores::dq::messaging::put_publication_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_publication_response& v);
    static ores::dq::messaging::put_publication_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_publications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_publications_request& v);
    static ores::dq::messaging::put_many_publications_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_publications_request& v);
    static ores::dq::messaging::put_many_publications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::put_many_publications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::put_many_publications_response& v);
    static ores::dq::messaging::put_many_publications_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::put_many_publications_response& v);
    static ores::dq::messaging::put_many_publications_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_publication_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_publication_request& v);
    static ores::dq::messaging::delete_publication_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_publication_request& v);
    static ores::dq::messaging::delete_publication_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_publication_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_publication_response& v);
    static ores::dq::messaging::delete_publication_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::delete_publication_response& v);
    static ores::dq::messaging::delete_publication_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_publications_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_publications_request& v);
    static ores::dq::messaging::delete_many_publications_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_publications_request& v);
    static ores::dq::messaging::delete_many_publications_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::delete_many_publications_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::delete_many_publications_response& v);
    static ores::dq::messaging::delete_many_publications_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::delete_many_publications_response& v);
    static ores::dq::messaging::delete_many_publications_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
