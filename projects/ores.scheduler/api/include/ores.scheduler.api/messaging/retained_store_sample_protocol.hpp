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
#ifndef ORES_SCHEDULER_API_MESSAGING_RETAINED_STORE_SAMPLE_PROTOCOL_HPP
#define ORES_SCHEDULER_API_MESSAGING_RETAINED_STORE_SAMPLE_PROTOCOL_HPP

#include "ores.scheduler.api/domain/retained_store_sample_codec.hpp"
#include "ores.scheduler.api/export.hpp"
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

namespace ores::scheduler::messaging {

struct retained_store_sample_key {
    boost::uuids::uuid id;
    std::chrono::system_clock::time_point sampled_at;

    friend bool operator==(const retained_store_sample_key&,
                           const retained_store_sample_key&) = default;
};

struct retained_store_sample_write {
    boost::uuids::uuid id;
    std::chrono::system_clock::time_point sampled_at;
    std::string store_code;
    std::int64_t size_bytes{};
    std::int64_t measure_count{};

    friend bool operator==(const retained_store_sample_write&,
                           const retained_store_sample_write&) = default;
};

struct retained_store_sample_change {
    retained_store_sample_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const retained_store_sample_change&,
                           const retained_store_sample_change&) = default;
};

struct retained_store_sample_removal {
    retained_store_sample_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const retained_store_sample_removal&,
                           const retained_store_sample_removal&) = default;
};

struct retained_store_sample_lookup {
    retained_store_sample_key key;
    std::optional<ores::scheduler::domain::retained_store_sample> retained_store_sample;

    friend bool operator==(const retained_store_sample_lookup&,
                           const retained_store_sample_lookup&) = default;
};

struct retained_store_sample_event {
    boost::uuids::uuid event_id;
    retained_store_sample_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const retained_store_sample_event&,
                           const retained_store_sample_event&) = default;
};

struct list_retained_store_samples_request {
    using response_type = struct list_retained_store_samples_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_store_samples.list";
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

    friend bool operator==(const list_retained_store_samples_request&,
                           const list_retained_store_samples_request&) = default;
};

struct list_retained_store_samples_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::retained_store_sample> samples;
    std::uint64_t total{};

    friend bool operator==(const list_retained_store_samples_response&,
                           const list_retained_store_samples_response&) = default;
};

struct get_retained_store_sample_request {
    using response_type = struct get_retained_store_sample_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_store_samples.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_sample_key key;

    friend bool operator==(const get_retained_store_sample_request&,
                           const get_retained_store_sample_request&) = default;
};

struct get_retained_store_sample_response {
    ores::utility::domain::result result;
    std::optional<ores::scheduler::domain::retained_store_sample> retained_store_sample;

    friend bool operator==(const get_retained_store_sample_response&,
                           const get_retained_store_sample_response&) = default;
};

struct get_many_retained_store_samples_request {
    using response_type = struct get_many_retained_store_samples_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_store_samples.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<retained_store_sample_key> keys;

    friend bool operator==(const get_many_retained_store_samples_request&,
                           const get_many_retained_store_samples_request&) = default;
};

struct get_many_retained_store_samples_response {
    ores::utility::domain::result result;
    std::vector<retained_store_sample_lookup> entries;

    friend bool operator==(const get_many_retained_store_samples_response&,
                           const get_many_retained_store_samples_response&) = default;
};

struct put_retained_store_sample_request {
    using response_type = struct put_retained_store_sample_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_store_samples.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_sample_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_retained_store_sample_request&,
                           const put_retained_store_sample_request&) = default;
};

struct put_retained_store_sample_response {
    ores::utility::domain::result result;
    std::optional<ores::scheduler::domain::retained_store_sample> retained_store_sample;

    friend bool operator==(const put_retained_store_sample_response&,
                           const put_retained_store_sample_response&) = default;
};

struct put_many_retained_store_samples_request {
    using response_type = struct put_many_retained_store_samples_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_store_samples.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<retained_store_sample_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_retained_store_samples_request&,
                           const put_many_retained_store_samples_request&) = default;
};

struct put_many_retained_store_samples_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::retained_store_sample> samples;

    friend bool operator==(const put_many_retained_store_samples_response&,
                           const put_many_retained_store_samples_response&) = default;
};

struct delete_retained_store_sample_request {
    using response_type = struct delete_retained_store_sample_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.retained_store_samples.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    retained_store_sample_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_retained_store_sample_request&,
                           const delete_retained_store_sample_request&) = default;
};

struct delete_retained_store_sample_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_retained_store_sample_response&,
                           const delete_retained_store_sample_response&) = default;
};

struct delete_many_retained_store_samples_request {
    using response_type = struct delete_many_retained_store_samples_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.retained_store_samples.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<retained_store_sample_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_retained_store_samples_request&,
                           const delete_many_retained_store_samples_request&) = default;
};

struct delete_many_retained_store_samples_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_retained_store_samples_response&,
                           const delete_many_retained_store_samples_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace retained_store_sample_event_subjects {
inline constexpr std::string_view created = "scheduler.v1.events.retained_store_samples.created";
inline constexpr std::string_view updated = "scheduler.v1.events.retained_store_samples.updated";
inline constexpr std::string_view deleted = "scheduler.v1.events.retained_store_samples.deleted";
}
/**
 * @brief The members of retained_store_sample_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_sample_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &retained_store_sample_key::id},
        utility::codec::field{"sampled_at", &retained_store_sample_key::sampled_at},
    };
}

/**
 * @brief The members of retained_store_sample_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_sample_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &retained_store_sample_write::id},
        utility::codec::field{"sampled_at", &retained_store_sample_write::sampled_at},
        utility::codec::field{"store_code", &retained_store_sample_write::store_code},
        utility::codec::field{"size_bytes", &retained_store_sample_write::size_bytes},
        utility::codec::field{"measure_count", &retained_store_sample_write::measure_count},
    };
}

/**
 * @brief The members of retained_store_sample_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_sample_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &retained_store_sample_change::write},
        utility::codec::field{"precondition", &retained_store_sample_change::precondition},
    };
}

/**
 * @brief The members of retained_store_sample_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_sample_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &retained_store_sample_removal::key},
        utility::codec::field{"precondition", &retained_store_sample_removal::precondition},
    };
}

/**
 * @brief The members of retained_store_sample_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_sample_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &retained_store_sample_lookup::key},
        utility::codec::field{"retained_store_sample",
                              &retained_store_sample_lookup::retained_store_sample},
    };
}

/**
 * @brief The members of retained_store_sample_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<retained_store_sample_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &retained_store_sample_event::event_id},
        utility::codec::field{"key", &retained_store_sample_event::key},
        utility::codec::field{"action", &retained_store_sample_event::action},
        utility::codec::field{"version", &retained_store_sample_event::version},
        utility::codec::field{"occurred_at", &retained_store_sample_event::occurred_at},
        utility::codec::field{"correlation_id", &retained_store_sample_event::correlation_id},
    };
}

/**
 * @brief The members of list_retained_store_samples_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_retained_store_samples_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_retained_store_samples_request::offset},
        utility::codec::field{"limit", &list_retained_store_samples_request::limit},
        utility::codec::field{"order", &list_retained_store_samples_request::order},
    };
}

/**
 * @brief The members of list_retained_store_samples_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_retained_store_samples_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_retained_store_samples_response::result},
        utility::codec::field{"samples", &list_retained_store_samples_response::samples},
        utility::codec::field{"total", &list_retained_store_samples_response::total},
    };
}

/**
 * @brief The members of get_retained_store_sample_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retained_store_sample_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_retained_store_sample_request::key},
    };
}

/**
 * @brief The members of get_retained_store_sample_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_retained_store_sample_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_retained_store_sample_response::result},
        utility::codec::field{"retained_store_sample",
                              &get_retained_store_sample_response::retained_store_sample},
    };
}

/**
 * @brief The members of get_many_retained_store_samples_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_retained_store_samples_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_retained_store_samples_request::keys},
    };
}

/**
 * @brief The members of get_many_retained_store_samples_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_retained_store_samples_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_retained_store_samples_response::result},
        utility::codec::field{"entries", &get_many_retained_store_samples_response::entries},
    };
}

/**
 * @brief The members of put_retained_store_sample_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_retained_store_sample_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_retained_store_sample_request::change},
        utility::codec::field{"intent", &put_retained_store_sample_request::intent},
    };
}

/**
 * @brief The members of put_retained_store_sample_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_retained_store_sample_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_retained_store_sample_response::result},
        utility::codec::field{"retained_store_sample",
                              &put_retained_store_sample_response::retained_store_sample},
    };
}

/**
 * @brief The members of put_many_retained_store_samples_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_retained_store_samples_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_retained_store_samples_request::changes},
        utility::codec::field{"intent", &put_many_retained_store_samples_request::intent},
    };
}

/**
 * @brief The members of put_many_retained_store_samples_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_retained_store_samples_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_retained_store_samples_response::result},
        utility::codec::field{"samples", &put_many_retained_store_samples_response::samples},
    };
}

/**
 * @brief The members of delete_retained_store_sample_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_retained_store_sample_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_retained_store_sample_request::removal},
        utility::codec::field{"intent", &delete_retained_store_sample_request::intent},
    };
}

/**
 * @brief The members of delete_retained_store_sample_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_retained_store_sample_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_retained_store_sample_response::result},
    };
}

/**
 * @brief The members of delete_many_retained_store_samples_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_retained_store_samples_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_retained_store_samples_request::removals},
        utility::codec::field{"intent", &delete_many_retained_store_samples_request::intent},
    };
}

/**
 * @brief The members of delete_many_retained_store_samples_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_retained_store_samples_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_retained_store_samples_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_sample_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_sample_key& v);
    static ores::scheduler::messaging::retained_store_sample_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_sample_key& v);
    static ores::scheduler::messaging::retained_store_sample_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_sample_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_sample_write& v);
    static ores::scheduler::messaging::retained_store_sample_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_sample_write& v);
    static ores::scheduler::messaging::retained_store_sample_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_sample_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::retained_store_sample_change& v);
    static ores::scheduler::messaging::retained_store_sample_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_sample_change& v);
    static ores::scheduler::messaging::retained_store_sample_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_sample_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::retained_store_sample_removal& v);
    static ores::scheduler::messaging::retained_store_sample_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_sample_removal& v);
    static ores::scheduler::messaging::retained_store_sample_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_sample_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::retained_store_sample_lookup& v);
    static ores::scheduler::messaging::retained_store_sample_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_sample_lookup& v);
    static ores::scheduler::messaging::retained_store_sample_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::retained_store_sample_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::retained_store_sample_event& v);
    static ores::scheduler::messaging::retained_store_sample_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::retained_store_sample_event& v);
    static ores::scheduler::messaging::retained_store_sample_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retained_store_samples_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::list_retained_store_samples_request& v);
    static ores::scheduler::messaging::list_retained_store_samples_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retained_store_samples_request& v);
    static ores::scheduler::messaging::list_retained_store_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::list_retained_store_samples_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::list_retained_store_samples_response& v);
    static ores::scheduler::messaging::list_retained_store_samples_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::list_retained_store_samples_response& v);
    static ores::scheduler::messaging::list_retained_store_samples_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retained_store_sample_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_retained_store_sample_request& v);
    static ores::scheduler::messaging::get_retained_store_sample_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retained_store_sample_request& v);
    static ores::scheduler::messaging::get_retained_store_sample_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_retained_store_sample_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_retained_store_sample_response& v);
    static ores::scheduler::messaging::get_retained_store_sample_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_retained_store_sample_response& v);
    static ores::scheduler::messaging::get_retained_store_sample_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_many_retained_store_samples_request& v);
    static ores::scheduler::messaging::get_many_retained_store_samples_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_many_retained_store_samples_request& v);
    static ores::scheduler::messaging::get_many_retained_store_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_many_retained_store_samples_response& v);
    static ores::scheduler::messaging::get_many_retained_store_samples_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_many_retained_store_samples_response& v);
    static ores::scheduler::messaging::get_many_retained_store_samples_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_retained_store_sample_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::put_retained_store_sample_request& v);
    static ores::scheduler::messaging::put_retained_store_sample_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_retained_store_sample_request& v);
    static ores::scheduler::messaging::put_retained_store_sample_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_retained_store_sample_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::put_retained_store_sample_response& v);
    static ores::scheduler::messaging::put_retained_store_sample_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_retained_store_sample_response& v);
    static ores::scheduler::messaging::put_retained_store_sample_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::put_many_retained_store_samples_request& v);
    static ores::scheduler::messaging::put_many_retained_store_samples_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_many_retained_store_samples_request& v);
    static ores::scheduler::messaging::put_many_retained_store_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::put_many_retained_store_samples_response& v);
    static ores::scheduler::messaging::put_many_retained_store_samples_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::put_many_retained_store_samples_response& v);
    static ores::scheduler::messaging::put_many_retained_store_samples_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_retained_store_sample_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_retained_store_sample_request& v);
    static ores::scheduler::messaging::delete_retained_store_sample_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_retained_store_sample_request& v);
    static ores::scheduler::messaging::delete_retained_store_sample_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_retained_store_sample_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_retained_store_sample_response& v);
    static ores::scheduler::messaging::delete_retained_store_sample_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_retained_store_sample_response& v);
    static ores::scheduler::messaging::delete_retained_store_sample_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_many_retained_store_samples_request& v);
    static ores::scheduler::messaging::delete_many_retained_store_samples_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_many_retained_store_samples_request& v);
    static ores::scheduler::messaging::delete_many_retained_store_samples_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::delete_many_retained_store_samples_response& v);
    static ores::scheduler::messaging::delete_many_retained_store_samples_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::delete_many_retained_store_samples_response& v);
    static ores::scheduler::messaging::delete_many_retained_store_samples_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
