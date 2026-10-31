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
#ifndef ORES_REFDATA_API_MESSAGING_PAYMENT_FREQUENCY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_PAYMENT_FREQUENCY_PROTOCOL_HPP

#include "ores.refdata.api/domain/payment_frequency.hpp"
#include "ores.refdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::refdata::messaging {

struct payment_frequency_key {
    std::string code;
};

struct payment_frequency_write {
    std::string code;
    std::string name;
    std::string description;
    std::string period_unit;
    std::optional<int> period_multiplier;
    int display_order;
};

struct payment_frequency_change {
    payment_frequency_write write;
    ores::utility::domain::precondition precondition;
};

struct payment_frequency_removal {
    payment_frequency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct payment_frequency_lookup {
    payment_frequency_key key;
    std::optional<ores::refdata::domain::payment_frequency> payment_frequency;
};

struct payment_frequencies_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct payment_frequency_event {
    boost::uuids::uuid event_id;
    payment_frequency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct payment_frequency_version_key {
    payment_frequency_key payment_frequency;
    std::uint32_t version;
};

struct payment_frequency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_payment_frequencies_request {
    using response_type = struct list_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.list";
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
    std::optional<payment_frequencies_filter> filter;
    std::optional<std::string> as_of;
};

struct list_payment_frequencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::payment_frequency> payment_frequencies;
    std::uint64_t total;
};

struct get_payment_frequency_request {
    using response_type = struct get_payment_frequency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_key key;
};

struct get_payment_frequency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::payment_frequency> payment_frequency;
};

struct get_many_payment_frequencies_request {
    using response_type = struct get_many_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<payment_frequency_key> keys;
};

struct get_many_payment_frequencies_response {
    ores::utility::domain::result result;
    std::vector<payment_frequency_lookup> entries;
};

struct put_payment_frequency_request {
    using response_type = struct put_payment_frequency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_change change;
    ores::utility::domain::change_intent intent;
};

struct put_payment_frequency_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::payment_frequency> payment_frequency;
};

struct put_many_payment_frequencies_request {
    using response_type = struct put_many_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<payment_frequency_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_payment_frequencies_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::payment_frequency> payment_frequencies;
};

struct delete_payment_frequency_request {
    using response_type = struct delete_payment_frequency_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_payment_frequency_response {
    ores::utility::domain::result result;
};

struct delete_many_payment_frequencies_request {
    using response_type = struct delete_many_payment_frequencies_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<payment_frequency_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_payment_frequencies_response {
    ores::utility::domain::result result;
};

struct list_payment_frequency_versions_request {
    using response_type = struct list_payment_frequency_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<payment_frequency_versions_filter> filter;
};

struct list_payment_frequency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::payment_frequency> versions;
    std::uint64_t total;
};

struct get_payment_frequency_version_request {
    using response_type = struct get_payment_frequency_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.payment_frequencies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    payment_frequency_version_key key;
};

struct get_payment_frequency_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::payment_frequency> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace payment_frequency_event_subjects {
inline constexpr std::string_view created = "refdata.v1.payment_frequencies_events.created";
inline constexpr std::string_view updated = "refdata.v1.payment_frequencies_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.payment_frequencies_events.deleted";
}
/**
 * @brief The members of payment_frequency_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_key&) {
    return std::tuple{
        utility::codec::field{"code", &payment_frequency_key::code},
    };
}

/**
 * @brief The members of payment_frequency_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_write&) {
    return std::tuple{
        utility::codec::field{"code", &payment_frequency_write::code},
        utility::codec::field{"name", &payment_frequency_write::name},
        utility::codec::field{"description", &payment_frequency_write::description},
        utility::codec::field{"period_unit", &payment_frequency_write::period_unit},
        utility::codec::field{"period_multiplier", &payment_frequency_write::period_multiplier},
        utility::codec::field{"display_order", &payment_frequency_write::display_order},
    };
}

/**
 * @brief The members of payment_frequency_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_change&) {
    return std::tuple{
        utility::codec::field{"write", &payment_frequency_change::write},
        utility::codec::field{"precondition", &payment_frequency_change::precondition},
    };
}

/**
 * @brief The members of payment_frequency_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_removal&) {
    return std::tuple{
        utility::codec::field{"key", &payment_frequency_removal::key},
        utility::codec::field{"precondition", &payment_frequency_removal::precondition},
    };
}

/**
 * @brief The members of payment_frequency_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &payment_frequency_lookup::key},
        utility::codec::field{"payment_frequency", &payment_frequency_lookup::payment_frequency},
    };
}

/**
 * @brief The members of payment_frequencies_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequencies_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &payment_frequencies_filter::code_one_of},
    };
}

/**
 * @brief The members of payment_frequency_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &payment_frequency_event::event_id},
        utility::codec::field{"key", &payment_frequency_event::key},
        utility::codec::field{"action", &payment_frequency_event::action},
        utility::codec::field{"version", &payment_frequency_event::version},
        utility::codec::field{"occurred_at", &payment_frequency_event::occurred_at},
        utility::codec::field{"correlation_id", &payment_frequency_event::correlation_id},
    };
}

/**
 * @brief The members of payment_frequency_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_version_key&) {
    return std::tuple{
        utility::codec::field{"payment_frequency",
                              &payment_frequency_version_key::payment_frequency},
        utility::codec::field{"version", &payment_frequency_version_key::version},
    };
}

/**
 * @brief The members of payment_frequency_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const payment_frequency_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &payment_frequency_versions_filter::version},
        utility::codec::field{"from_version", &payment_frequency_versions_filter::from_version},
        utility::codec::field{"to_version", &payment_frequency_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_payment_frequencies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_payment_frequencies_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_payment_frequencies_request::offset},
        utility::codec::field{"limit", &list_payment_frequencies_request::limit},
        utility::codec::field{"order", &list_payment_frequencies_request::order},
        utility::codec::field{"filter", &list_payment_frequencies_request::filter},
        utility::codec::field{"as_of", &list_payment_frequencies_request::as_of},
    };
}

/**
 * @brief The members of list_payment_frequencies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_payment_frequencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_payment_frequencies_response::result},
        utility::codec::field{"payment_frequencies",
                              &list_payment_frequencies_response::payment_frequencies},
        utility::codec::field{"total", &list_payment_frequencies_response::total},
    };
}

/**
 * @brief The members of get_payment_frequency_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_payment_frequency_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_payment_frequency_request::key},
    };
}

/**
 * @brief The members of get_payment_frequency_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_payment_frequency_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_payment_frequency_response::result},
        utility::codec::field{"payment_frequency",
                              &get_payment_frequency_response::payment_frequency},
    };
}

/**
 * @brief The members of get_many_payment_frequencies_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_payment_frequencies_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_payment_frequencies_request::keys},
    };
}

/**
 * @brief The members of get_many_payment_frequencies_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_payment_frequencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_payment_frequencies_response::result},
        utility::codec::field{"entries", &get_many_payment_frequencies_response::entries},
    };
}

/**
 * @brief The members of put_payment_frequency_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_payment_frequency_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_payment_frequency_request::change},
        utility::codec::field{"intent", &put_payment_frequency_request::intent},
    };
}

/**
 * @brief The members of put_payment_frequency_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_payment_frequency_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_payment_frequency_response::result},
        utility::codec::field{"payment_frequency",
                              &put_payment_frequency_response::payment_frequency},
    };
}

/**
 * @brief The members of put_many_payment_frequencies_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_payment_frequencies_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_payment_frequencies_request::changes},
        utility::codec::field{"intent", &put_many_payment_frequencies_request::intent},
    };
}

/**
 * @brief The members of put_many_payment_frequencies_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_payment_frequencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_payment_frequencies_response::result},
        utility::codec::field{"payment_frequencies",
                              &put_many_payment_frequencies_response::payment_frequencies},
    };
}

/**
 * @brief The members of delete_payment_frequency_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_payment_frequency_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_payment_frequency_request::removal},
        utility::codec::field{"intent", &delete_payment_frequency_request::intent},
    };
}

/**
 * @brief The members of delete_payment_frequency_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_payment_frequency_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_payment_frequency_response::result},
    };
}

/**
 * @brief The members of delete_many_payment_frequencies_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_payment_frequencies_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_payment_frequencies_request::removals},
        utility::codec::field{"intent", &delete_many_payment_frequencies_request::intent},
    };
}

/**
 * @brief The members of delete_many_payment_frequencies_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_payment_frequencies_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_payment_frequencies_response::result},
    };
}

/**
 * @brief The members of list_payment_frequency_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_payment_frequency_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_payment_frequency_versions_request::key},
        utility::codec::field{"offset", &list_payment_frequency_versions_request::offset},
        utility::codec::field{"limit", &list_payment_frequency_versions_request::limit},
        utility::codec::field{"order", &list_payment_frequency_versions_request::order},
        utility::codec::field{"filter", &list_payment_frequency_versions_request::filter},
    };
}

/**
 * @brief The members of list_payment_frequency_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_payment_frequency_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_payment_frequency_versions_response::result},
        utility::codec::field{"versions", &list_payment_frequency_versions_response::versions},
        utility::codec::field{"total", &list_payment_frequency_versions_response::total},
    };
}

/**
 * @brief The members of get_payment_frequency_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_payment_frequency_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_payment_frequency_version_request::key},
    };
}

/**
 * @brief The members of get_payment_frequency_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_payment_frequency_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_payment_frequency_version_response::result},
        utility::codec::field{"version", &get_payment_frequency_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::payment_frequency_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequency_key& v);
    static ores::refdata::messaging::payment_frequency_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::payment_frequency_key& v);
    static ores::refdata::messaging::payment_frequency_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::payment_frequency_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequency_write& v);
    static ores::refdata::messaging::payment_frequency_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::payment_frequency_write& v);
    static ores::refdata::messaging::payment_frequency_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::payment_frequency_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequency_change& v);
    static ores::refdata::messaging::payment_frequency_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::payment_frequency_change& v);
    static ores::refdata::messaging::payment_frequency_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::payment_frequency_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequency_removal& v);
    static ores::refdata::messaging::payment_frequency_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::payment_frequency_removal& v);
    static ores::refdata::messaging::payment_frequency_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::payment_frequency_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequency_lookup& v);
    static ores::refdata::messaging::payment_frequency_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::payment_frequency_lookup& v);
    static ores::refdata::messaging::payment_frequency_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::payment_frequencies_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequencies_filter& v);
    static ores::refdata::messaging::payment_frequencies_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::payment_frequencies_filter& v);
    static ores::refdata::messaging::payment_frequencies_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::payment_frequency_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequency_event& v);
    static ores::refdata::messaging::payment_frequency_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::payment_frequency_event& v);
    static ores::refdata::messaging::payment_frequency_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::payment_frequency_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::payment_frequency_version_key& v);
    static ores::refdata::messaging::payment_frequency_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::payment_frequency_version_key& v);
    static ores::refdata::messaging::payment_frequency_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::payment_frequency_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::payment_frequency_versions_filter& v);
    static ores::refdata::messaging::payment_frequency_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::payment_frequency_versions_filter& v);
    static ores::refdata::messaging::payment_frequency_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_payment_frequencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_payment_frequencies_request& v);
    static ores::refdata::messaging::list_payment_frequencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_payment_frequencies_request& v);
    static ores::refdata::messaging::list_payment_frequencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_payment_frequencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_payment_frequencies_response& v);
    static ores::refdata::messaging::list_payment_frequencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_payment_frequencies_response& v);
    static ores::refdata::messaging::list_payment_frequencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_payment_frequency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_payment_frequency_request& v);
    static ores::refdata::messaging::get_payment_frequency_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_payment_frequency_request& v);
    static ores::refdata::messaging::get_payment_frequency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_payment_frequency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_payment_frequency_response& v);
    static ores::refdata::messaging::get_payment_frequency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_payment_frequency_response& v);
    static ores::refdata::messaging::get_payment_frequency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_payment_frequencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_payment_frequencies_request& v);
    static ores::refdata::messaging::get_many_payment_frequencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_payment_frequencies_request& v);
    static ores::refdata::messaging::get_many_payment_frequencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_payment_frequencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_payment_frequencies_response& v);
    static ores::refdata::messaging::get_many_payment_frequencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_payment_frequencies_response& v);
    static ores::refdata::messaging::get_many_payment_frequencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_payment_frequency_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_payment_frequency_request& v);
    static ores::refdata::messaging::put_payment_frequency_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_payment_frequency_request& v);
    static ores::refdata::messaging::put_payment_frequency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_payment_frequency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_payment_frequency_response& v);
    static ores::refdata::messaging::put_payment_frequency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_payment_frequency_response& v);
    static ores::refdata::messaging::put_payment_frequency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_payment_frequencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_payment_frequencies_request& v);
    static ores::refdata::messaging::put_many_payment_frequencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_payment_frequencies_request& v);
    static ores::refdata::messaging::put_many_payment_frequencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_payment_frequencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_payment_frequencies_response& v);
    static ores::refdata::messaging::put_many_payment_frequencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_payment_frequencies_response& v);
    static ores::refdata::messaging::put_many_payment_frequencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_payment_frequency_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_payment_frequency_request& v);
    static ores::refdata::messaging::delete_payment_frequency_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_payment_frequency_request& v);
    static ores::refdata::messaging::delete_payment_frequency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_payment_frequency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_payment_frequency_response& v);
    static ores::refdata::messaging::delete_payment_frequency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_payment_frequency_response& v);
    static ores::refdata::messaging::delete_payment_frequency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_payment_frequencies_request& v);
    static ores::refdata::messaging::delete_many_payment_frequencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_payment_frequencies_request& v);
    static ores::refdata::messaging::delete_many_payment_frequencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_payment_frequencies_response& v);
    static ores::refdata::messaging::delete_many_payment_frequencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_payment_frequencies_response& v);
    static ores::refdata::messaging::delete_many_payment_frequencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_payment_frequency_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_payment_frequency_versions_request& v);
    static ores::refdata::messaging::list_payment_frequency_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_payment_frequency_versions_request& v);
    static ores::refdata::messaging::list_payment_frequency_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_payment_frequency_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_payment_frequency_versions_response& v);
    static ores::refdata::messaging::list_payment_frequency_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_payment_frequency_versions_response& v);
    static ores::refdata::messaging::list_payment_frequency_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_payment_frequency_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_payment_frequency_version_request& v);
    static ores::refdata::messaging::get_payment_frequency_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_payment_frequency_version_request& v);
    static ores::refdata::messaging::get_payment_frequency_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_payment_frequency_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_payment_frequency_version_response& v);
    static ores::refdata::messaging::get_payment_frequency_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_payment_frequency_version_response& v);
    static ores::refdata::messaging::get_payment_frequency_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
