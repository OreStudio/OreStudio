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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_INPUT_VALUE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_INPUT_VALUE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_input_value_codec.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct market_input_value_key {
    boost::uuids::uuid id;

    friend bool operator==(const market_input_value_key&, const market_input_value_key&) = default;
};

struct market_input_value_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    boost::uuids::uuid result_id;
    std::string source;
    int line{};
    std::chrono::system_clock::time_point as_of;
    boost::uuids::uuid report_definition_id;
    std::string family;
    std::string datum_id;
    double value{};
    std::optional<std::chrono::system_clock::time_point> payment_date;

    friend bool operator==(const market_input_value_write&,
                           const market_input_value_write&) = default;
};

struct market_input_value_change {
    market_input_value_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const market_input_value_change&,
                           const market_input_value_change&) = default;
};

struct market_input_value_removal {
    market_input_value_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const market_input_value_removal&,
                           const market_input_value_removal&) = default;
};

struct market_input_value_lookup {
    market_input_value_key key;
    std::optional<ores::marketdata::domain::market_input_value> market_input_value;

    friend bool operator==(const market_input_value_lookup&,
                           const market_input_value_lookup&) = default;
};

struct market_input_values_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> family;
    std::optional<std::string> datum_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> family_one_of;
    std::optional<std::vector<std::string>> datum_id_one_of;

    friend bool operator==(const market_input_values_filter&,
                           const market_input_values_filter&) = default;
};

struct market_input_value_event {
    boost::uuids::uuid event_id;
    market_input_value_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const market_input_value_event&,
                           const market_input_value_event&) = default;
};

struct list_market_input_values_request {
    using response_type = struct list_market_input_values_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_input_values.list";
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
    std::optional<market_input_values_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_market_input_values_request&,
                           const list_market_input_values_request&) = default;
};

struct list_market_input_values_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_input_value> values;
    std::uint64_t total{};

    friend bool operator==(const list_market_input_values_response&,
                           const list_market_input_values_response&) = default;
};

struct get_market_input_value_request {
    using response_type = struct get_market_input_value_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_input_values.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_input_value_key key;

    friend bool operator==(const get_market_input_value_request&,
                           const get_market_input_value_request&) = default;
};

struct get_market_input_value_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_input_value> market_input_value;

    friend bool operator==(const get_market_input_value_response&,
                           const get_market_input_value_response&) = default;
};

struct get_many_market_input_values_request {
    using response_type = struct get_many_market_input_values_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_input_values.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_input_value_key> keys;

    friend bool operator==(const get_many_market_input_values_request&,
                           const get_many_market_input_values_request&) = default;
};

struct get_many_market_input_values_response {
    ores::utility::domain::result result;
    std::vector<market_input_value_lookup> entries;

    friend bool operator==(const get_many_market_input_values_response&,
                           const get_many_market_input_values_response&) = default;
};

struct put_market_input_value_request {
    using response_type = struct put_market_input_value_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_input_values.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_input_value_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_market_input_value_request&,
                           const put_market_input_value_request&) = default;
};

struct put_market_input_value_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::market_input_value> market_input_value;

    friend bool operator==(const put_market_input_value_response&,
                           const put_market_input_value_response&) = default;
};

struct put_many_market_input_values_request {
    using response_type = struct put_many_market_input_values_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_input_values.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_input_value_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_market_input_values_request&,
                           const put_many_market_input_values_request&) = default;
};

struct put_many_market_input_values_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::market_input_value> values;

    friend bool operator==(const put_many_market_input_values_response&,
                           const put_many_market_input_values_response&) = default;
};

struct delete_market_input_value_request {
    using response_type = struct delete_market_input_value_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_input_values.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    market_input_value_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_market_input_value_request&,
                           const delete_market_input_value_request&) = default;
};

struct delete_market_input_value_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_market_input_value_response&,
                           const delete_market_input_value_response&) = default;
};

struct delete_many_market_input_values_request {
    using response_type = struct delete_many_market_input_values_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_input_values.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<market_input_value_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_market_input_values_request&,
                           const delete_many_market_input_values_request&) = default;
};

struct delete_many_market_input_values_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_market_input_values_response&,
                           const delete_many_market_input_values_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace market_input_value_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.market_input_values.created";
inline constexpr std::string_view updated = "marketdata.v1.events.market_input_values.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.market_input_values.deleted";
}
/**
 * @brief The members of market_input_value_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_input_value_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &market_input_value_key::id},
    };
}

/**
 * @brief The members of market_input_value_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_input_value_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &market_input_value_write::id},
        utility::codec::field{"party_id", &market_input_value_write::party_id},
        utility::codec::field{"report_instance_id", &market_input_value_write::report_instance_id},
        utility::codec::field{"result_id", &market_input_value_write::result_id},
        utility::codec::field{"source", &market_input_value_write::source},
        utility::codec::field{"line", &market_input_value_write::line},
        utility::codec::field{"as_of", &market_input_value_write::as_of},
        utility::codec::field{"report_definition_id",
                              &market_input_value_write::report_definition_id},
        utility::codec::field{"family", &market_input_value_write::family},
        utility::codec::field{"datum_id", &market_input_value_write::datum_id},
        utility::codec::field{"value", &market_input_value_write::value},
        utility::codec::field{"payment_date", &market_input_value_write::payment_date},
    };
}

/**
 * @brief The members of market_input_value_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_input_value_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &market_input_value_change::write},
        utility::codec::field{"precondition", &market_input_value_change::precondition},
    };
}

/**
 * @brief The members of market_input_value_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_input_value_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &market_input_value_removal::key},
        utility::codec::field{"precondition", &market_input_value_removal::precondition},
    };
}

/**
 * @brief The members of market_input_value_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_input_value_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &market_input_value_lookup::key},
        utility::codec::field{"market_input_value", &market_input_value_lookup::market_input_value},
    };
}

/**
 * @brief The members of market_input_values_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_input_values_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"report_instance_id",
                              &market_input_values_filter::report_instance_id},
        utility::codec::field{"family", &market_input_values_filter::family},
        utility::codec::field{"datum_id", &market_input_values_filter::datum_id},
        utility::codec::field{"id_one_of", &market_input_values_filter::id_one_of},
        utility::codec::field{"report_instance_id_one_of",
                              &market_input_values_filter::report_instance_id_one_of},
        utility::codec::field{"family_one_of", &market_input_values_filter::family_one_of},
        utility::codec::field{"datum_id_one_of", &market_input_values_filter::datum_id_one_of},
    };
}

/**
 * @brief The members of market_input_value_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_input_value_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &market_input_value_event::event_id},
        utility::codec::field{"key", &market_input_value_event::key},
        utility::codec::field{"action", &market_input_value_event::action},
        utility::codec::field{"version", &market_input_value_event::version},
        utility::codec::field{"occurred_at", &market_input_value_event::occurred_at},
        utility::codec::field{"correlation_id", &market_input_value_event::correlation_id},
    };
}

/**
 * @brief The members of list_market_input_values_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_market_input_values_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_market_input_values_request::offset},
        utility::codec::field{"limit", &list_market_input_values_request::limit},
        utility::codec::field{"order", &list_market_input_values_request::order},
        utility::codec::field{"filter", &list_market_input_values_request::filter},
        utility::codec::field{"as_of", &list_market_input_values_request::as_of},
    };
}

/**
 * @brief The members of list_market_input_values_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_market_input_values_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_market_input_values_response::result},
        utility::codec::field{"values", &list_market_input_values_response::values},
        utility::codec::field{"total", &list_market_input_values_response::total},
    };
}

/**
 * @brief The members of get_market_input_value_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_input_value_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_market_input_value_request::key},
    };
}

/**
 * @brief The members of get_market_input_value_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_market_input_value_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_market_input_value_response::result},
        utility::codec::field{"market_input_value",
                              &get_market_input_value_response::market_input_value},
    };
}

/**
 * @brief The members of get_many_market_input_values_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_market_input_values_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_market_input_values_request::keys},
    };
}

/**
 * @brief The members of get_many_market_input_values_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_market_input_values_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_market_input_values_response::result},
        utility::codec::field{"entries", &get_many_market_input_values_response::entries},
    };
}

/**
 * @brief The members of put_market_input_value_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_market_input_value_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_market_input_value_request::change},
        utility::codec::field{"intent", &put_market_input_value_request::intent},
    };
}

/**
 * @brief The members of put_market_input_value_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_market_input_value_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_market_input_value_response::result},
        utility::codec::field{"market_input_value",
                              &put_market_input_value_response::market_input_value},
    };
}

/**
 * @brief The members of put_many_market_input_values_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_market_input_values_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_market_input_values_request::changes},
        utility::codec::field{"intent", &put_many_market_input_values_request::intent},
    };
}

/**
 * @brief The members of put_many_market_input_values_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_market_input_values_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_market_input_values_response::result},
        utility::codec::field{"values", &put_many_market_input_values_response::values},
    };
}

/**
 * @brief The members of delete_market_input_value_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_market_input_value_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_market_input_value_request::removal},
        utility::codec::field{"intent", &delete_market_input_value_request::intent},
    };
}

/**
 * @brief The members of delete_market_input_value_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_market_input_value_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_market_input_value_response::result},
    };
}

/**
 * @brief The members of delete_many_market_input_values_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_input_values_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_market_input_values_request::removals},
        utility::codec::field{"intent", &delete_many_market_input_values_request::intent},
    };
}

/**
 * @brief The members of delete_many_market_input_values_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_market_input_values_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_market_input_values_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_input_value_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_input_value_key& v);
    static ores::marketdata::messaging::market_input_value_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_input_value_key& v);
    static ores::marketdata::messaging::market_input_value_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_input_value_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_input_value_write& v);
    static ores::marketdata::messaging::market_input_value_write read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_input_value_write& v);
    static ores::marketdata::messaging::market_input_value_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_input_value_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_input_value_change& v);
    static ores::marketdata::messaging::market_input_value_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_input_value_change& v);
    static ores::marketdata::messaging::market_input_value_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_input_value_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_input_value_removal& v);
    static ores::marketdata::messaging::market_input_value_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_input_value_removal& v);
    static ores::marketdata::messaging::market_input_value_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_input_value_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_input_value_lookup& v);
    static ores::marketdata::messaging::market_input_value_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_input_value_lookup& v);
    static ores::marketdata::messaging::market_input_value_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_input_values_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_input_values_filter& v);
    static ores::marketdata::messaging::market_input_values_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_input_values_filter& v);
    static ores::marketdata::messaging::market_input_values_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::market_input_value_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_input_value_event& v);
    static ores::marketdata::messaging::market_input_value_event read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::market_input_value_event& v);
    static ores::marketdata::messaging::market_input_value_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_input_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_input_values_request& v);
    static ores::marketdata::messaging::list_market_input_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_input_values_request& v);
    static ores::marketdata::messaging::list_market_input_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_market_input_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_market_input_values_response& v);
    static ores::marketdata::messaging::list_market_input_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_market_input_values_response& v);
    static ores::marketdata::messaging::list_market_input_values_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_input_value_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_input_value_request& v);
    static ores::marketdata::messaging::get_market_input_value_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_input_value_request& v);
    static ores::marketdata::messaging::get_market_input_value_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_market_input_value_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_market_input_value_response& v);
    static ores::marketdata::messaging::get_market_input_value_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_market_input_value_response& v);
    static ores::marketdata::messaging::get_market_input_value_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_input_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_input_values_request& v);
    static ores::marketdata::messaging::get_many_market_input_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_input_values_request& v);
    static ores::marketdata::messaging::get_many_market_input_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_market_input_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_market_input_values_response& v);
    static ores::marketdata::messaging::get_many_market_input_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_market_input_values_response& v);
    static ores::marketdata::messaging::get_many_market_input_values_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_input_value_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_input_value_request& v);
    static ores::marketdata::messaging::put_market_input_value_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_input_value_request& v);
    static ores::marketdata::messaging::put_market_input_value_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_market_input_value_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_market_input_value_response& v);
    static ores::marketdata::messaging::put_market_input_value_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_market_input_value_response& v);
    static ores::marketdata::messaging::put_market_input_value_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_input_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_input_values_request& v);
    static ores::marketdata::messaging::put_many_market_input_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_input_values_request& v);
    static ores::marketdata::messaging::put_many_market_input_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_market_input_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_market_input_values_response& v);
    static ores::marketdata::messaging::put_many_market_input_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_market_input_values_response& v);
    static ores::marketdata::messaging::put_many_market_input_values_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_input_value_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_input_value_request& v);
    static ores::marketdata::messaging::delete_market_input_value_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_input_value_request& v);
    static ores::marketdata::messaging::delete_market_input_value_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_market_input_value_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_market_input_value_response& v);
    static ores::marketdata::messaging::delete_market_input_value_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_market_input_value_response& v);
    static ores::marketdata::messaging::delete_market_input_value_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_input_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_input_values_request& v);
    static ores::marketdata::messaging::delete_many_market_input_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_input_values_request& v);
    static ores::marketdata::messaging::delete_many_market_input_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_market_input_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_market_input_values_response& v);
    static ores::marketdata::messaging::delete_many_market_input_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_market_input_values_response& v);
    static ores::marketdata::messaging::delete_many_market_input_values_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
