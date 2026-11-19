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
#ifndef ORES_TRADING_API_MESSAGING_CBO_STRUCTURE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_CBO_STRUCTURE_PROTOCOL_HPP

#include "ores.trading.api/domain/cbo_structure_codec.hpp"
#include "ores.trading.api/export.hpp"
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

namespace ores::trading::messaging {

struct cbo_structure_key {
    boost::uuids::uuid trade_id;

    friend bool operator==(const cbo_structure_key&, const cbo_structure_key&) = default;
};

struct cbo_structure_write {
    boost::uuids::uuid trade_id;
    boost::uuids::uuid trade_activity_id;
    std::string day_counter;
    std::string payment_convention;
    std::string currency;
    std::optional<std::chrono::year_month_day> reinvestment_end_date;
    ores::utility::decimal::decimal senior_fee;
    std::string fee_day_counter;
    ores::utility::decimal::decimal subordinated_fee;
    ores::utility::decimal::decimal equity_kicker;

    friend bool operator==(const cbo_structure_write&, const cbo_structure_write&) = default;
};

struct cbo_structure_change {
    cbo_structure_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const cbo_structure_change&, const cbo_structure_change&) = default;
};

struct cbo_structure_removal {
    cbo_structure_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const cbo_structure_removal&, const cbo_structure_removal&) = default;
};

struct cbo_structure_lookup {
    cbo_structure_key key;
    std::optional<ores::trading::domain::cbo_structure> cbo_structure;

    friend bool operator==(const cbo_structure_lookup&, const cbo_structure_lookup&) = default;
};

struct cbo_structures_filter {
    std::optional<boost::uuids::uuid> trade_id;
    std::optional<std::vector<boost::uuids::uuid>> trade_id_one_of;

    friend bool operator==(const cbo_structures_filter&, const cbo_structures_filter&) = default;
};

struct cbo_structure_event {
    boost::uuids::uuid event_id;
    cbo_structure_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const cbo_structure_event&, const cbo_structure_event&) = default;
};

struct cbo_structure_version_key {
    cbo_structure_key cbo_structure;
    std::uint32_t version{};

    friend bool operator==(const cbo_structure_version_key&,
                           const cbo_structure_version_key&) = default;
};

struct cbo_structure_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const cbo_structure_versions_filter&,
                           const cbo_structure_versions_filter&) = default;
};

struct list_cbo_structures_request {
    using response_type = struct list_cbo_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.list";
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
    std::optional<cbo_structures_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_cbo_structures_request&,
                           const list_cbo_structures_request&) = default;
};

struct list_cbo_structures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_structure> cbo_structures;
    std::uint64_t total{};

    friend bool operator==(const list_cbo_structures_response&,
                           const list_cbo_structures_response&) = default;
};

struct get_cbo_structure_request {
    using response_type = struct get_cbo_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_structure_key key;

    friend bool operator==(const get_cbo_structure_request&,
                           const get_cbo_structure_request&) = default;
};

struct get_cbo_structure_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::cbo_structure> cbo_structure;

    friend bool operator==(const get_cbo_structure_response&,
                           const get_cbo_structure_response&) = default;
};

struct get_many_cbo_structures_request {
    using response_type = struct get_many_cbo_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cbo_structure_key> keys;

    friend bool operator==(const get_many_cbo_structures_request&,
                           const get_many_cbo_structures_request&) = default;
};

struct get_many_cbo_structures_response {
    ores::utility::domain::result result;
    std::vector<cbo_structure_lookup> entries;

    friend bool operator==(const get_many_cbo_structures_response&,
                           const get_many_cbo_structures_response&) = default;
};

struct put_cbo_structure_request {
    using response_type = struct put_cbo_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_structure_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_cbo_structure_request&,
                           const put_cbo_structure_request&) = default;
};

struct put_cbo_structure_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::cbo_structure> cbo_structure;

    friend bool operator==(const put_cbo_structure_response&,
                           const put_cbo_structure_response&) = default;
};

struct put_many_cbo_structures_request {
    using response_type = struct put_many_cbo_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cbo_structure_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_cbo_structures_request&,
                           const put_many_cbo_structures_request&) = default;
};

struct put_many_cbo_structures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_structure> cbo_structures;

    friend bool operator==(const put_many_cbo_structures_response&,
                           const put_many_cbo_structures_response&) = default;
};

struct delete_cbo_structure_request {
    using response_type = struct delete_cbo_structure_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_structure_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_cbo_structure_request&,
                           const delete_cbo_structure_request&) = default;
};

struct delete_cbo_structure_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_cbo_structure_response&,
                           const delete_cbo_structure_response&) = default;
};

struct delete_many_cbo_structures_request {
    using response_type = struct delete_many_cbo_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cbo_structure_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_cbo_structures_request&,
                           const delete_many_cbo_structures_request&) = default;
};

struct delete_many_cbo_structures_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_cbo_structures_response&,
                           const delete_many_cbo_structures_response&) = default;
};

struct list_by_trade_id_cbo_structures_request {
    using response_type = struct list_by_trade_id_cbo_structures_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures.list_by_trade_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid trade_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cbo_structures_filter> filter;

    friend bool operator==(const list_by_trade_id_cbo_structures_request&,
                           const list_by_trade_id_cbo_structures_request&) = default;
};

struct list_by_trade_id_cbo_structures_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_structure> cbo_structures;
    std::uint64_t total{};

    friend bool operator==(const list_by_trade_id_cbo_structures_response&,
                           const list_by_trade_id_cbo_structures_response&) = default;
};

struct list_cbo_structure_versions_request {
    using response_type = struct list_cbo_structure_versions_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_structure_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cbo_structure_versions_filter> filter;

    friend bool operator==(const list_cbo_structure_versions_request&,
                           const list_cbo_structure_versions_request&) = default;
};

struct list_cbo_structure_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::cbo_structure> versions;
    std::uint64_t total{};

    friend bool operator==(const list_cbo_structure_versions_response&,
                           const list_cbo_structure_versions_response&) = default;
};

struct get_cbo_structure_version_request {
    using response_type = struct get_cbo_structure_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.cbo_structures_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cbo_structure_version_key key;

    friend bool operator==(const get_cbo_structure_version_request&,
                           const get_cbo_structure_version_request&) = default;
};

struct get_cbo_structure_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::cbo_structure> version;

    friend bool operator==(const get_cbo_structure_version_response&,
                           const get_cbo_structure_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cbo_structure_event_subjects {
inline constexpr std::string_view created = "trading.v1.events.cbo_structures.created";
inline constexpr std::string_view updated = "trading.v1.events.cbo_structures.updated";
inline constexpr std::string_view deleted = "trading.v1.events.cbo_structures.deleted";
}
/**
 * @brief The members of cbo_structure_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &cbo_structure_key::trade_id},
    };
}

/**
 * @brief The members of cbo_structure_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &cbo_structure_write::trade_id},
        utility::codec::field{"trade_activity_id", &cbo_structure_write::trade_activity_id},
        utility::codec::field{"day_counter", &cbo_structure_write::day_counter},
        utility::codec::field{"payment_convention", &cbo_structure_write::payment_convention},
        utility::codec::field{"currency", &cbo_structure_write::currency},
        utility::codec::field{"reinvestment_end_date", &cbo_structure_write::reinvestment_end_date},
        utility::codec::field{"senior_fee", &cbo_structure_write::senior_fee},
        utility::codec::field{"fee_day_counter", &cbo_structure_write::fee_day_counter},
        utility::codec::field{"subordinated_fee", &cbo_structure_write::subordinated_fee},
        utility::codec::field{"equity_kicker", &cbo_structure_write::equity_kicker},
    };
}

/**
 * @brief The members of cbo_structure_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &cbo_structure_change::write},
        utility::codec::field{"precondition", &cbo_structure_change::precondition},
    };
}

/**
 * @brief The members of cbo_structure_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &cbo_structure_removal::key},
        utility::codec::field{"precondition", &cbo_structure_removal::precondition},
    };
}

/**
 * @brief The members of cbo_structure_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &cbo_structure_lookup::key},
        utility::codec::field{"cbo_structure", &cbo_structure_lookup::cbo_structure},
    };
}

/**
 * @brief The members of cbo_structures_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structures_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &cbo_structures_filter::trade_id},
        utility::codec::field{"trade_id_one_of", &cbo_structures_filter::trade_id_one_of},
    };
}

/**
 * @brief The members of cbo_structure_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &cbo_structure_event::event_id},
        utility::codec::field{"key", &cbo_structure_event::key},
        utility::codec::field{"action", &cbo_structure_event::action},
        utility::codec::field{"version", &cbo_structure_event::version},
        utility::codec::field{"occurred_at", &cbo_structure_event::occurred_at},
        utility::codec::field{"correlation_id", &cbo_structure_event::correlation_id},
    };
}

/**
 * @brief The members of cbo_structure_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"cbo_structure", &cbo_structure_version_key::cbo_structure},
        utility::codec::field{"version", &cbo_structure_version_key::version},
    };
}

/**
 * @brief The members of cbo_structure_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cbo_structure_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &cbo_structure_versions_filter::version},
        utility::codec::field{"from_version", &cbo_structure_versions_filter::from_version},
        utility::codec::field{"to_version", &cbo_structure_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cbo_structures_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cbo_structures_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_cbo_structures_request::offset},
        utility::codec::field{"limit", &list_cbo_structures_request::limit},
        utility::codec::field{"order", &list_cbo_structures_request::order},
        utility::codec::field{"filter", &list_cbo_structures_request::filter},
        utility::codec::field{"as_of", &list_cbo_structures_request::as_of},
    };
}

/**
 * @brief The members of list_cbo_structures_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cbo_structures_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_cbo_structures_response::result},
        utility::codec::field{"cbo_structures", &list_cbo_structures_response::cbo_structures},
        utility::codec::field{"total", &list_cbo_structures_response::total},
    };
}

/**
 * @brief The members of get_cbo_structure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cbo_structure_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_cbo_structure_request::key},
    };
}

/**
 * @brief The members of get_cbo_structure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cbo_structure_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_cbo_structure_response::result},
        utility::codec::field{"cbo_structure", &get_cbo_structure_response::cbo_structure},
    };
}

/**
 * @brief The members of get_many_cbo_structures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cbo_structures_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_cbo_structures_request::keys},
    };
}

/**
 * @brief The members of get_many_cbo_structures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cbo_structures_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_cbo_structures_response::result},
        utility::codec::field{"entries", &get_many_cbo_structures_response::entries},
    };
}

/**
 * @brief The members of put_cbo_structure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_cbo_structure_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_cbo_structure_request::change},
        utility::codec::field{"intent", &put_cbo_structure_request::intent},
    };
}

/**
 * @brief The members of put_cbo_structure_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_cbo_structure_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_cbo_structure_response::result},
        utility::codec::field{"cbo_structure", &put_cbo_structure_response::cbo_structure},
    };
}

/**
 * @brief The members of put_many_cbo_structures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_cbo_structures_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_cbo_structures_request::changes},
        utility::codec::field{"intent", &put_many_cbo_structures_request::intent},
    };
}

/**
 * @brief The members of put_many_cbo_structures_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_cbo_structures_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_cbo_structures_response::result},
        utility::codec::field{"cbo_structures", &put_many_cbo_structures_response::cbo_structures},
    };
}

/**
 * @brief The members of delete_cbo_structure_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_cbo_structure_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_cbo_structure_request::removal},
        utility::codec::field{"intent", &delete_cbo_structure_request::intent},
    };
}

/**
 * @brief The members of delete_cbo_structure_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_cbo_structure_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_cbo_structure_response::result},
    };
}

/**
 * @brief The members of delete_many_cbo_structures_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_cbo_structures_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_cbo_structures_request::removals},
        utility::codec::field{"intent", &delete_many_cbo_structures_request::intent},
    };
}

/**
 * @brief The members of delete_many_cbo_structures_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_cbo_structures_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_cbo_structures_response::result},
    };
}

/**
 * @brief The members of list_by_trade_id_cbo_structures_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_cbo_structures_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"trade_id", &list_by_trade_id_cbo_structures_request::trade_id},
        utility::codec::field{"scope", &list_by_trade_id_cbo_structures_request::scope},
        utility::codec::field{"offset", &list_by_trade_id_cbo_structures_request::offset},
        utility::codec::field{"limit", &list_by_trade_id_cbo_structures_request::limit},
        utility::codec::field{"order", &list_by_trade_id_cbo_structures_request::order},
        utility::codec::field{"filter", &list_by_trade_id_cbo_structures_request::filter},
    };
}

/**
 * @brief The members of list_by_trade_id_cbo_structures_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_trade_id_cbo_structures_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_trade_id_cbo_structures_response::result},
        utility::codec::field{"cbo_structures",
                              &list_by_trade_id_cbo_structures_response::cbo_structures},
        utility::codec::field{"total", &list_by_trade_id_cbo_structures_response::total},
    };
}

/**
 * @brief The members of list_cbo_structure_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cbo_structure_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_cbo_structure_versions_request::key},
        utility::codec::field{"offset", &list_cbo_structure_versions_request::offset},
        utility::codec::field{"limit", &list_cbo_structure_versions_request::limit},
        utility::codec::field{"order", &list_cbo_structure_versions_request::order},
        utility::codec::field{"filter", &list_cbo_structure_versions_request::filter},
    };
}

/**
 * @brief The members of list_cbo_structure_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cbo_structure_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_cbo_structure_versions_response::result},
        utility::codec::field{"versions", &list_cbo_structure_versions_response::versions},
        utility::codec::field{"total", &list_cbo_structure_versions_response::total},
    };
}

/**
 * @brief The members of get_cbo_structure_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cbo_structure_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_cbo_structure_version_request::key},
    };
}

/**
 * @brief The members of get_cbo_structure_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cbo_structure_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_cbo_structure_version_response::result},
        utility::codec::field{"version", &get_cbo_structure_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structure_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_key& v);
    static ores::trading::messaging::cbo_structure_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structure_key& v);
    static ores::trading::messaging::cbo_structure_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structure_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_write& v);
    static ores::trading::messaging::cbo_structure_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structure_write& v);
    static ores::trading::messaging::cbo_structure_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structure_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_change& v);
    static ores::trading::messaging::cbo_structure_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structure_change& v);
    static ores::trading::messaging::cbo_structure_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structure_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_removal& v);
    static ores::trading::messaging::cbo_structure_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structure_removal& v);
    static ores::trading::messaging::cbo_structure_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structure_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_lookup& v);
    static ores::trading::messaging::cbo_structure_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structure_lookup& v);
    static ores::trading::messaging::cbo_structure_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structures_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structures_filter& v);
    static ores::trading::messaging::cbo_structures_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structures_filter& v);
    static ores::trading::messaging::cbo_structures_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structure_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_event& v);
    static ores::trading::messaging::cbo_structure_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structure_event& v);
    static ores::trading::messaging::cbo_structure_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::cbo_structure_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_version_key& v);
    static ores::trading::messaging::cbo_structure_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::cbo_structure_version_key& v);
    static ores::trading::messaging::cbo_structure_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::cbo_structure_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::cbo_structure_versions_filter& v);
    static ores::trading::messaging::cbo_structure_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::cbo_structure_versions_filter& v);
    static ores::trading::messaging::cbo_structure_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_cbo_structures_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_cbo_structures_request& v);
    static ores::trading::messaging::list_cbo_structures_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_structures_request& v);
    static ores::trading::messaging::list_cbo_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::list_cbo_structures_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::list_cbo_structures_response& v);
    static ores::trading::messaging::list_cbo_structures_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_structures_response& v);
    static ores::trading::messaging::list_cbo_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_cbo_structure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_cbo_structure_request& v);
    static ores::trading::messaging::get_cbo_structure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_cbo_structure_request& v);
    static ores::trading::messaging::get_cbo_structure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::get_cbo_structure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::get_cbo_structure_response& v);
    static ores::trading::messaging::get_cbo_structure_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::get_cbo_structure_response& v);
    static ores::trading::messaging::get_cbo_structure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_cbo_structures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_cbo_structures_request& v);
    static ores::trading::messaging::get_many_cbo_structures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_cbo_structures_request& v);
    static ores::trading::messaging::get_many_cbo_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_many_cbo_structures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_many_cbo_structures_response& v);
    static ores::trading::messaging::get_many_cbo_structures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_many_cbo_structures_response& v);
    static ores::trading::messaging::get_many_cbo_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_cbo_structure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_cbo_structure_request& v);
    static ores::trading::messaging::put_cbo_structure_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_cbo_structure_request& v);
    static ores::trading::messaging::put_cbo_structure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::put_cbo_structure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::put_cbo_structure_response& v);
    static ores::trading::messaging::put_cbo_structure_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::trading::messaging::put_cbo_structure_response& v);
    static ores::trading::messaging::put_cbo_structure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_cbo_structures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_cbo_structures_request& v);
    static ores::trading::messaging::put_many_cbo_structures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_cbo_structures_request& v);
    static ores::trading::messaging::put_many_cbo_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::put_many_cbo_structures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::put_many_cbo_structures_response& v);
    static ores::trading::messaging::put_many_cbo_structures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::put_many_cbo_structures_response& v);
    static ores::trading::messaging::put_many_cbo_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT wire_traits<ores::trading::messaging::delete_cbo_structure_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_cbo_structure_request& v);
    static ores::trading::messaging::delete_cbo_structure_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_cbo_structure_request& v);
    static ores::trading::messaging::delete_cbo_structure_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_cbo_structure_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::trading::messaging::delete_cbo_structure_response& v);
    static ores::trading::messaging::delete_cbo_structure_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_cbo_structure_response& v);
    static ores::trading::messaging::delete_cbo_structure_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_cbo_structures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_cbo_structures_request& v);
    static ores::trading::messaging::delete_many_cbo_structures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_cbo_structures_request& v);
    static ores::trading::messaging::delete_many_cbo_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::delete_many_cbo_structures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::delete_many_cbo_structures_response& v);
    static ores::trading::messaging::delete_many_cbo_structures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::delete_many_cbo_structures_response& v);
    static ores::trading::messaging::delete_many_cbo_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_cbo_structures_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_cbo_structures_request& v);
    static ores::trading::messaging::list_by_trade_id_cbo_structures_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_cbo_structures_request& v);
    static ores::trading::messaging::list_by_trade_id_cbo_structures_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_by_trade_id_cbo_structures_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_by_trade_id_cbo_structures_response& v);
    static ores::trading::messaging::list_by_trade_id_cbo_structures_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_by_trade_id_cbo_structures_response& v);
    static ores::trading::messaging::list_by_trade_id_cbo_structures_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_cbo_structure_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_cbo_structure_versions_request& v);
    static ores::trading::messaging::list_cbo_structure_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_structure_versions_request& v);
    static ores::trading::messaging::list_cbo_structure_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::list_cbo_structure_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::list_cbo_structure_versions_response& v);
    static ores::trading::messaging::list_cbo_structure_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::list_cbo_structure_versions_response& v);
    static ores::trading::messaging::list_cbo_structure_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_cbo_structure_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_cbo_structure_version_request& v);
    static ores::trading::messaging::get_cbo_structure_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_cbo_structure_version_request& v);
    static ores::trading::messaging::get_cbo_structure_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_TRADING_API_EXPORT
    wire_traits<ores::trading::messaging::get_cbo_structure_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::trading::messaging::get_cbo_structure_version_response& v);
    static ores::trading::messaging::get_cbo_structure_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::trading::messaging::get_cbo_structure_version_response& v);
    static ores::trading::messaging::get_cbo_structure_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
