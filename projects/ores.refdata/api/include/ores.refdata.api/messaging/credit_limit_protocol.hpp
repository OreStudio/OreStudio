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
#ifndef ORES_REFDATA_API_MESSAGING_CREDIT_LIMIT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CREDIT_LIMIT_PROTOCOL_HPP

#include "ores.refdata.api/domain/credit_limit_codec.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct credit_limit_key {
    std::string code;

    friend bool operator==(const credit_limit_key&, const credit_limit_key&) = default;
};

struct credit_limit_write {
    boost::uuids::uuid id;
    std::string code;
    boost::uuids::uuid party_id;
    boost::uuids::uuid counterparty_id;
    std::optional<ores::utility::decimal::decimal> limit_amount;
    std::optional<std::string> description;

    friend bool operator==(const credit_limit_write&, const credit_limit_write&) = default;
};

struct credit_limit_change {
    credit_limit_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const credit_limit_change&, const credit_limit_change&) = default;
};

struct credit_limit_removal {
    credit_limit_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const credit_limit_removal&, const credit_limit_removal&) = default;
};

struct credit_limit_lookup {
    credit_limit_key key;
    std::optional<ores::refdata::domain::credit_limit> credit_limit;

    friend bool operator==(const credit_limit_lookup&, const credit_limit_lookup&) = default;
};

struct credit_limits_filter {
    std::optional<boost::uuids::uuid> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;

    friend bool operator==(const credit_limits_filter&, const credit_limits_filter&) = default;
};

struct credit_limit_event {
    boost::uuids::uuid event_id;
    credit_limit_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const credit_limit_event&, const credit_limit_event&) = default;
};

struct credit_limit_version_key {
    credit_limit_key credit_limit;
    std::uint32_t version{};

    friend bool operator==(const credit_limit_version_key&,
                           const credit_limit_version_key&) = default;
};

struct credit_limit_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const credit_limit_versions_filter&,
                           const credit_limit_versions_filter&) = default;
};

struct list_credit_limits_request {
    using response_type = struct list_credit_limits_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits.list";
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
    std::optional<credit_limits_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_credit_limits_request&,
                           const list_credit_limits_request&) = default;
};

struct list_credit_limits_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::credit_limit> credit_limits;
    std::uint64_t total{};

    friend bool operator==(const list_credit_limits_response&,
                           const list_credit_limits_response&) = default;
};

struct get_credit_limit_request {
    using response_type = struct get_credit_limit_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_limit_key key;

    friend bool operator==(const get_credit_limit_request&,
                           const get_credit_limit_request&) = default;
};

struct get_credit_limit_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::credit_limit> credit_limit;

    friend bool operator==(const get_credit_limit_response&,
                           const get_credit_limit_response&) = default;
};

struct get_many_credit_limits_request {
    using response_type = struct get_many_credit_limits_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_limit_key> keys;

    friend bool operator==(const get_many_credit_limits_request&,
                           const get_many_credit_limits_request&) = default;
};

struct get_many_credit_limits_response {
    ores::utility::domain::result result;
    std::vector<credit_limit_lookup> entries;

    friend bool operator==(const get_many_credit_limits_response&,
                           const get_many_credit_limits_response&) = default;
};

struct put_credit_limit_request {
    using response_type = struct put_credit_limit_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_limit_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_credit_limit_request&,
                           const put_credit_limit_request&) = default;
};

struct put_credit_limit_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::credit_limit> credit_limit;

    friend bool operator==(const put_credit_limit_response&,
                           const put_credit_limit_response&) = default;
};

struct put_many_credit_limits_request {
    using response_type = struct put_many_credit_limits_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_limit_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_credit_limits_request&,
                           const put_many_credit_limits_request&) = default;
};

struct put_many_credit_limits_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::credit_limit> credit_limits;

    friend bool operator==(const put_many_credit_limits_response&,
                           const put_many_credit_limits_response&) = default;
};

struct delete_credit_limit_request {
    using response_type = struct delete_credit_limit_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_limit_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_credit_limit_request&,
                           const delete_credit_limit_request&) = default;
};

struct delete_credit_limit_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_credit_limit_response&,
                           const delete_credit_limit_response&) = default;
};

struct delete_many_credit_limits_request {
    using response_type = struct delete_many_credit_limits_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_limit_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_credit_limits_request&,
                           const delete_many_credit_limits_request&) = default;
};

struct delete_many_credit_limits_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_credit_limits_response&,
                           const delete_many_credit_limits_response&) = default;
};

struct list_by_counterparty_id_credit_limits_request {
    using response_type = struct list_by_counterparty_id_credit_limits_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.credit_limits.list_by_counterparty_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid counterparty_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<credit_limits_filter> filter;

    friend bool operator==(const list_by_counterparty_id_credit_limits_request&,
                           const list_by_counterparty_id_credit_limits_request&) = default;
};

struct list_by_counterparty_id_credit_limits_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::credit_limit> credit_limits;
    std::uint64_t total{};

    friend bool operator==(const list_by_counterparty_id_credit_limits_response&,
                           const list_by_counterparty_id_credit_limits_response&) = default;
};

struct list_credit_limit_versions_request {
    using response_type = struct list_credit_limit_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_limit_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<credit_limit_versions_filter> filter;

    friend bool operator==(const list_credit_limit_versions_request&,
                           const list_credit_limit_versions_request&) = default;
};

struct list_credit_limit_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::credit_limit> versions;
    std::uint64_t total{};

    friend bool operator==(const list_credit_limit_versions_response&,
                           const list_credit_limit_versions_response&) = default;
};

struct get_credit_limit_version_request {
    using response_type = struct get_credit_limit_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.credit_limits_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_limit_version_key key;

    friend bool operator==(const get_credit_limit_version_request&,
                           const get_credit_limit_version_request&) = default;
};

struct get_credit_limit_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::credit_limit> version;

    friend bool operator==(const get_credit_limit_version_response&,
                           const get_credit_limit_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace credit_limit_event_subjects {
inline constexpr std::string_view created = "refdata.v1.events.credit_limits.created";
inline constexpr std::string_view updated = "refdata.v1.events.credit_limits.updated";
inline constexpr std::string_view deleted = "refdata.v1.events.credit_limits.deleted";
}
/**
 * @brief The members of credit_limit_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &credit_limit_key::code},
    };
}

/**
 * @brief The members of credit_limit_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &credit_limit_write::id},
        utility::codec::field{"code", &credit_limit_write::code},
        utility::codec::field{"party_id", &credit_limit_write::party_id},
        utility::codec::field{"counterparty_id", &credit_limit_write::counterparty_id},
        utility::codec::field{"limit_amount", &credit_limit_write::limit_amount},
        utility::codec::field{"description", &credit_limit_write::description},
    };
}

/**
 * @brief The members of credit_limit_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &credit_limit_change::write},
        utility::codec::field{"precondition", &credit_limit_change::precondition},
    };
}

/**
 * @brief The members of credit_limit_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &credit_limit_removal::key},
        utility::codec::field{"precondition", &credit_limit_removal::precondition},
    };
}

/**
 * @brief The members of credit_limit_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &credit_limit_lookup::key},
        utility::codec::field{"credit_limit", &credit_limit_lookup::credit_limit},
    };
}

/**
 * @brief The members of credit_limits_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limits_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"counterparty_id", &credit_limits_filter::counterparty_id},
        utility::codec::field{"id_one_of", &credit_limits_filter::id_one_of},
        utility::codec::field{"counterparty_id_one_of",
                              &credit_limits_filter::counterparty_id_one_of},
    };
}

/**
 * @brief The members of credit_limit_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &credit_limit_event::event_id},
        utility::codec::field{"key", &credit_limit_event::key},
        utility::codec::field{"action", &credit_limit_event::action},
        utility::codec::field{"version", &credit_limit_event::version},
        utility::codec::field{"occurred_at", &credit_limit_event::occurred_at},
        utility::codec::field{"correlation_id", &credit_limit_event::correlation_id},
    };
}

/**
 * @brief The members of credit_limit_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"credit_limit", &credit_limit_version_key::credit_limit},
        utility::codec::field{"version", &credit_limit_version_key::version},
    };
}

/**
 * @brief The members of credit_limit_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<credit_limit_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &credit_limit_versions_filter::version},
        utility::codec::field{"from_version", &credit_limit_versions_filter::from_version},
        utility::codec::field{"to_version", &credit_limit_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_credit_limits_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_credit_limits_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_credit_limits_request::offset},
        utility::codec::field{"limit", &list_credit_limits_request::limit},
        utility::codec::field{"order", &list_credit_limits_request::order},
        utility::codec::field{"filter", &list_credit_limits_request::filter},
        utility::codec::field{"as_of", &list_credit_limits_request::as_of},
    };
}

/**
 * @brief The members of list_credit_limits_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_credit_limits_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_credit_limits_response::result},
        utility::codec::field{"credit_limits", &list_credit_limits_response::credit_limits},
        utility::codec::field{"total", &list_credit_limits_response::total},
    };
}

/**
 * @brief The members of get_credit_limit_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_credit_limit_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_credit_limit_request::key},
    };
}

/**
 * @brief The members of get_credit_limit_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_credit_limit_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_credit_limit_response::result},
        utility::codec::field{"credit_limit", &get_credit_limit_response::credit_limit},
    };
}

/**
 * @brief The members of get_many_credit_limits_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_credit_limits_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_credit_limits_request::keys},
    };
}

/**
 * @brief The members of get_many_credit_limits_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_credit_limits_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_credit_limits_response::result},
        utility::codec::field{"entries", &get_many_credit_limits_response::entries},
    };
}

/**
 * @brief The members of put_credit_limit_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_credit_limit_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_credit_limit_request::change},
        utility::codec::field{"intent", &put_credit_limit_request::intent},
    };
}

/**
 * @brief The members of put_credit_limit_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_credit_limit_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_credit_limit_response::result},
        utility::codec::field{"credit_limit", &put_credit_limit_response::credit_limit},
    };
}

/**
 * @brief The members of put_many_credit_limits_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_credit_limits_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_credit_limits_request::changes},
        utility::codec::field{"intent", &put_many_credit_limits_request::intent},
    };
}

/**
 * @brief The members of put_many_credit_limits_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_credit_limits_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_credit_limits_response::result},
        utility::codec::field{"credit_limits", &put_many_credit_limits_response::credit_limits},
    };
}

/**
 * @brief The members of delete_credit_limit_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_credit_limit_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_credit_limit_request::removal},
        utility::codec::field{"intent", &delete_credit_limit_request::intent},
    };
}

/**
 * @brief The members of delete_credit_limit_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_credit_limit_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_credit_limit_response::result},
    };
}

/**
 * @brief The members of delete_many_credit_limits_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_credit_limits_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_credit_limits_request::removals},
        utility::codec::field{"intent", &delete_many_credit_limits_request::intent},
    };
}

/**
 * @brief The members of delete_many_credit_limits_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_credit_limits_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_credit_limits_response::result},
    };
}

/**
 * @brief The members of list_by_counterparty_id_credit_limits_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_counterparty_id_credit_limits_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"counterparty_id",
                              &list_by_counterparty_id_credit_limits_request::counterparty_id},
        utility::codec::field{"scope", &list_by_counterparty_id_credit_limits_request::scope},
        utility::codec::field{"offset", &list_by_counterparty_id_credit_limits_request::offset},
        utility::codec::field{"limit", &list_by_counterparty_id_credit_limits_request::limit},
        utility::codec::field{"order", &list_by_counterparty_id_credit_limits_request::order},
        utility::codec::field{"filter", &list_by_counterparty_id_credit_limits_request::filter},
    };
}

/**
 * @brief The members of list_by_counterparty_id_credit_limits_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_counterparty_id_credit_limits_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_counterparty_id_credit_limits_response::result},
        utility::codec::field{"credit_limits",
                              &list_by_counterparty_id_credit_limits_response::credit_limits},
        utility::codec::field{"total", &list_by_counterparty_id_credit_limits_response::total},
    };
}

/**
 * @brief The members of list_credit_limit_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_credit_limit_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_credit_limit_versions_request::key},
        utility::codec::field{"offset", &list_credit_limit_versions_request::offset},
        utility::codec::field{"limit", &list_credit_limit_versions_request::limit},
        utility::codec::field{"order", &list_credit_limit_versions_request::order},
        utility::codec::field{"filter", &list_credit_limit_versions_request::filter},
    };
}

/**
 * @brief The members of list_credit_limit_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_credit_limit_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_credit_limit_versions_response::result},
        utility::codec::field{"versions", &list_credit_limit_versions_response::versions},
        utility::codec::field{"total", &list_credit_limit_versions_response::total},
    };
}

/**
 * @brief The members of get_credit_limit_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_credit_limit_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_credit_limit_version_request::key},
    };
}

/**
 * @brief The members of get_credit_limit_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_credit_limit_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_credit_limit_version_response::result},
        utility::codec::field{"version", &get_credit_limit_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_key& v);
    static ores::refdata::messaging::credit_limit_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limit_key& v);
    static ores::refdata::messaging::credit_limit_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_write& v);
    static ores::refdata::messaging::credit_limit_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limit_write& v);
    static ores::refdata::messaging::credit_limit_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_change& v);
    static ores::refdata::messaging::credit_limit_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limit_change& v);
    static ores::refdata::messaging::credit_limit_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_removal& v);
    static ores::refdata::messaging::credit_limit_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limit_removal& v);
    static ores::refdata::messaging::credit_limit_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_lookup& v);
    static ores::refdata::messaging::credit_limit_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limit_lookup& v);
    static ores::refdata::messaging::credit_limit_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limits_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limits_filter& v);
    static ores::refdata::messaging::credit_limits_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limits_filter& v);
    static ores::refdata::messaging::credit_limits_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_event& v);
    static ores::refdata::messaging::credit_limit_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limit_event& v);
    static ores::refdata::messaging::credit_limit_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_version_key& v);
    static ores::refdata::messaging::credit_limit_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::credit_limit_version_key& v);
    static ores::refdata::messaging::credit_limit_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::credit_limit_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::credit_limit_versions_filter& v);
    static ores::refdata::messaging::credit_limit_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::credit_limit_versions_filter& v);
    static ores::refdata::messaging::credit_limit_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_credit_limits_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_credit_limits_request& v);
    static ores::refdata::messaging::list_credit_limits_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::list_credit_limits_request& v);
    static ores::refdata::messaging::list_credit_limits_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_credit_limits_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_credit_limits_response& v);
    static ores::refdata::messaging::list_credit_limits_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_credit_limits_response& v);
    static ores::refdata::messaging::list_credit_limits_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_credit_limit_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_credit_limit_request& v);
    static ores::refdata::messaging::get_credit_limit_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_credit_limit_request& v);
    static ores::refdata::messaging::get_credit_limit_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_credit_limit_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_credit_limit_response& v);
    static ores::refdata::messaging::get_credit_limit_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_credit_limit_response& v);
    static ores::refdata::messaging::get_credit_limit_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_credit_limits_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_credit_limits_request& v);
    static ores::refdata::messaging::get_many_credit_limits_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_credit_limits_request& v);
    static ores::refdata::messaging::get_many_credit_limits_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_credit_limits_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_credit_limits_response& v);
    static ores::refdata::messaging::get_many_credit_limits_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_credit_limits_response& v);
    static ores::refdata::messaging::get_many_credit_limits_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_credit_limit_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_credit_limit_request& v);
    static ores::refdata::messaging::put_credit_limit_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_credit_limit_request& v);
    static ores::refdata::messaging::put_credit_limit_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_credit_limit_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_credit_limit_response& v);
    static ores::refdata::messaging::put_credit_limit_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_credit_limit_response& v);
    static ores::refdata::messaging::put_credit_limit_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_credit_limits_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_credit_limits_request& v);
    static ores::refdata::messaging::put_many_credit_limits_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_credit_limits_request& v);
    static ores::refdata::messaging::put_many_credit_limits_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_credit_limits_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_credit_limits_response& v);
    static ores::refdata::messaging::put_many_credit_limits_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_credit_limits_response& v);
    static ores::refdata::messaging::put_many_credit_limits_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_credit_limit_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_credit_limit_request& v);
    static ores::refdata::messaging::delete_credit_limit_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_credit_limit_request& v);
    static ores::refdata::messaging::delete_credit_limit_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_credit_limit_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_credit_limit_response& v);
    static ores::refdata::messaging::delete_credit_limit_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_credit_limit_response& v);
    static ores::refdata::messaging::delete_credit_limit_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_credit_limits_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_credit_limits_request& v);
    static ores::refdata::messaging::delete_many_credit_limits_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_credit_limits_request& v);
    static ores::refdata::messaging::delete_many_credit_limits_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_credit_limits_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_credit_limits_response& v);
    static ores::refdata::messaging::delete_many_credit_limits_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_credit_limits_response& v);
    static ores::refdata::messaging::delete_many_credit_limits_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_counterparty_id_credit_limits_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_counterparty_id_credit_limits_request& v);
    static ores::refdata::messaging::list_by_counterparty_id_credit_limits_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_counterparty_id_credit_limits_request& v);
    static ores::refdata::messaging::list_by_counterparty_id_credit_limits_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_counterparty_id_credit_limits_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_counterparty_id_credit_limits_response& v);
    static ores::refdata::messaging::list_by_counterparty_id_credit_limits_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_counterparty_id_credit_limits_response& v);
    static ores::refdata::messaging::list_by_counterparty_id_credit_limits_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_credit_limit_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_credit_limit_versions_request& v);
    static ores::refdata::messaging::list_credit_limit_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_credit_limit_versions_request& v);
    static ores::refdata::messaging::list_credit_limit_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_credit_limit_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_credit_limit_versions_response& v);
    static ores::refdata::messaging::list_credit_limit_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_credit_limit_versions_response& v);
    static ores::refdata::messaging::list_credit_limit_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_credit_limit_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_credit_limit_version_request& v);
    static ores::refdata::messaging::get_credit_limit_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_credit_limit_version_request& v);
    static ores::refdata::messaging::get_credit_limit_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_credit_limit_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_credit_limit_version_response& v);
    static ores::refdata::messaging::get_credit_limit_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_credit_limit_version_response& v);
    static ores::refdata::messaging::get_credit_limit_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
