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
#ifndef ORES_SYNTHETIC_API_MESSAGING_CALIBRATION_PROVIDER_ORDER_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_CALIBRATION_PROVIDER_ORDER_PROTOCOL_HPP

#include "ores.synthetic.api/domain/calibration_provider_order.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct calibration_provider_order_key {
    boost::uuids::uuid id;

    friend bool operator==(const calibration_provider_order_key&,
                           const calibration_provider_order_key&) = default;
};

struct calibration_provider_order_write {
    boost::uuids::uuid id;
    boost::uuids::uuid policy_id;
    std::string provider_code;
    int rank;

    friend bool operator==(const calibration_provider_order_write&,
                           const calibration_provider_order_write&) = default;
};

struct calibration_provider_order_change {
    calibration_provider_order_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const calibration_provider_order_change&,
                           const calibration_provider_order_change&) = default;
};

struct calibration_provider_order_removal {
    calibration_provider_order_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const calibration_provider_order_removal&,
                           const calibration_provider_order_removal&) = default;
};

struct calibration_provider_order_lookup {
    calibration_provider_order_key key;
    std::optional<ores::synthetic::domain::calibration_provider_order> calibration_provider_order;

    friend bool operator==(const calibration_provider_order_lookup&,
                           const calibration_provider_order_lookup&) = default;
};

struct calibration_provider_orders_filter {
    std::optional<boost::uuids::uuid> policy_id;
    std::optional<std::string> provider_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> policy_id_one_of;
    std::optional<std::vector<std::string>> provider_code_one_of;

    friend bool operator==(const calibration_provider_orders_filter&,
                           const calibration_provider_orders_filter&) = default;
};

struct calibration_provider_order_event {
    boost::uuids::uuid event_id;
    calibration_provider_order_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const calibration_provider_order_event&,
                           const calibration_provider_order_event&) = default;
};

struct calibration_provider_order_version_key {
    calibration_provider_order_key calibration_provider_order;
    std::uint32_t version;

    friend bool operator==(const calibration_provider_order_version_key&,
                           const calibration_provider_order_version_key&) = default;
};

struct calibration_provider_order_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const calibration_provider_order_versions_filter&,
                           const calibration_provider_order_versions_filter&) = default;
};

struct list_calibration_provider_orders_request {
    using response_type = struct list_calibration_provider_orders_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_provider_orders.list";
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
    std::optional<calibration_provider_orders_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_calibration_provider_orders_request&,
                           const list_calibration_provider_orders_request&) = default;
};

struct list_calibration_provider_orders_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_provider_order> provider_orders;
    std::uint64_t total;

    friend bool operator==(const list_calibration_provider_orders_response&,
                           const list_calibration_provider_orders_response&) = default;
};

struct get_calibration_provider_order_request {
    using response_type = struct get_calibration_provider_order_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_provider_orders.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_provider_order_key key;

    friend bool operator==(const get_calibration_provider_order_request&,
                           const get_calibration_provider_order_request&) = default;
};

struct get_calibration_provider_order_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_provider_order> calibration_provider_order;

    friend bool operator==(const get_calibration_provider_order_response&,
                           const get_calibration_provider_order_response&) = default;
};

struct get_many_calibration_provider_orders_request {
    using response_type = struct get_many_calibration_provider_orders_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_provider_orders.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_provider_order_key> keys;

    friend bool operator==(const get_many_calibration_provider_orders_request&,
                           const get_many_calibration_provider_orders_request&) = default;
};

struct get_many_calibration_provider_orders_response {
    ores::utility::domain::result result;
    std::vector<calibration_provider_order_lookup> entries;

    friend bool operator==(const get_many_calibration_provider_orders_response&,
                           const get_many_calibration_provider_orders_response&) = default;
};

struct put_calibration_provider_order_request {
    using response_type = struct put_calibration_provider_order_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_provider_orders.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_provider_order_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_calibration_provider_order_request&,
                           const put_calibration_provider_order_request&) = default;
};

struct put_calibration_provider_order_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_provider_order> calibration_provider_order;

    friend bool operator==(const put_calibration_provider_order_response&,
                           const put_calibration_provider_order_response&) = default;
};

struct put_many_calibration_provider_orders_request {
    using response_type = struct put_many_calibration_provider_orders_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_provider_orders.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_provider_order_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_calibration_provider_orders_request&,
                           const put_many_calibration_provider_orders_request&) = default;
};

struct put_many_calibration_provider_orders_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_provider_order> provider_orders;

    friend bool operator==(const put_many_calibration_provider_orders_response&,
                           const put_many_calibration_provider_orders_response&) = default;
};

struct delete_calibration_provider_order_request {
    using response_type = struct delete_calibration_provider_order_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_provider_orders.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_provider_order_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_calibration_provider_order_request&,
                           const delete_calibration_provider_order_request&) = default;
};

struct delete_calibration_provider_order_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_calibration_provider_order_response&,
                           const delete_calibration_provider_order_response&) = default;
};

struct delete_many_calibration_provider_orders_request {
    using response_type = struct delete_many_calibration_provider_orders_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_provider_orders.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_provider_order_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_calibration_provider_orders_request&,
                           const delete_many_calibration_provider_orders_request&) = default;
};

struct delete_many_calibration_provider_orders_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_calibration_provider_orders_response&,
                           const delete_many_calibration_provider_orders_response&) = default;
};

struct list_calibration_provider_order_versions_request {
    using response_type = struct list_calibration_provider_order_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_provider_orders_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_provider_order_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calibration_provider_order_versions_filter> filter;

    friend bool operator==(const list_calibration_provider_order_versions_request&,
                           const list_calibration_provider_order_versions_request&) = default;
};

struct list_calibration_provider_order_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_provider_order> versions;
    std::uint64_t total;

    friend bool operator==(const list_calibration_provider_order_versions_response&,
                           const list_calibration_provider_order_versions_response&) = default;
};

struct get_calibration_provider_order_version_request {
    using response_type = struct get_calibration_provider_order_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_provider_orders_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_provider_order_version_key key;

    friend bool operator==(const get_calibration_provider_order_version_request&,
                           const get_calibration_provider_order_version_request&) = default;
};

struct get_calibration_provider_order_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_provider_order> version;

    friend bool operator==(const get_calibration_provider_order_version_response&,
                           const get_calibration_provider_order_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace calibration_provider_order_event_subjects {
inline constexpr std::string_view created =
    "synthetic.v1.events.calibration_provider_orders.created";
inline constexpr std::string_view updated =
    "synthetic.v1.events.calibration_provider_orders.updated";
inline constexpr std::string_view deleted =
    "synthetic.v1.events.calibration_provider_orders.deleted";
}
/**
 * @brief The members of calibration_provider_order_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_key&) {
    return std::tuple{
        utility::codec::field{"id", &calibration_provider_order_key::id},
    };
}

/**
 * @brief The members of calibration_provider_order_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_write&) {
    return std::tuple{
        utility::codec::field{"id", &calibration_provider_order_write::id},
        utility::codec::field{"policy_id", &calibration_provider_order_write::policy_id},
        utility::codec::field{"provider_code", &calibration_provider_order_write::provider_code},
        utility::codec::field{"rank", &calibration_provider_order_write::rank},
    };
}

/**
 * @brief The members of calibration_provider_order_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_change&) {
    return std::tuple{
        utility::codec::field{"write", &calibration_provider_order_change::write},
        utility::codec::field{"precondition", &calibration_provider_order_change::precondition},
    };
}

/**
 * @brief The members of calibration_provider_order_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_removal&) {
    return std::tuple{
        utility::codec::field{"key", &calibration_provider_order_removal::key},
        utility::codec::field{"precondition", &calibration_provider_order_removal::precondition},
    };
}

/**
 * @brief The members of calibration_provider_order_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &calibration_provider_order_lookup::key},
        utility::codec::field{"calibration_provider_order",
                              &calibration_provider_order_lookup::calibration_provider_order},
    };
}

/**
 * @brief The members of calibration_provider_orders_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_orders_filter&) {
    return std::tuple{
        utility::codec::field{"policy_id", &calibration_provider_orders_filter::policy_id},
        utility::codec::field{"provider_code", &calibration_provider_orders_filter::provider_code},
        utility::codec::field{"id_one_of", &calibration_provider_orders_filter::id_one_of},
        utility::codec::field{"policy_id_one_of",
                              &calibration_provider_orders_filter::policy_id_one_of},
        utility::codec::field{"provider_code_one_of",
                              &calibration_provider_orders_filter::provider_code_one_of},
    };
}

/**
 * @brief The members of calibration_provider_order_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &calibration_provider_order_event::event_id},
        utility::codec::field{"key", &calibration_provider_order_event::key},
        utility::codec::field{"action", &calibration_provider_order_event::action},
        utility::codec::field{"version", &calibration_provider_order_event::version},
        utility::codec::field{"occurred_at", &calibration_provider_order_event::occurred_at},
        utility::codec::field{"correlation_id", &calibration_provider_order_event::correlation_id},
    };
}

/**
 * @brief The members of calibration_provider_order_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_version_key&) {
    return std::tuple{
        utility::codec::field{"calibration_provider_order",
                              &calibration_provider_order_version_key::calibration_provider_order},
        utility::codec::field{"version", &calibration_provider_order_version_key::version},
    };
}

/**
 * @brief The members of calibration_provider_order_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const calibration_provider_order_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &calibration_provider_order_versions_filter::version},
        utility::codec::field{"from_version",
                              &calibration_provider_order_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &calibration_provider_order_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_calibration_provider_orders_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calibration_provider_orders_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_calibration_provider_orders_request::offset},
        utility::codec::field{"limit", &list_calibration_provider_orders_request::limit},
        utility::codec::field{"order", &list_calibration_provider_orders_request::order},
        utility::codec::field{"filter", &list_calibration_provider_orders_request::filter},
        utility::codec::field{"as_of", &list_calibration_provider_orders_request::as_of},
    };
}

/**
 * @brief The members of list_calibration_provider_orders_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calibration_provider_orders_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calibration_provider_orders_response::result},
        utility::codec::field{"provider_orders",
                              &list_calibration_provider_orders_response::provider_orders},
        utility::codec::field{"total", &list_calibration_provider_orders_response::total},
    };
}

/**
 * @brief The members of get_calibration_provider_order_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calibration_provider_order_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calibration_provider_order_request::key},
    };
}

/**
 * @brief The members of get_calibration_provider_order_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calibration_provider_order_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calibration_provider_order_response::result},
        utility::codec::field{"calibration_provider_order",
                              &get_calibration_provider_order_response::calibration_provider_order},
    };
}

/**
 * @brief The members of get_many_calibration_provider_orders_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calibration_provider_orders_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_calibration_provider_orders_request::keys},
    };
}

/**
 * @brief The members of get_many_calibration_provider_orders_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_calibration_provider_orders_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_calibration_provider_orders_response::result},
        utility::codec::field{"entries", &get_many_calibration_provider_orders_response::entries},
    };
}

/**
 * @brief The members of put_calibration_provider_order_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calibration_provider_order_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_calibration_provider_order_request::change},
        utility::codec::field{"intent", &put_calibration_provider_order_request::intent},
    };
}

/**
 * @brief The members of put_calibration_provider_order_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_calibration_provider_order_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_calibration_provider_order_response::result},
        utility::codec::field{"calibration_provider_order",
                              &put_calibration_provider_order_response::calibration_provider_order},
    };
}

/**
 * @brief The members of put_many_calibration_provider_orders_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calibration_provider_orders_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_calibration_provider_orders_request::changes},
        utility::codec::field{"intent", &put_many_calibration_provider_orders_request::intent},
    };
}

/**
 * @brief The members of put_many_calibration_provider_orders_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_calibration_provider_orders_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_calibration_provider_orders_response::result},
        utility::codec::field{"provider_orders",
                              &put_many_calibration_provider_orders_response::provider_orders},
    };
}

/**
 * @brief The members of delete_calibration_provider_order_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calibration_provider_order_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_calibration_provider_order_request::removal},
        utility::codec::field{"intent", &delete_calibration_provider_order_request::intent},
    };
}

/**
 * @brief The members of delete_calibration_provider_order_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_calibration_provider_order_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_calibration_provider_order_response::result},
    };
}

/**
 * @brief The members of delete_many_calibration_provider_orders_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calibration_provider_orders_request&) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_calibration_provider_orders_request::removals},
        utility::codec::field{"intent", &delete_many_calibration_provider_orders_request::intent},
    };
}

/**
 * @brief The members of delete_many_calibration_provider_orders_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_calibration_provider_orders_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_calibration_provider_orders_response::result},
    };
}

/**
 * @brief The members of list_calibration_provider_order_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calibration_provider_order_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_calibration_provider_order_versions_request::key},
        utility::codec::field{"offset", &list_calibration_provider_order_versions_request::offset},
        utility::codec::field{"limit", &list_calibration_provider_order_versions_request::limit},
        utility::codec::field{"order", &list_calibration_provider_order_versions_request::order},
        utility::codec::field{"filter", &list_calibration_provider_order_versions_request::filter},
    };
}

/**
 * @brief The members of list_calibration_provider_order_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_calibration_provider_order_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_calibration_provider_order_versions_response::result},
        utility::codec::field{"versions",
                              &list_calibration_provider_order_versions_response::versions},
        utility::codec::field{"total", &list_calibration_provider_order_versions_response::total},
    };
}

/**
 * @brief The members of get_calibration_provider_order_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calibration_provider_order_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_calibration_provider_order_version_request::key},
    };
}

/**
 * @brief The members of get_calibration_provider_order_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_calibration_provider_order_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_calibration_provider_order_version_response::result},
        utility::codec::field{"version", &get_calibration_provider_order_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_key& v);
    static ores::synthetic::messaging::calibration_provider_order_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_key& v);
    static ores::synthetic::messaging::calibration_provider_order_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_write& v);
    static ores::synthetic::messaging::calibration_provider_order_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_write& v);
    static ores::synthetic::messaging::calibration_provider_order_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_change& v);
    static ores::synthetic::messaging::calibration_provider_order_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_change& v);
    static ores::synthetic::messaging::calibration_provider_order_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_removal& v);
    static ores::synthetic::messaging::calibration_provider_order_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_removal& v);
    static ores::synthetic::messaging::calibration_provider_order_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_lookup& v);
    static ores::synthetic::messaging::calibration_provider_order_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_lookup& v);
    static ores::synthetic::messaging::calibration_provider_order_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_orders_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_orders_filter& v);
    static ores::synthetic::messaging::calibration_provider_orders_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_orders_filter& v);
    static ores::synthetic::messaging::calibration_provider_orders_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_event& v);
    static ores::synthetic::messaging::calibration_provider_order_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_event& v);
    static ores::synthetic::messaging::calibration_provider_order_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_version_key& v);
    static ores::synthetic::messaging::calibration_provider_order_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_version_key& v);
    static ores::synthetic::messaging::calibration_provider_order_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_provider_order_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_provider_order_versions_filter& v);
    static ores::synthetic::messaging::calibration_provider_order_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_provider_order_versions_filter& v);
    static ores::synthetic::messaging::calibration_provider_order_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_provider_orders_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::list_calibration_provider_orders_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::list_calibration_provider_orders_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_provider_orders_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::list_calibration_provider_orders_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::list_calibration_provider_orders_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_provider_order_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_calibration_provider_order_request& v);
    static ores::synthetic::messaging::get_calibration_provider_order_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_calibration_provider_order_request& v);
    static ores::synthetic::messaging::get_calibration_provider_order_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_provider_order_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_calibration_provider_order_response& v);
    static ores::synthetic::messaging::get_calibration_provider_order_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_calibration_provider_order_response& v);
    static ores::synthetic::messaging::get_calibration_provider_order_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_calibration_provider_orders_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::get_many_calibration_provider_orders_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_many_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::get_many_calibration_provider_orders_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_calibration_provider_orders_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::get_many_calibration_provider_orders_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_many_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::get_many_calibration_provider_orders_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_calibration_provider_order_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_calibration_provider_order_request& v);
    static ores::synthetic::messaging::put_calibration_provider_order_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_calibration_provider_order_request& v);
    static ores::synthetic::messaging::put_calibration_provider_order_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_calibration_provider_order_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_calibration_provider_order_response& v);
    static ores::synthetic::messaging::put_calibration_provider_order_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_calibration_provider_order_response& v);
    static ores::synthetic::messaging::put_calibration_provider_order_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_calibration_provider_orders_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::put_many_calibration_provider_orders_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_many_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::put_many_calibration_provider_orders_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_calibration_provider_orders_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::put_many_calibration_provider_orders_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::put_many_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::put_many_calibration_provider_orders_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_calibration_provider_order_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_calibration_provider_order_request& v);
    static ores::synthetic::messaging::delete_calibration_provider_order_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_calibration_provider_order_request& v);
    static ores::synthetic::messaging::delete_calibration_provider_order_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_calibration_provider_order_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_calibration_provider_order_response& v);
    static ores::synthetic::messaging::delete_calibration_provider_order_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_calibration_provider_order_response& v);
    static ores::synthetic::messaging::delete_calibration_provider_order_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_calibration_provider_orders_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_many_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::delete_many_calibration_provider_orders_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_calibration_provider_orders_request& v);
    static ores::synthetic::messaging::delete_many_calibration_provider_orders_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_calibration_provider_orders_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::delete_many_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::delete_many_calibration_provider_orders_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_calibration_provider_orders_response& v);
    static ores::synthetic::messaging::delete_many_calibration_provider_orders_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_provider_order_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_calibration_provider_order_versions_request& v);
    static ores::synthetic::messaging::list_calibration_provider_order_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_calibration_provider_order_versions_request& v);
    static ores::synthetic::messaging::list_calibration_provider_order_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_provider_order_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::list_calibration_provider_order_versions_response& v);
    static ores::synthetic::messaging::list_calibration_provider_order_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_calibration_provider_order_versions_response& v);
    static ores::synthetic::messaging::list_calibration_provider_order_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_provider_order_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_calibration_provider_order_version_request& v);
    static ores::synthetic::messaging::get_calibration_provider_order_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_calibration_provider_order_version_request& v);
    static ores::synthetic::messaging::get_calibration_provider_order_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_provider_order_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::synthetic::messaging::get_calibration_provider_order_version_response& v);
    static ores::synthetic::messaging::get_calibration_provider_order_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_calibration_provider_order_version_response& v);
    static ores::synthetic::messaging::get_calibration_provider_order_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
