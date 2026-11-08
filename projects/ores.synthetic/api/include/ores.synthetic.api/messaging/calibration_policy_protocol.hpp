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
#ifndef ORES_SYNTHETIC_API_MESSAGING_CALIBRATION_POLICY_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_CALIBRATION_POLICY_PROTOCOL_HPP

#include "ores.synthetic.api/domain/calibration_policy_codec.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct calibration_policy_key {
    boost::uuids::uuid id;

    friend bool operator==(const calibration_policy_key&, const calibration_policy_key&) = default;
};

struct calibration_policy_write {
    boost::uuids::uuid id;
    std::string asset_class_code;
    int window_days{};
    int variance_window_days{};
    std::string variance_approval_mode;
    int variance_min_returns{};
    bool variance_require_starts_agree{};
    double variance_min_persistence{};
    double variance_max_persistence{};
    double variance_min_reaction{};
    bool allow_inverse{};
    std::string fallback_code;

    friend bool operator==(const calibration_policy_write&,
                           const calibration_policy_write&) = default;
};

struct calibration_policy_change {
    calibration_policy_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const calibration_policy_change&,
                           const calibration_policy_change&) = default;
};

struct calibration_policy_removal {
    calibration_policy_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const calibration_policy_removal&,
                           const calibration_policy_removal&) = default;
};

struct calibration_policy_lookup {
    calibration_policy_key key;
    std::optional<ores::synthetic::domain::calibration_policy> calibration_policy;

    friend bool operator==(const calibration_policy_lookup&,
                           const calibration_policy_lookup&) = default;
};

struct calibration_policies_filter {
    std::optional<std::string> fallback_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> fallback_code_one_of;
    std::optional<std::string> search;

    friend bool operator==(const calibration_policies_filter&,
                           const calibration_policies_filter&) = default;
};

struct calibration_policy_event {
    boost::uuids::uuid event_id;
    calibration_policy_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const calibration_policy_event&,
                           const calibration_policy_event&) = default;
};

struct calibration_policy_version_key {
    calibration_policy_key calibration_policy;
    std::uint32_t version{};

    friend bool operator==(const calibration_policy_version_key&,
                           const calibration_policy_version_key&) = default;
};

struct calibration_policy_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const calibration_policy_versions_filter&,
                           const calibration_policy_versions_filter&) = default;
};

struct list_calibration_policies_request {
    using response_type = struct list_calibration_policies_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_policies.list";
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
    std::optional<calibration_policies_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_calibration_policies_request&,
                           const list_calibration_policies_request&) = default;
};

struct list_calibration_policies_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_policy> policies;
    std::uint64_t total{};

    friend bool operator==(const list_calibration_policies_response&,
                           const list_calibration_policies_response&) = default;
};

struct get_calibration_policy_request {
    using response_type = struct get_calibration_policy_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_policies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_policy_key key;

    friend bool operator==(const get_calibration_policy_request&,
                           const get_calibration_policy_request&) = default;
};

struct get_calibration_policy_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_policy> calibration_policy;

    friend bool operator==(const get_calibration_policy_response&,
                           const get_calibration_policy_response&) = default;
};

struct get_many_calibration_policies_request {
    using response_type = struct get_many_calibration_policies_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_policies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_policy_key> keys;

    friend bool operator==(const get_many_calibration_policies_request&,
                           const get_many_calibration_policies_request&) = default;
};

struct get_many_calibration_policies_response {
    ores::utility::domain::result result;
    std::vector<calibration_policy_lookup> entries;

    friend bool operator==(const get_many_calibration_policies_response&,
                           const get_many_calibration_policies_response&) = default;
};

struct put_calibration_policy_request {
    using response_type = struct put_calibration_policy_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_policies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_policy_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_calibration_policy_request&,
                           const put_calibration_policy_request&) = default;
};

struct put_calibration_policy_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_policy> calibration_policy;

    friend bool operator==(const put_calibration_policy_response&,
                           const put_calibration_policy_response&) = default;
};

struct put_many_calibration_policies_request {
    using response_type = struct put_many_calibration_policies_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_policies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_policy_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_calibration_policies_request&,
                           const put_many_calibration_policies_request&) = default;
};

struct put_many_calibration_policies_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_policy> policies;

    friend bool operator==(const put_many_calibration_policies_response&,
                           const put_many_calibration_policies_response&) = default;
};

struct delete_calibration_policy_request {
    using response_type = struct delete_calibration_policy_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.calibration_policies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_policy_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_calibration_policy_request&,
                           const delete_calibration_policy_request&) = default;
};

struct delete_calibration_policy_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_calibration_policy_response&,
                           const delete_calibration_policy_response&) = default;
};

struct delete_many_calibration_policies_request {
    using response_type = struct delete_many_calibration_policies_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_policies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calibration_policy_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_calibration_policies_request&,
                           const delete_many_calibration_policies_request&) = default;
};

struct delete_many_calibration_policies_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_calibration_policies_response&,
                           const delete_many_calibration_policies_response&) = default;
};

struct list_calibration_policy_versions_request {
    using response_type = struct list_calibration_policy_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_policies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_policy_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calibration_policy_versions_filter> filter;

    friend bool operator==(const list_calibration_policy_versions_request&,
                           const list_calibration_policy_versions_request&) = default;
};

struct list_calibration_policy_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::calibration_policy> versions;
    std::uint64_t total{};

    friend bool operator==(const list_calibration_policy_versions_response&,
                           const list_calibration_policy_versions_response&) = default;
};

struct get_calibration_policy_version_request {
    using response_type = struct get_calibration_policy_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.calibration_policies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calibration_policy_version_key key;

    friend bool operator==(const get_calibration_policy_version_request&,
                           const get_calibration_policy_version_request&) = default;
};

struct get_calibration_policy_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::calibration_policy> version;

    friend bool operator==(const get_calibration_policy_version_response&,
                           const get_calibration_policy_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace calibration_policy_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.events.calibration_policies.created";
inline constexpr std::string_view updated = "synthetic.v1.events.calibration_policies.updated";
inline constexpr std::string_view deleted = "synthetic.v1.events.calibration_policies.deleted";
}
/**
 * @brief The members of calibration_policy_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_key>) {
    return std::tuple{
        utility::codec::field{"id", &calibration_policy_key::id},
    };
}

/**
 * @brief The members of calibration_policy_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_write>) {
    return std::tuple{
        utility::codec::field{"id", &calibration_policy_write::id},
        utility::codec::field{"asset_class_code", &calibration_policy_write::asset_class_code},
        utility::codec::field{"window_days", &calibration_policy_write::window_days},
        utility::codec::field{"variance_window_days",
                              &calibration_policy_write::variance_window_days},
        utility::codec::field{"variance_approval_mode",
                              &calibration_policy_write::variance_approval_mode},
        utility::codec::field{"variance_min_returns",
                              &calibration_policy_write::variance_min_returns},
        utility::codec::field{"variance_require_starts_agree",
                              &calibration_policy_write::variance_require_starts_agree},
        utility::codec::field{"variance_min_persistence",
                              &calibration_policy_write::variance_min_persistence},
        utility::codec::field{"variance_max_persistence",
                              &calibration_policy_write::variance_max_persistence},
        utility::codec::field{"variance_min_reaction",
                              &calibration_policy_write::variance_min_reaction},
        utility::codec::field{"allow_inverse", &calibration_policy_write::allow_inverse},
        utility::codec::field{"fallback_code", &calibration_policy_write::fallback_code},
    };
}

/**
 * @brief The members of calibration_policy_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_change>) {
    return std::tuple{
        utility::codec::field{"write", &calibration_policy_change::write},
        utility::codec::field{"precondition", &calibration_policy_change::precondition},
    };
}

/**
 * @brief The members of calibration_policy_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_removal>) {
    return std::tuple{
        utility::codec::field{"key", &calibration_policy_removal::key},
        utility::codec::field{"precondition", &calibration_policy_removal::precondition},
    };
}

/**
 * @brief The members of calibration_policy_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &calibration_policy_lookup::key},
        utility::codec::field{"calibration_policy", &calibration_policy_lookup::calibration_policy},
    };
}

/**
 * @brief The members of calibration_policies_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policies_filter>) {
    return std::tuple{
        utility::codec::field{"fallback_code", &calibration_policies_filter::fallback_code},
        utility::codec::field{"id_one_of", &calibration_policies_filter::id_one_of},
        utility::codec::field{"fallback_code_one_of",
                              &calibration_policies_filter::fallback_code_one_of},
        utility::codec::field{"search", &calibration_policies_filter::search},
    };
}

/**
 * @brief The members of calibration_policy_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &calibration_policy_event::event_id},
        utility::codec::field{"key", &calibration_policy_event::key},
        utility::codec::field{"action", &calibration_policy_event::action},
        utility::codec::field{"version", &calibration_policy_event::version},
        utility::codec::field{"occurred_at", &calibration_policy_event::occurred_at},
        utility::codec::field{"correlation_id", &calibration_policy_event::correlation_id},
    };
}

/**
 * @brief The members of calibration_policy_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_version_key>) {
    return std::tuple{
        utility::codec::field{"calibration_policy",
                              &calibration_policy_version_key::calibration_policy},
        utility::codec::field{"version", &calibration_policy_version_key::version},
    };
}

/**
 * @brief The members of calibration_policy_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<calibration_policy_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &calibration_policy_versions_filter::version},
        utility::codec::field{"from_version", &calibration_policy_versions_filter::from_version},
        utility::codec::field{"to_version", &calibration_policy_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_calibration_policies_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_calibration_policies_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_calibration_policies_request::offset},
        utility::codec::field{"limit", &list_calibration_policies_request::limit},
        utility::codec::field{"order", &list_calibration_policies_request::order},
        utility::codec::field{"filter", &list_calibration_policies_request::filter},
        utility::codec::field{"as_of", &list_calibration_policies_request::as_of},
    };
}

/**
 * @brief The members of list_calibration_policies_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_calibration_policies_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_calibration_policies_response::result},
        utility::codec::field{"policies", &list_calibration_policies_response::policies},
        utility::codec::field{"total", &list_calibration_policies_response::total},
    };
}

/**
 * @brief The members of get_calibration_policy_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_calibration_policy_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_calibration_policy_request::key},
    };
}

/**
 * @brief The members of get_calibration_policy_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_calibration_policy_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_calibration_policy_response::result},
        utility::codec::field{"calibration_policy",
                              &get_calibration_policy_response::calibration_policy},
    };
}

/**
 * @brief The members of get_many_calibration_policies_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_calibration_policies_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_calibration_policies_request::keys},
    };
}

/**
 * @brief The members of get_many_calibration_policies_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_calibration_policies_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_calibration_policies_response::result},
        utility::codec::field{"entries", &get_many_calibration_policies_response::entries},
    };
}

/**
 * @brief The members of put_calibration_policy_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_calibration_policy_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_calibration_policy_request::change},
        utility::codec::field{"intent", &put_calibration_policy_request::intent},
    };
}

/**
 * @brief The members of put_calibration_policy_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_calibration_policy_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_calibration_policy_response::result},
        utility::codec::field{"calibration_policy",
                              &put_calibration_policy_response::calibration_policy},
    };
}

/**
 * @brief The members of put_many_calibration_policies_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_calibration_policies_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_calibration_policies_request::changes},
        utility::codec::field{"intent", &put_many_calibration_policies_request::intent},
    };
}

/**
 * @brief The members of put_many_calibration_policies_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_calibration_policies_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_calibration_policies_response::result},
        utility::codec::field{"policies", &put_many_calibration_policies_response::policies},
    };
}

/**
 * @brief The members of delete_calibration_policy_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_calibration_policy_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_calibration_policy_request::removal},
        utility::codec::field{"intent", &delete_calibration_policy_request::intent},
    };
}

/**
 * @brief The members of delete_calibration_policy_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_calibration_policy_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_calibration_policy_response::result},
    };
}

/**
 * @brief The members of delete_many_calibration_policies_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_calibration_policies_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_calibration_policies_request::removals},
        utility::codec::field{"intent", &delete_many_calibration_policies_request::intent},
    };
}

/**
 * @brief The members of delete_many_calibration_policies_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_calibration_policies_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_calibration_policies_response::result},
    };
}

/**
 * @brief The members of list_calibration_policy_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_calibration_policy_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_calibration_policy_versions_request::key},
        utility::codec::field{"offset", &list_calibration_policy_versions_request::offset},
        utility::codec::field{"limit", &list_calibration_policy_versions_request::limit},
        utility::codec::field{"order", &list_calibration_policy_versions_request::order},
        utility::codec::field{"filter", &list_calibration_policy_versions_request::filter},
    };
}

/**
 * @brief The members of list_calibration_policy_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_calibration_policy_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_calibration_policy_versions_response::result},
        utility::codec::field{"versions", &list_calibration_policy_versions_response::versions},
        utility::codec::field{"total", &list_calibration_policy_versions_response::total},
    };
}

/**
 * @brief The members of get_calibration_policy_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_calibration_policy_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_calibration_policy_version_request::key},
    };
}

/**
 * @brief The members of get_calibration_policy_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_calibration_policy_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_calibration_policy_version_response::result},
        utility::codec::field{"version", &get_calibration_policy_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::calibration_policy_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policy_key& v);
    static ores::synthetic::messaging::calibration_policy_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::calibration_policy_key& v);
    static ores::synthetic::messaging::calibration_policy_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::calibration_policy_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policy_write& v);
    static ores::synthetic::messaging::calibration_policy_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::calibration_policy_write& v);
    static ores::synthetic::messaging::calibration_policy_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_policy_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policy_change& v);
    static ores::synthetic::messaging::calibration_policy_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_policy_change& v);
    static ores::synthetic::messaging::calibration_policy_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_policy_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policy_removal& v);
    static ores::synthetic::messaging::calibration_policy_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_policy_removal& v);
    static ores::synthetic::messaging::calibration_policy_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_policy_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policy_lookup& v);
    static ores::synthetic::messaging::calibration_policy_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_policy_lookup& v);
    static ores::synthetic::messaging::calibration_policy_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_policies_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policies_filter& v);
    static ores::synthetic::messaging::calibration_policies_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_policies_filter& v);
    static ores::synthetic::messaging::calibration_policies_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT wire_traits<ores::synthetic::messaging::calibration_policy_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::calibration_policy_event& v);
    static ores::synthetic::messaging::calibration_policy_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::synthetic::messaging::calibration_policy_event& v);
    static ores::synthetic::messaging::calibration_policy_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_policy_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_policy_version_key& v);
    static ores::synthetic::messaging::calibration_policy_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_policy_version_key& v);
    static ores::synthetic::messaging::calibration_policy_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::calibration_policy_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::calibration_policy_versions_filter& v);
    static ores::synthetic::messaging::calibration_policy_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::calibration_policy_versions_filter& v);
    static ores::synthetic::messaging::calibration_policy_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_policies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_calibration_policies_request& v);
    static ores::synthetic::messaging::list_calibration_policies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_calibration_policies_request& v);
    static ores::synthetic::messaging::list_calibration_policies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_policies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_calibration_policies_response& v);
    static ores::synthetic::messaging::list_calibration_policies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_calibration_policies_response& v);
    static ores::synthetic::messaging::list_calibration_policies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_policy_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_calibration_policy_request& v);
    static ores::synthetic::messaging::get_calibration_policy_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_calibration_policy_request& v);
    static ores::synthetic::messaging::get_calibration_policy_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_policy_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_calibration_policy_response& v);
    static ores::synthetic::messaging::get_calibration_policy_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_calibration_policy_response& v);
    static ores::synthetic::messaging::get_calibration_policy_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_calibration_policies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_calibration_policies_request& v);
    static ores::synthetic::messaging::get_many_calibration_policies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_calibration_policies_request& v);
    static ores::synthetic::messaging::get_many_calibration_policies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_calibration_policies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_calibration_policies_response& v);
    static ores::synthetic::messaging::get_many_calibration_policies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_calibration_policies_response& v);
    static ores::synthetic::messaging::get_many_calibration_policies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_calibration_policy_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_calibration_policy_request& v);
    static ores::synthetic::messaging::put_calibration_policy_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_calibration_policy_request& v);
    static ores::synthetic::messaging::put_calibration_policy_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_calibration_policy_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_calibration_policy_response& v);
    static ores::synthetic::messaging::put_calibration_policy_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_calibration_policy_response& v);
    static ores::synthetic::messaging::put_calibration_policy_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_calibration_policies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_calibration_policies_request& v);
    static ores::synthetic::messaging::put_many_calibration_policies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_calibration_policies_request& v);
    static ores::synthetic::messaging::put_many_calibration_policies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_calibration_policies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_calibration_policies_response& v);
    static ores::synthetic::messaging::put_many_calibration_policies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_calibration_policies_response& v);
    static ores::synthetic::messaging::put_many_calibration_policies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_calibration_policy_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_calibration_policy_request& v);
    static ores::synthetic::messaging::delete_calibration_policy_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_calibration_policy_request& v);
    static ores::synthetic::messaging::delete_calibration_policy_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_calibration_policy_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_calibration_policy_response& v);
    static ores::synthetic::messaging::delete_calibration_policy_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_calibration_policy_response& v);
    static ores::synthetic::messaging::delete_calibration_policy_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_calibration_policies_request& v);
    static ores::synthetic::messaging::delete_many_calibration_policies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_calibration_policies_request& v);
    static ores::synthetic::messaging::delete_many_calibration_policies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_calibration_policies_response& v);
    static ores::synthetic::messaging::delete_many_calibration_policies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_many_calibration_policies_response& v);
    static ores::synthetic::messaging::delete_many_calibration_policies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_calibration_policy_versions_request& v);
    static ores::synthetic::messaging::list_calibration_policy_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_calibration_policy_versions_request& v);
    static ores::synthetic::messaging::list_calibration_policy_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_calibration_policy_versions_response& v);
    static ores::synthetic::messaging::list_calibration_policy_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_calibration_policy_versions_response& v);
    static ores::synthetic::messaging::list_calibration_policy_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_policy_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_calibration_policy_version_request& v);
    static ores::synthetic::messaging::get_calibration_policy_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_calibration_policy_version_request& v);
    static ores::synthetic::messaging::get_calibration_policy_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_calibration_policy_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_calibration_policy_version_response& v);
    static ores::synthetic::messaging::get_calibration_policy_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_calibration_policy_version_response& v);
    static ores::synthetic::messaging::get_calibration_policy_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
