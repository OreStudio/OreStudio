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
#ifndef ORES_IAM_API_MESSAGING_SEED_PROFILE_PARAMETER_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SEED_PROFILE_PARAMETER_PROTOCOL_HPP

#include "ores.iam.api/domain/seed_profile_parameter.hpp"
#include "ores.iam.api/export.hpp"
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

namespace ores::iam::messaging {

struct seed_profile_parameter_key {
    boost::uuids::uuid id;

    friend bool operator==(const seed_profile_parameter_key&,
                           const seed_profile_parameter_key&) = default;
};

struct seed_profile_parameter_write {
    boost::uuids::uuid id;
    boost::uuids::uuid seed_profile_id;
    std::string name;
    std::string label;
    std::string data_type;
    std::string choices_json;
    std::string default_value;
    bool is_required;
    std::string description;
    int display_order;

    friend bool operator==(const seed_profile_parameter_write&,
                           const seed_profile_parameter_write&) = default;
};

struct seed_profile_parameter_change {
    seed_profile_parameter_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const seed_profile_parameter_change&,
                           const seed_profile_parameter_change&) = default;
};

struct seed_profile_parameter_removal {
    seed_profile_parameter_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const seed_profile_parameter_removal&,
                           const seed_profile_parameter_removal&) = default;
};

struct seed_profile_parameter_lookup {
    seed_profile_parameter_key key;
    std::optional<ores::iam::domain::seed_profile_parameter> seed_profile_parameter;

    friend bool operator==(const seed_profile_parameter_lookup&,
                           const seed_profile_parameter_lookup&) = default;
};

struct seed_profile_parameters_filter {
    std::optional<boost::uuids::uuid> seed_profile_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> seed_profile_id_one_of;

    friend bool operator==(const seed_profile_parameters_filter&,
                           const seed_profile_parameters_filter&) = default;
};

struct seed_profile_parameter_event {
    boost::uuids::uuid event_id;
    seed_profile_parameter_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const seed_profile_parameter_event&,
                           const seed_profile_parameter_event&) = default;
};

struct seed_profile_parameter_version_key {
    seed_profile_parameter_key seed_profile_parameter;
    std::uint32_t version;

    friend bool operator==(const seed_profile_parameter_version_key&,
                           const seed_profile_parameter_version_key&) = default;
};

struct seed_profile_parameter_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const seed_profile_parameter_versions_filter&,
                           const seed_profile_parameter_versions_filter&) = default;
};

struct list_seed_profile_parameters_request {
    using response_type = struct list_seed_profile_parameters_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters.list";
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
    std::optional<seed_profile_parameters_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_seed_profile_parameters_request&,
                           const list_seed_profile_parameters_request&) = default;
};

struct list_seed_profile_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::seed_profile_parameter> seed_profile_parameters;
    std::uint64_t total;

    friend bool operator==(const list_seed_profile_parameters_response&,
                           const list_seed_profile_parameters_response&) = default;
};

struct get_seed_profile_parameter_request {
    using response_type = struct get_seed_profile_parameter_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_parameter_key key;

    friend bool operator==(const get_seed_profile_parameter_request&,
                           const get_seed_profile_parameter_request&) = default;
};

struct get_seed_profile_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::seed_profile_parameter> seed_profile_parameter;

    friend bool operator==(const get_seed_profile_parameter_response&,
                           const get_seed_profile_parameter_response&) = default;
};

struct get_many_seed_profile_parameters_request {
    using response_type = struct get_many_seed_profile_parameters_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<seed_profile_parameter_key> keys;

    friend bool operator==(const get_many_seed_profile_parameters_request&,
                           const get_many_seed_profile_parameters_request&) = default;
};

struct get_many_seed_profile_parameters_response {
    ores::utility::domain::result result;
    std::vector<seed_profile_parameter_lookup> entries;

    friend bool operator==(const get_many_seed_profile_parameters_response&,
                           const get_many_seed_profile_parameters_response&) = default;
};

struct put_seed_profile_parameter_request {
    using response_type = struct put_seed_profile_parameter_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_parameter_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_seed_profile_parameter_request&,
                           const put_seed_profile_parameter_request&) = default;
};

struct put_seed_profile_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::seed_profile_parameter> seed_profile_parameter;

    friend bool operator==(const put_seed_profile_parameter_response&,
                           const put_seed_profile_parameter_response&) = default;
};

struct put_many_seed_profile_parameters_request {
    using response_type = struct put_many_seed_profile_parameters_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<seed_profile_parameter_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_seed_profile_parameters_request&,
                           const put_many_seed_profile_parameters_request&) = default;
};

struct put_many_seed_profile_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::seed_profile_parameter> seed_profile_parameters;

    friend bool operator==(const put_many_seed_profile_parameters_response&,
                           const put_many_seed_profile_parameters_response&) = default;
};

struct delete_seed_profile_parameter_request {
    using response_type = struct delete_seed_profile_parameter_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_parameter_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_seed_profile_parameter_request&,
                           const delete_seed_profile_parameter_request&) = default;
};

struct delete_seed_profile_parameter_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_seed_profile_parameter_response&,
                           const delete_seed_profile_parameter_response&) = default;
};

struct delete_many_seed_profile_parameters_request {
    using response_type = struct delete_many_seed_profile_parameters_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<seed_profile_parameter_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_seed_profile_parameters_request&,
                           const delete_many_seed_profile_parameters_request&) = default;
};

struct delete_many_seed_profile_parameters_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_seed_profile_parameters_response&,
                           const delete_many_seed_profile_parameters_response&) = default;
};

struct list_by_seed_profile_id_seed_profile_parameters_request {
    using response_type = struct list_by_seed_profile_id_seed_profile_parameters_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.seed_profile_parameters.list_by_seed_profile_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid seed_profile_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<seed_profile_parameters_filter> filter;

    friend bool
    operator==(const list_by_seed_profile_id_seed_profile_parameters_request&,
               const list_by_seed_profile_id_seed_profile_parameters_request&) = default;
};

struct list_by_seed_profile_id_seed_profile_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::seed_profile_parameter> seed_profile_parameters;
    std::uint64_t total;

    friend bool
    operator==(const list_by_seed_profile_id_seed_profile_parameters_response&,
               const list_by_seed_profile_id_seed_profile_parameters_response&) = default;
};

struct list_seed_profile_parameter_versions_request {
    using response_type = struct list_seed_profile_parameter_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_parameter_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<seed_profile_parameter_versions_filter> filter;

    friend bool operator==(const list_seed_profile_parameter_versions_request&,
                           const list_seed_profile_parameter_versions_request&) = default;
};

struct list_seed_profile_parameter_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::seed_profile_parameter> versions;
    std::uint64_t total;

    friend bool operator==(const list_seed_profile_parameter_versions_response&,
                           const list_seed_profile_parameter_versions_response&) = default;
};

struct get_seed_profile_parameter_version_request {
    using response_type = struct get_seed_profile_parameter_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profile_parameters_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_parameter_version_key key;

    friend bool operator==(const get_seed_profile_parameter_version_request&,
                           const get_seed_profile_parameter_version_request&) = default;
};

struct get_seed_profile_parameter_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::seed_profile_parameter> version;

    friend bool operator==(const get_seed_profile_parameter_version_response&,
                           const get_seed_profile_parameter_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace seed_profile_parameter_event_subjects {
inline constexpr std::string_view created = "iam.v1.events.seed_profile_parameters.created";
inline constexpr std::string_view updated = "iam.v1.events.seed_profile_parameters.updated";
inline constexpr std::string_view deleted = "iam.v1.events.seed_profile_parameters.deleted";
}
/**
 * @brief The members of seed_profile_parameter_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_key&) {
    return std::tuple{
        utility::codec::field{"id", &seed_profile_parameter_key::id},
    };
}

/**
 * @brief The members of seed_profile_parameter_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_write&) {
    return std::tuple{
        utility::codec::field{"id", &seed_profile_parameter_write::id},
        utility::codec::field{"seed_profile_id", &seed_profile_parameter_write::seed_profile_id},
        utility::codec::field{"name", &seed_profile_parameter_write::name},
        utility::codec::field{"label", &seed_profile_parameter_write::label},
        utility::codec::field{"data_type", &seed_profile_parameter_write::data_type},
        utility::codec::field{"choices_json", &seed_profile_parameter_write::choices_json},
        utility::codec::field{"default_value", &seed_profile_parameter_write::default_value},
        utility::codec::field{"is_required", &seed_profile_parameter_write::is_required},
        utility::codec::field{"description", &seed_profile_parameter_write::description},
        utility::codec::field{"display_order", &seed_profile_parameter_write::display_order},
    };
}

/**
 * @brief The members of seed_profile_parameter_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_change&) {
    return std::tuple{
        utility::codec::field{"write", &seed_profile_parameter_change::write},
        utility::codec::field{"precondition", &seed_profile_parameter_change::precondition},
    };
}

/**
 * @brief The members of seed_profile_parameter_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_removal&) {
    return std::tuple{
        utility::codec::field{"key", &seed_profile_parameter_removal::key},
        utility::codec::field{"precondition", &seed_profile_parameter_removal::precondition},
    };
}

/**
 * @brief The members of seed_profile_parameter_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &seed_profile_parameter_lookup::key},
        utility::codec::field{"seed_profile_parameter",
                              &seed_profile_parameter_lookup::seed_profile_parameter},
    };
}

/**
 * @brief The members of seed_profile_parameters_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameters_filter&) {
    return std::tuple{
        utility::codec::field{"seed_profile_id", &seed_profile_parameters_filter::seed_profile_id},
        utility::codec::field{"id_one_of", &seed_profile_parameters_filter::id_one_of},
        utility::codec::field{"seed_profile_id_one_of",
                              &seed_profile_parameters_filter::seed_profile_id_one_of},
    };
}

/**
 * @brief The members of seed_profile_parameter_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &seed_profile_parameter_event::event_id},
        utility::codec::field{"key", &seed_profile_parameter_event::key},
        utility::codec::field{"action", &seed_profile_parameter_event::action},
        utility::codec::field{"version", &seed_profile_parameter_event::version},
        utility::codec::field{"occurred_at", &seed_profile_parameter_event::occurred_at},
        utility::codec::field{"correlation_id", &seed_profile_parameter_event::correlation_id},
    };
}

/**
 * @brief The members of seed_profile_parameter_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_version_key&) {
    return std::tuple{
        utility::codec::field{"seed_profile_parameter",
                              &seed_profile_parameter_version_key::seed_profile_parameter},
        utility::codec::field{"version", &seed_profile_parameter_version_key::version},
    };
}

/**
 * @brief The members of seed_profile_parameter_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const seed_profile_parameter_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &seed_profile_parameter_versions_filter::version},
        utility::codec::field{"from_version",
                              &seed_profile_parameter_versions_filter::from_version},
        utility::codec::field{"to_version", &seed_profile_parameter_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_seed_profile_parameters_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_seed_profile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_seed_profile_parameters_request::offset},
        utility::codec::field{"limit", &list_seed_profile_parameters_request::limit},
        utility::codec::field{"order", &list_seed_profile_parameters_request::order},
        utility::codec::field{"filter", &list_seed_profile_parameters_request::filter},
        utility::codec::field{"as_of", &list_seed_profile_parameters_request::as_of},
    };
}

/**
 * @brief The members of list_seed_profile_parameters_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_seed_profile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_seed_profile_parameters_response::result},
        utility::codec::field{"seed_profile_parameters",
                              &list_seed_profile_parameters_response::seed_profile_parameters},
        utility::codec::field{"total", &list_seed_profile_parameters_response::total},
    };
}

/**
 * @brief The members of get_seed_profile_parameter_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_seed_profile_parameter_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_seed_profile_parameter_request::key},
    };
}

/**
 * @brief The members of get_seed_profile_parameter_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_seed_profile_parameter_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_seed_profile_parameter_response::result},
        utility::codec::field{"seed_profile_parameter",
                              &get_seed_profile_parameter_response::seed_profile_parameter},
    };
}

/**
 * @brief The members of get_many_seed_profile_parameters_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_seed_profile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_seed_profile_parameters_request::keys},
    };
}

/**
 * @brief The members of get_many_seed_profile_parameters_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_seed_profile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_seed_profile_parameters_response::result},
        utility::codec::field{"entries", &get_many_seed_profile_parameters_response::entries},
    };
}

/**
 * @brief The members of put_seed_profile_parameter_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_seed_profile_parameter_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_seed_profile_parameter_request::change},
        utility::codec::field{"intent", &put_seed_profile_parameter_request::intent},
    };
}

/**
 * @brief The members of put_seed_profile_parameter_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_seed_profile_parameter_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_seed_profile_parameter_response::result},
        utility::codec::field{"seed_profile_parameter",
                              &put_seed_profile_parameter_response::seed_profile_parameter},
    };
}

/**
 * @brief The members of put_many_seed_profile_parameters_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_seed_profile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_seed_profile_parameters_request::changes},
        utility::codec::field{"intent", &put_many_seed_profile_parameters_request::intent},
    };
}

/**
 * @brief The members of put_many_seed_profile_parameters_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_seed_profile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_seed_profile_parameters_response::result},
        utility::codec::field{"seed_profile_parameters",
                              &put_many_seed_profile_parameters_response::seed_profile_parameters},
    };
}

/**
 * @brief The members of delete_seed_profile_parameter_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_seed_profile_parameter_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_seed_profile_parameter_request::removal},
        utility::codec::field{"intent", &delete_seed_profile_parameter_request::intent},
    };
}

/**
 * @brief The members of delete_seed_profile_parameter_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_seed_profile_parameter_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_seed_profile_parameter_response::result},
    };
}

/**
 * @brief The members of delete_many_seed_profile_parameters_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_seed_profile_parameters_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_seed_profile_parameters_request::removals},
        utility::codec::field{"intent", &delete_many_seed_profile_parameters_request::intent},
    };
}

/**
 * @brief The members of delete_many_seed_profile_parameters_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_seed_profile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_seed_profile_parameters_response::result},
    };
}

/**
 * @brief The members of list_by_seed_profile_id_seed_profile_parameters_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_seed_profile_id_seed_profile_parameters_request&) {
    return std::tuple{
        utility::codec::field{
            "seed_profile_id",
            &list_by_seed_profile_id_seed_profile_parameters_request::seed_profile_id},
        utility::codec::field{"scope",
                              &list_by_seed_profile_id_seed_profile_parameters_request::scope},
        utility::codec::field{"offset",
                              &list_by_seed_profile_id_seed_profile_parameters_request::offset},
        utility::codec::field{"limit",
                              &list_by_seed_profile_id_seed_profile_parameters_request::limit},
        utility::codec::field{"order",
                              &list_by_seed_profile_id_seed_profile_parameters_request::order},
        utility::codec::field{"filter",
                              &list_by_seed_profile_id_seed_profile_parameters_request::filter},
    };
}

/**
 * @brief The members of list_by_seed_profile_id_seed_profile_parameters_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_seed_profile_id_seed_profile_parameters_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_seed_profile_id_seed_profile_parameters_response::result},
        utility::codec::field{
            "seed_profile_parameters",
            &list_by_seed_profile_id_seed_profile_parameters_response::seed_profile_parameters},
        utility::codec::field{"total",
                              &list_by_seed_profile_id_seed_profile_parameters_response::total},
    };
}

/**
 * @brief The members of list_seed_profile_parameter_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_seed_profile_parameter_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_seed_profile_parameter_versions_request::key},
        utility::codec::field{"offset", &list_seed_profile_parameter_versions_request::offset},
        utility::codec::field{"limit", &list_seed_profile_parameter_versions_request::limit},
        utility::codec::field{"order", &list_seed_profile_parameter_versions_request::order},
        utility::codec::field{"filter", &list_seed_profile_parameter_versions_request::filter},
    };
}

/**
 * @brief The members of list_seed_profile_parameter_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_seed_profile_parameter_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_seed_profile_parameter_versions_response::result},
        utility::codec::field{"versions", &list_seed_profile_parameter_versions_response::versions},
        utility::codec::field{"total", &list_seed_profile_parameter_versions_response::total},
    };
}

/**
 * @brief The members of get_seed_profile_parameter_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_seed_profile_parameter_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_seed_profile_parameter_version_request::key},
    };
}

/**
 * @brief The members of get_seed_profile_parameter_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_seed_profile_parameter_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_seed_profile_parameter_version_response::result},
        utility::codec::field{"version", &get_seed_profile_parameter_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameter_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::seed_profile_parameter_key& v);
    static ores::iam::messaging::seed_profile_parameter_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::seed_profile_parameter_key& v);
    static ores::iam::messaging::seed_profile_parameter_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameter_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::seed_profile_parameter_write& v);
    static ores::iam::messaging::seed_profile_parameter_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::seed_profile_parameter_write& v);
    static ores::iam::messaging::seed_profile_parameter_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameter_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::seed_profile_parameter_change& v);
    static ores::iam::messaging::seed_profile_parameter_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::seed_profile_parameter_change& v);
    static ores::iam::messaging::seed_profile_parameter_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameter_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::seed_profile_parameter_removal& v);
    static ores::iam::messaging::seed_profile_parameter_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::seed_profile_parameter_removal& v);
    static ores::iam::messaging::seed_profile_parameter_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameter_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::seed_profile_parameter_lookup& v);
    static ores::iam::messaging::seed_profile_parameter_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::seed_profile_parameter_lookup& v);
    static ores::iam::messaging::seed_profile_parameter_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameters_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::seed_profile_parameters_filter& v);
    static ores::iam::messaging::seed_profile_parameters_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::seed_profile_parameters_filter& v);
    static ores::iam::messaging::seed_profile_parameters_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameter_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::seed_profile_parameter_event& v);
    static ores::iam::messaging::seed_profile_parameter_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::seed_profile_parameter_event& v);
    static ores::iam::messaging::seed_profile_parameter_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::seed_profile_parameter_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::seed_profile_parameter_version_key& v);
    static ores::iam::messaging::seed_profile_parameter_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::seed_profile_parameter_version_key& v);
    static ores::iam::messaging::seed_profile_parameter_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::seed_profile_parameter_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::seed_profile_parameter_versions_filter& v);
    static ores::iam::messaging::seed_profile_parameter_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::seed_profile_parameter_versions_filter& v);
    static ores::iam::messaging::seed_profile_parameter_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_seed_profile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_seed_profile_parameters_request& v);
    static ores::iam::messaging::list_seed_profile_parameters_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_seed_profile_parameters_request& v);
    static ores::iam::messaging::list_seed_profile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_seed_profile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_seed_profile_parameters_response& v);
    static ores::iam::messaging::list_seed_profile_parameters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_seed_profile_parameters_response& v);
    static ores::iam::messaging::list_seed_profile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_seed_profile_parameter_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_seed_profile_parameter_request& v);
    static ores::iam::messaging::get_seed_profile_parameter_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_seed_profile_parameter_request& v);
    static ores::iam::messaging::get_seed_profile_parameter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_seed_profile_parameter_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_seed_profile_parameter_response& v);
    static ores::iam::messaging::get_seed_profile_parameter_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_seed_profile_parameter_response& v);
    static ores::iam::messaging::get_seed_profile_parameter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_many_seed_profile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_seed_profile_parameters_request& v);
    static ores::iam::messaging::get_many_seed_profile_parameters_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_seed_profile_parameters_request& v);
    static ores::iam::messaging::get_many_seed_profile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_many_seed_profile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_many_seed_profile_parameters_response& v);
    static ores::iam::messaging::get_many_seed_profile_parameters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_many_seed_profile_parameters_response& v);
    static ores::iam::messaging::get_many_seed_profile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_seed_profile_parameter_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_seed_profile_parameter_request& v);
    static ores::iam::messaging::put_seed_profile_parameter_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_seed_profile_parameter_request& v);
    static ores::iam::messaging::put_seed_profile_parameter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::put_seed_profile_parameter_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_seed_profile_parameter_response& v);
    static ores::iam::messaging::put_seed_profile_parameter_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_seed_profile_parameter_response& v);
    static ores::iam::messaging::put_seed_profile_parameter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_many_seed_profile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_seed_profile_parameters_request& v);
    static ores::iam::messaging::put_many_seed_profile_parameters_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_seed_profile_parameters_request& v);
    static ores::iam::messaging::put_many_seed_profile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::put_many_seed_profile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::put_many_seed_profile_parameters_response& v);
    static ores::iam::messaging::put_many_seed_profile_parameters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::put_many_seed_profile_parameters_response& v);
    static ores::iam::messaging::put_many_seed_profile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_seed_profile_parameter_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_seed_profile_parameter_request& v);
    static ores::iam::messaging::delete_seed_profile_parameter_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_seed_profile_parameter_request& v);
    static ores::iam::messaging::delete_seed_profile_parameter_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_seed_profile_parameter_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_seed_profile_parameter_response& v);
    static ores::iam::messaging::delete_seed_profile_parameter_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_seed_profile_parameter_response& v);
    static ores::iam::messaging::delete_seed_profile_parameter_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_seed_profile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_seed_profile_parameters_request& v);
    static ores::iam::messaging::delete_many_seed_profile_parameters_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_seed_profile_parameters_request& v);
    static ores::iam::messaging::delete_many_seed_profile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::delete_many_seed_profile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::delete_many_seed_profile_parameters_response& v);
    static ores::iam::messaging::delete_many_seed_profile_parameters_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::delete_many_seed_profile_parameters_response& v);
    static ores::iam::messaging::delete_many_seed_profile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_request& v);
    static ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_request& v);
    static ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_response& v);
    static ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_response& v);
    static ores::iam::messaging::list_by_seed_profile_id_seed_profile_parameters_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_seed_profile_parameter_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_seed_profile_parameter_versions_request& v);
    static ores::iam::messaging::list_seed_profile_parameter_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_seed_profile_parameter_versions_request& v);
    static ores::iam::messaging::list_seed_profile_parameter_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::list_seed_profile_parameter_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::list_seed_profile_parameter_versions_response& v);
    static ores::iam::messaging::list_seed_profile_parameter_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::list_seed_profile_parameter_versions_response& v);
    static ores::iam::messaging::list_seed_profile_parameter_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_seed_profile_parameter_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_seed_profile_parameter_version_request& v);
    static ores::iam::messaging::get_seed_profile_parameter_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_seed_profile_parameter_version_request& v);
    static ores::iam::messaging::get_seed_profile_parameter_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT
    wire_traits<ores::iam::messaging::get_seed_profile_parameter_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::iam::messaging::get_seed_profile_parameter_version_response& v);
    static ores::iam::messaging::get_seed_profile_parameter_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::iam::messaging::get_seed_profile_parameter_version_response& v);
    static ores::iam::messaging::get_seed_profile_parameter_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
