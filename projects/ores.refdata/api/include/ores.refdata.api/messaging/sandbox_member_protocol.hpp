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
#ifndef ORES_REFDATA_API_MESSAGING_SANDBOX_MEMBER_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_SANDBOX_MEMBER_PROTOCOL_HPP

#include "ores.refdata.api/domain/sandbox_member.hpp"
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

struct sandbox_member_key {
    boost::uuids::uuid id;
};

struct sandbox_member_write {
    boost::uuids::uuid id;
    boost::uuids::uuid sandbox_id;
    boost::uuids::uuid account_id;
};

struct sandbox_member_change {
    sandbox_member_write write;
    ores::utility::domain::precondition precondition;
};

struct sandbox_member_removal {
    sandbox_member_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct sandbox_member_lookup {
    sandbox_member_key key;
    std::optional<ores::refdata::domain::sandbox_member> sandbox_member;
};

struct sandbox_members_filter {
    std::optional<boost::uuids::uuid> sandbox_id;
    std::optional<boost::uuids::uuid> account_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> sandbox_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> account_id_one_of;
};

struct sandbox_member_event {
    boost::uuids::uuid event_id;
    sandbox_member_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct sandbox_member_version_key {
    sandbox_member_key sandbox_member;
    std::uint32_t version;
};

struct sandbox_member_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_sandbox_members_request {
    using response_type = struct list_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.list";
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
    std::optional<sandbox_members_filter> filter;
    std::optional<std::string> as_of;
};

struct list_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
    std::uint64_t total;
};

struct get_sandbox_member_request {
    using response_type = struct get_sandbox_member_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_key key;
};

struct get_sandbox_member_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox_member> sandbox_member;
};

struct get_many_sandbox_members_request {
    using response_type = struct get_many_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_member_key> keys;
};

struct get_many_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<sandbox_member_lookup> entries;
};

struct put_sandbox_member_request {
    using response_type = struct put_sandbox_member_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_change change;
    ores::utility::domain::change_intent intent;
};

struct put_sandbox_member_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox_member> sandbox_member;
};

struct put_many_sandbox_members_request {
    using response_type = struct put_many_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_member_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
};

struct delete_sandbox_member_request {
    using response_type = struct delete_sandbox_member_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_sandbox_member_response {
    ores::utility::domain::result result;
};

struct delete_many_sandbox_members_request {
    using response_type = struct delete_many_sandbox_members_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sandbox_member_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_sandbox_members_response {
    ores::utility::domain::result result;
};

struct list_by_sandbox_id_sandbox_members_request {
    using response_type = struct list_by_sandbox_id_sandbox_members_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.sandbox_members.list_by_sandbox_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid sandbox_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sandbox_members_filter> filter;
};

struct list_by_sandbox_id_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
    std::uint64_t total;
};

struct list_by_account_id_sandbox_members_request {
    using response_type = struct list_by_account_id_sandbox_members_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.sandbox_members.list_by_account_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sandbox_members_filter> filter;
};

struct list_by_account_id_sandbox_members_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> sandbox_members;
    std::uint64_t total;
};

struct list_sandbox_member_versions_request {
    using response_type = struct list_sandbox_member_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sandbox_member_versions_filter> filter;
};

struct list_sandbox_member_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::sandbox_member> versions;
    std::uint64_t total;
};

struct get_sandbox_member_version_request {
    using response_type = struct get_sandbox_member_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.sandbox_members_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sandbox_member_version_key key;
};

struct get_sandbox_member_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::sandbox_member> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace sandbox_member_event_subjects {
inline constexpr std::string_view created = "refdata.v1.sandbox_members_events.created";
inline constexpr std::string_view updated = "refdata.v1.sandbox_members_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.sandbox_members_events.deleted";
}
/**
 * @brief The members of sandbox_member_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_key&) {
    return std::tuple{
        utility::codec::field{"id", &sandbox_member_key::id},
    };
}

/**
 * @brief The members of sandbox_member_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_write&) {
    return std::tuple{
        utility::codec::field{"id", &sandbox_member_write::id},
        utility::codec::field{"sandbox_id", &sandbox_member_write::sandbox_id},
        utility::codec::field{"account_id", &sandbox_member_write::account_id},
    };
}

/**
 * @brief The members of sandbox_member_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_change&) {
    return std::tuple{
        utility::codec::field{"write", &sandbox_member_change::write},
        utility::codec::field{"precondition", &sandbox_member_change::precondition},
    };
}

/**
 * @brief The members of sandbox_member_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_removal&) {
    return std::tuple{
        utility::codec::field{"key", &sandbox_member_removal::key},
        utility::codec::field{"precondition", &sandbox_member_removal::precondition},
    };
}

/**
 * @brief The members of sandbox_member_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &sandbox_member_lookup::key},
        utility::codec::field{"sandbox_member", &sandbox_member_lookup::sandbox_member},
    };
}

/**
 * @brief The members of sandbox_members_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_members_filter&) {
    return std::tuple{
        utility::codec::field{"sandbox_id", &sandbox_members_filter::sandbox_id},
        utility::codec::field{"account_id", &sandbox_members_filter::account_id},
        utility::codec::field{"id_one_of", &sandbox_members_filter::id_one_of},
        utility::codec::field{"sandbox_id_one_of", &sandbox_members_filter::sandbox_id_one_of},
        utility::codec::field{"account_id_one_of", &sandbox_members_filter::account_id_one_of},
    };
}

/**
 * @brief The members of sandbox_member_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &sandbox_member_event::event_id},
        utility::codec::field{"key", &sandbox_member_event::key},
        utility::codec::field{"action", &sandbox_member_event::action},
        utility::codec::field{"version", &sandbox_member_event::version},
        utility::codec::field{"occurred_at", &sandbox_member_event::occurred_at},
        utility::codec::field{"correlation_id", &sandbox_member_event::correlation_id},
    };
}

/**
 * @brief The members of sandbox_member_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_version_key&) {
    return std::tuple{
        utility::codec::field{"sandbox_member", &sandbox_member_version_key::sandbox_member},
        utility::codec::field{"version", &sandbox_member_version_key::version},
    };
}

/**
 * @brief The members of sandbox_member_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const sandbox_member_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &sandbox_member_versions_filter::version},
        utility::codec::field{"from_version", &sandbox_member_versions_filter::from_version},
        utility::codec::field{"to_version", &sandbox_member_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_sandbox_members_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandbox_members_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_sandbox_members_request::offset},
        utility::codec::field{"limit", &list_sandbox_members_request::limit},
        utility::codec::field{"order", &list_sandbox_members_request::order},
        utility::codec::field{"filter", &list_sandbox_members_request::filter},
        utility::codec::field{"as_of", &list_sandbox_members_request::as_of},
    };
}

/**
 * @brief The members of list_sandbox_members_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandbox_members_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_sandbox_members_response::result},
        utility::codec::field{"sandbox_members", &list_sandbox_members_response::sandbox_members},
        utility::codec::field{"total", &list_sandbox_members_response::total},
    };
}

/**
 * @brief The members of get_sandbox_member_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_member_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_sandbox_member_request::key},
    };
}

/**
 * @brief The members of get_sandbox_member_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_member_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_sandbox_member_response::result},
        utility::codec::field{"sandbox_member", &get_sandbox_member_response::sandbox_member},
    };
}

/**
 * @brief The members of get_many_sandbox_members_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_sandbox_members_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_sandbox_members_request::keys},
    };
}

/**
 * @brief The members of get_many_sandbox_members_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_sandbox_members_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_sandbox_members_response::result},
        utility::codec::field{"entries", &get_many_sandbox_members_response::entries},
    };
}

/**
 * @brief The members of put_sandbox_member_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_sandbox_member_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_sandbox_member_request::change},
        utility::codec::field{"intent", &put_sandbox_member_request::intent},
    };
}

/**
 * @brief The members of put_sandbox_member_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_sandbox_member_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_sandbox_member_response::result},
        utility::codec::field{"sandbox_member", &put_sandbox_member_response::sandbox_member},
    };
}

/**
 * @brief The members of put_many_sandbox_members_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_sandbox_members_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_sandbox_members_request::changes},
        utility::codec::field{"intent", &put_many_sandbox_members_request::intent},
    };
}

/**
 * @brief The members of put_many_sandbox_members_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_sandbox_members_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_sandbox_members_response::result},
        utility::codec::field{"sandbox_members",
                              &put_many_sandbox_members_response::sandbox_members},
    };
}

/**
 * @brief The members of delete_sandbox_member_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_sandbox_member_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_sandbox_member_request::removal},
        utility::codec::field{"intent", &delete_sandbox_member_request::intent},
    };
}

/**
 * @brief The members of delete_sandbox_member_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_sandbox_member_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_sandbox_member_response::result},
    };
}

/**
 * @brief The members of delete_many_sandbox_members_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_sandbox_members_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_sandbox_members_request::removals},
        utility::codec::field{"intent", &delete_many_sandbox_members_request::intent},
    };
}

/**
 * @brief The members of delete_many_sandbox_members_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_sandbox_members_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_sandbox_members_response::result},
    };
}

/**
 * @brief The members of list_by_sandbox_id_sandbox_members_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_sandbox_id_sandbox_members_request&) {
    return std::tuple{
        utility::codec::field{"sandbox_id",
                              &list_by_sandbox_id_sandbox_members_request::sandbox_id},
        utility::codec::field{"scope", &list_by_sandbox_id_sandbox_members_request::scope},
        utility::codec::field{"offset", &list_by_sandbox_id_sandbox_members_request::offset},
        utility::codec::field{"limit", &list_by_sandbox_id_sandbox_members_request::limit},
        utility::codec::field{"order", &list_by_sandbox_id_sandbox_members_request::order},
        utility::codec::field{"filter", &list_by_sandbox_id_sandbox_members_request::filter},
    };
}

/**
 * @brief The members of list_by_sandbox_id_sandbox_members_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_sandbox_id_sandbox_members_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_sandbox_id_sandbox_members_response::result},
        utility::codec::field{"sandbox_members",
                              &list_by_sandbox_id_sandbox_members_response::sandbox_members},
        utility::codec::field{"total", &list_by_sandbox_id_sandbox_members_response::total},
    };
}

/**
 * @brief The members of list_by_account_id_sandbox_members_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_account_id_sandbox_members_request&) {
    return std::tuple{
        utility::codec::field{"account_id",
                              &list_by_account_id_sandbox_members_request::account_id},
        utility::codec::field{"scope", &list_by_account_id_sandbox_members_request::scope},
        utility::codec::field{"offset", &list_by_account_id_sandbox_members_request::offset},
        utility::codec::field{"limit", &list_by_account_id_sandbox_members_request::limit},
        utility::codec::field{"order", &list_by_account_id_sandbox_members_request::order},
        utility::codec::field{"filter", &list_by_account_id_sandbox_members_request::filter},
    };
}

/**
 * @brief The members of list_by_account_id_sandbox_members_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_account_id_sandbox_members_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_account_id_sandbox_members_response::result},
        utility::codec::field{"sandbox_members",
                              &list_by_account_id_sandbox_members_response::sandbox_members},
        utility::codec::field{"total", &list_by_account_id_sandbox_members_response::total},
    };
}

/**
 * @brief The members of list_sandbox_member_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandbox_member_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_sandbox_member_versions_request::key},
        utility::codec::field{"offset", &list_sandbox_member_versions_request::offset},
        utility::codec::field{"limit", &list_sandbox_member_versions_request::limit},
        utility::codec::field{"order", &list_sandbox_member_versions_request::order},
        utility::codec::field{"filter", &list_sandbox_member_versions_request::filter},
    };
}

/**
 * @brief The members of list_sandbox_member_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_sandbox_member_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_sandbox_member_versions_response::result},
        utility::codec::field{"versions", &list_sandbox_member_versions_response::versions},
        utility::codec::field{"total", &list_sandbox_member_versions_response::total},
    };
}

/**
 * @brief The members of get_sandbox_member_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_member_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_sandbox_member_version_request::key},
    };
}

/**
 * @brief The members of get_sandbox_member_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_sandbox_member_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_sandbox_member_version_response::result},
        utility::codec::field{"version", &get_sandbox_member_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_member_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_member_key& v);
    static ores::refdata::messaging::sandbox_member_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_member_key& v);
    static ores::refdata::messaging::sandbox_member_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_member_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_member_write& v);
    static ores::refdata::messaging::sandbox_member_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_member_write& v);
    static ores::refdata::messaging::sandbox_member_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_member_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_member_change& v);
    static ores::refdata::messaging::sandbox_member_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_member_change& v);
    static ores::refdata::messaging::sandbox_member_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_member_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_member_removal& v);
    static ores::refdata::messaging::sandbox_member_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_member_removal& v);
    static ores::refdata::messaging::sandbox_member_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_member_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_member_lookup& v);
    static ores::refdata::messaging::sandbox_member_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_member_lookup& v);
    static ores::refdata::messaging::sandbox_member_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_members_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_members_filter& v);
    static ores::refdata::messaging::sandbox_members_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_members_filter& v);
    static ores::refdata::messaging::sandbox_members_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_member_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_member_event& v);
    static ores::refdata::messaging::sandbox_member_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_member_event& v);
    static ores::refdata::messaging::sandbox_member_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::sandbox_member_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::sandbox_member_version_key& v);
    static ores::refdata::messaging::sandbox_member_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::sandbox_member_version_key& v);
    static ores::refdata::messaging::sandbox_member_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::sandbox_member_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::sandbox_member_versions_filter& v);
    static ores::refdata::messaging::sandbox_member_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::sandbox_member_versions_filter& v);
    static ores::refdata::messaging::sandbox_member_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_sandbox_members_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_sandbox_members_request& v);
    static ores::refdata::messaging::list_sandbox_members_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_sandbox_members_request& v);
    static ores::refdata::messaging::list_sandbox_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_sandbox_members_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_sandbox_members_response& v);
    static ores::refdata::messaging::list_sandbox_members_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_sandbox_members_response& v);
    static ores::refdata::messaging::list_sandbox_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_sandbox_member_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_sandbox_member_request& v);
    static ores::refdata::messaging::get_sandbox_member_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_sandbox_member_request& v);
    static ores::refdata::messaging::get_sandbox_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_sandbox_member_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_sandbox_member_response& v);
    static ores::refdata::messaging::get_sandbox_member_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_sandbox_member_response& v);
    static ores::refdata::messaging::get_sandbox_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_sandbox_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_sandbox_members_request& v);
    static ores::refdata::messaging::get_many_sandbox_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_sandbox_members_request& v);
    static ores::refdata::messaging::get_many_sandbox_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_sandbox_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_sandbox_members_response& v);
    static ores::refdata::messaging::get_many_sandbox_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_sandbox_members_response& v);
    static ores::refdata::messaging::get_many_sandbox_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_sandbox_member_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_sandbox_member_request& v);
    static ores::refdata::messaging::put_sandbox_member_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_sandbox_member_request& v);
    static ores::refdata::messaging::put_sandbox_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_sandbox_member_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_sandbox_member_response& v);
    static ores::refdata::messaging::put_sandbox_member_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_sandbox_member_response& v);
    static ores::refdata::messaging::put_sandbox_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_sandbox_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_sandbox_members_request& v);
    static ores::refdata::messaging::put_many_sandbox_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_sandbox_members_request& v);
    static ores::refdata::messaging::put_many_sandbox_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_sandbox_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_sandbox_members_response& v);
    static ores::refdata::messaging::put_many_sandbox_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_sandbox_members_response& v);
    static ores::refdata::messaging::put_many_sandbox_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_sandbox_member_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_sandbox_member_request& v);
    static ores::refdata::messaging::delete_sandbox_member_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_sandbox_member_request& v);
    static ores::refdata::messaging::delete_sandbox_member_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_sandbox_member_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_sandbox_member_response& v);
    static ores::refdata::messaging::delete_sandbox_member_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_sandbox_member_response& v);
    static ores::refdata::messaging::delete_sandbox_member_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_sandbox_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_sandbox_members_request& v);
    static ores::refdata::messaging::delete_many_sandbox_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_sandbox_members_request& v);
    static ores::refdata::messaging::delete_many_sandbox_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_sandbox_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_sandbox_members_response& v);
    static ores::refdata::messaging::delete_many_sandbox_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_sandbox_members_response& v);
    static ores::refdata::messaging::delete_many_sandbox_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_sandbox_id_sandbox_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_sandbox_id_sandbox_members_request& v);
    static ores::refdata::messaging::list_by_sandbox_id_sandbox_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_sandbox_id_sandbox_members_request& v);
    static ores::refdata::messaging::list_by_sandbox_id_sandbox_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_sandbox_id_sandbox_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_sandbox_id_sandbox_members_response& v);
    static ores::refdata::messaging::list_by_sandbox_id_sandbox_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_sandbox_id_sandbox_members_response& v);
    static ores::refdata::messaging::list_by_sandbox_id_sandbox_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_account_id_sandbox_members_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_account_id_sandbox_members_request& v);
    static ores::refdata::messaging::list_by_account_id_sandbox_members_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_account_id_sandbox_members_request& v);
    static ores::refdata::messaging::list_by_account_id_sandbox_members_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_by_account_id_sandbox_members_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_by_account_id_sandbox_members_response& v);
    static ores::refdata::messaging::list_by_account_id_sandbox_members_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_by_account_id_sandbox_members_response& v);
    static ores::refdata::messaging::list_by_account_id_sandbox_members_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_sandbox_member_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_sandbox_member_versions_request& v);
    static ores::refdata::messaging::list_sandbox_member_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_sandbox_member_versions_request& v);
    static ores::refdata::messaging::list_sandbox_member_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_sandbox_member_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_sandbox_member_versions_response& v);
    static ores::refdata::messaging::list_sandbox_member_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_sandbox_member_versions_response& v);
    static ores::refdata::messaging::list_sandbox_member_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_sandbox_member_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_sandbox_member_version_request& v);
    static ores::refdata::messaging::get_sandbox_member_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_sandbox_member_version_request& v);
    static ores::refdata::messaging::get_sandbox_member_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_sandbox_member_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_sandbox_member_version_response& v);
    static ores::refdata::messaging::get_sandbox_member_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_sandbox_member_version_response& v);
    static ores::refdata::messaging::get_sandbox_member_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
