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
#ifndef ORES_REPORTING_API_MESSAGING_CUT_KIND_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CUT_KIND_PROTOCOL_HPP

#include "ores.reporting.api/domain/cut_kind_codec.hpp"
#include "ores.reporting.api/export.hpp"
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

namespace ores::reporting::messaging {

struct cut_kind_key {
    std::string code;

    friend bool operator==(const cut_kind_key&, const cut_kind_key&) = default;
};

struct cut_kind_write {
    std::string code;
    std::string name;
    std::string description;
    std::string family_code;
    std::optional<std::string> approval_kind_code;
    bool output_is_indicative{};
    std::string resolution_rule_code;
    int retention_months{};
    std::string retention_destination_code;
    bool legal_hold_default{};
    int display_order{};

    friend bool operator==(const cut_kind_write&, const cut_kind_write&) = default;
};

struct cut_kind_change {
    cut_kind_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const cut_kind_change&, const cut_kind_change&) = default;
};

struct cut_kind_removal {
    cut_kind_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const cut_kind_removal&, const cut_kind_removal&) = default;
};

struct cut_kind_lookup {
    cut_kind_key key;
    std::optional<ores::reporting::domain::cut_kind> cut_kind;

    friend bool operator==(const cut_kind_lookup&, const cut_kind_lookup&) = default;
};

struct cut_kinds_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const cut_kinds_filter&, const cut_kinds_filter&) = default;
};

struct cut_kind_event {
    boost::uuids::uuid event_id;
    cut_kind_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const cut_kind_event&, const cut_kind_event&) = default;
};

struct cut_kind_version_key {
    cut_kind_key cut_kind;
    std::uint32_t version{};

    friend bool operator==(const cut_kind_version_key&, const cut_kind_version_key&) = default;
};

struct cut_kind_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const cut_kind_versions_filter&,
                           const cut_kind_versions_filter&) = default;
};

struct list_cut_kinds_request {
    using response_type = struct list_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.list";
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
    std::optional<cut_kinds_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_cut_kinds_request&, const list_cut_kinds_request&) = default;
};

struct list_cut_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_kind> kinds;
    std::uint64_t total{};

    friend bool operator==(const list_cut_kinds_response&,
                           const list_cut_kinds_response&) = default;
};

struct get_cut_kind_request {
    using response_type = struct get_cut_kind_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_key key;

    friend bool operator==(const get_cut_kind_request&, const get_cut_kind_request&) = default;
};

struct get_cut_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_kind> cut_kind;

    friend bool operator==(const get_cut_kind_response&, const get_cut_kind_response&) = default;
};

struct get_many_cut_kinds_request {
    using response_type = struct get_many_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_kind_key> keys;

    friend bool operator==(const get_many_cut_kinds_request&,
                           const get_many_cut_kinds_request&) = default;
};

struct get_many_cut_kinds_response {
    ores::utility::domain::result result;
    std::vector<cut_kind_lookup> entries;

    friend bool operator==(const get_many_cut_kinds_response&,
                           const get_many_cut_kinds_response&) = default;
};

struct put_cut_kind_request {
    using response_type = struct put_cut_kind_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_cut_kind_request&, const put_cut_kind_request&) = default;
};

struct put_cut_kind_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_kind> cut_kind;

    friend bool operator==(const put_cut_kind_response&, const put_cut_kind_response&) = default;
};

struct put_many_cut_kinds_request {
    using response_type = struct put_many_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_kind_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_cut_kinds_request&,
                           const put_many_cut_kinds_request&) = default;
};

struct put_many_cut_kinds_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_kind> kinds;

    friend bool operator==(const put_many_cut_kinds_response&,
                           const put_many_cut_kinds_response&) = default;
};

struct delete_cut_kind_request {
    using response_type = struct delete_cut_kind_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_cut_kind_request&,
                           const delete_cut_kind_request&) = default;
};

struct delete_cut_kind_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_cut_kind_response&,
                           const delete_cut_kind_response&) = default;
};

struct delete_many_cut_kinds_request {
    using response_type = struct delete_many_cut_kinds_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_kind_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_cut_kinds_request&,
                           const delete_many_cut_kinds_request&) = default;
};

struct delete_many_cut_kinds_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_cut_kinds_response&,
                           const delete_many_cut_kinds_response&) = default;
};

struct list_cut_kind_versions_request {
    using response_type = struct list_cut_kind_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cut_kind_versions_filter> filter;

    friend bool operator==(const list_cut_kind_versions_request&,
                           const list_cut_kind_versions_request&) = default;
};

struct list_cut_kind_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_kind> versions;
    std::uint64_t total{};

    friend bool operator==(const list_cut_kind_versions_response&,
                           const list_cut_kind_versions_response&) = default;
};

struct get_cut_kind_version_request {
    using response_type = struct get_cut_kind_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_kinds_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_kind_version_key key;

    friend bool operator==(const get_cut_kind_version_request&,
                           const get_cut_kind_version_request&) = default;
};

struct get_cut_kind_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_kind> version;

    friend bool operator==(const get_cut_kind_version_response&,
                           const get_cut_kind_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cut_kind_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.cut_kinds.created";
inline constexpr std::string_view updated = "reporting.v1.events.cut_kinds.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.cut_kinds.deleted";
}
/**
 * @brief The members of cut_kind_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &cut_kind_key::code},
    };
}

/**
 * @brief The members of cut_kind_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code", &cut_kind_write::code},
        utility::codec::field{"name", &cut_kind_write::name},
        utility::codec::field{"description", &cut_kind_write::description},
        utility::codec::field{"family_code", &cut_kind_write::family_code},
        utility::codec::field{"approval_kind_code", &cut_kind_write::approval_kind_code},
        utility::codec::field{"output_is_indicative", &cut_kind_write::output_is_indicative},
        utility::codec::field{"resolution_rule_code", &cut_kind_write::resolution_rule_code},
        utility::codec::field{"retention_months", &cut_kind_write::retention_months},
        utility::codec::field{"retention_destination_code",
                              &cut_kind_write::retention_destination_code},
        utility::codec::field{"legal_hold_default", &cut_kind_write::legal_hold_default},
        utility::codec::field{"display_order", &cut_kind_write::display_order},
    };
}

/**
 * @brief The members of cut_kind_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &cut_kind_change::write},
        utility::codec::field{"precondition", &cut_kind_change::precondition},
    };
}

/**
 * @brief The members of cut_kind_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &cut_kind_removal::key},
        utility::codec::field{"precondition", &cut_kind_removal::precondition},
    };
}

/**
 * @brief The members of cut_kind_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &cut_kind_lookup::key},
        utility::codec::field{"cut_kind", &cut_kind_lookup::cut_kind},
    };
}

/**
 * @brief The members of cut_kinds_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kinds_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"code_one_of", &cut_kinds_filter::code_one_of},
    };
}

/**
 * @brief The members of cut_kind_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &cut_kind_event::event_id},
        utility::codec::field{"key", &cut_kind_event::key},
        utility::codec::field{"action", &cut_kind_event::action},
        utility::codec::field{"version", &cut_kind_event::version},
        utility::codec::field{"occurred_at", &cut_kind_event::occurred_at},
        utility::codec::field{"correlation_id", &cut_kind_event::correlation_id},
    };
}

/**
 * @brief The members of cut_kind_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"cut_kind", &cut_kind_version_key::cut_kind},
        utility::codec::field{"version", &cut_kind_version_key::version},
    };
}

/**
 * @brief The members of cut_kind_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<cut_kind_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &cut_kind_versions_filter::version},
        utility::codec::field{"from_version", &cut_kind_versions_filter::from_version},
        utility::codec::field{"to_version", &cut_kind_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cut_kinds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cut_kinds_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_cut_kinds_request::offset},
        utility::codec::field{"limit", &list_cut_kinds_request::limit},
        utility::codec::field{"order", &list_cut_kinds_request::order},
        utility::codec::field{"filter", &list_cut_kinds_request::filter},
        utility::codec::field{"as_of", &list_cut_kinds_request::as_of},
    };
}

/**
 * @brief The members of list_cut_kinds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cut_kinds_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_cut_kinds_response::result},
        utility::codec::field{"kinds", &list_cut_kinds_response::kinds},
        utility::codec::field{"total", &list_cut_kinds_response::total},
    };
}

/**
 * @brief The members of get_cut_kind_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_kind_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_cut_kind_request::key},
    };
}

/**
 * @brief The members of get_cut_kind_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_kind_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_cut_kind_response::result},
        utility::codec::field{"cut_kind", &get_cut_kind_response::cut_kind},
    };
}

/**
 * @brief The members of get_many_cut_kinds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cut_kinds_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_cut_kinds_request::keys},
    };
}

/**
 * @brief The members of get_many_cut_kinds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_cut_kinds_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_cut_kinds_response::result},
        utility::codec::field{"entries", &get_many_cut_kinds_response::entries},
    };
}

/**
 * @brief The members of put_cut_kind_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_cut_kind_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_cut_kind_request::change},
        utility::codec::field{"intent", &put_cut_kind_request::intent},
    };
}

/**
 * @brief The members of put_cut_kind_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_cut_kind_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_cut_kind_response::result},
        utility::codec::field{"cut_kind", &put_cut_kind_response::cut_kind},
    };
}

/**
 * @brief The members of put_many_cut_kinds_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_cut_kinds_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_cut_kinds_request::changes},
        utility::codec::field{"intent", &put_many_cut_kinds_request::intent},
    };
}

/**
 * @brief The members of put_many_cut_kinds_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_cut_kinds_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_cut_kinds_response::result},
        utility::codec::field{"kinds", &put_many_cut_kinds_response::kinds},
    };
}

/**
 * @brief The members of delete_cut_kind_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_cut_kind_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_cut_kind_request::removal},
        utility::codec::field{"intent", &delete_cut_kind_request::intent},
    };
}

/**
 * @brief The members of delete_cut_kind_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_cut_kind_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_cut_kind_response::result},
    };
}

/**
 * @brief The members of delete_many_cut_kinds_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_cut_kinds_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_cut_kinds_request::removals},
        utility::codec::field{"intent", &delete_many_cut_kinds_request::intent},
    };
}

/**
 * @brief The members of delete_many_cut_kinds_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_cut_kinds_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_cut_kinds_response::result},
    };
}

/**
 * @brief The members of list_cut_kind_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cut_kind_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_cut_kind_versions_request::key},
        utility::codec::field{"offset", &list_cut_kind_versions_request::offset},
        utility::codec::field{"limit", &list_cut_kind_versions_request::limit},
        utility::codec::field{"order", &list_cut_kind_versions_request::order},
        utility::codec::field{"filter", &list_cut_kind_versions_request::filter},
    };
}

/**
 * @brief The members of list_cut_kind_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_cut_kind_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_cut_kind_versions_response::result},
        utility::codec::field{"versions", &list_cut_kind_versions_response::versions},
        utility::codec::field{"total", &list_cut_kind_versions_response::total},
    };
}

/**
 * @brief The members of get_cut_kind_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_kind_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_cut_kind_version_request::key},
    };
}

/**
 * @brief The members of get_cut_kind_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_cut_kind_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_cut_kind_version_response::result},
        utility::codec::field{"version", &get_cut_kind_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_key& v);
    static ores::reporting::messaging::cut_kind_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_key& v);
    static ores::reporting::messaging::cut_kind_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_write& v);
    static ores::reporting::messaging::cut_kind_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_write& v);
    static ores::reporting::messaging::cut_kind_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_change& v);
    static ores::reporting::messaging::cut_kind_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_change& v);
    static ores::reporting::messaging::cut_kind_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_removal& v);
    static ores::reporting::messaging::cut_kind_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_removal& v);
    static ores::reporting::messaging::cut_kind_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_lookup& v);
    static ores::reporting::messaging::cut_kind_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_lookup& v);
    static ores::reporting::messaging::cut_kind_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kinds_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kinds_filter& v);
    static ores::reporting::messaging::cut_kinds_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kinds_filter& v);
    static ores::reporting::messaging::cut_kinds_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_event& v);
    static ores::reporting::messaging::cut_kind_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_event& v);
    static ores::reporting::messaging::cut_kind_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_version_key& v);
    static ores::reporting::messaging::cut_kind_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_version_key& v);
    static ores::reporting::messaging::cut_kind_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_kind_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_kind_versions_filter& v);
    static ores::reporting::messaging::cut_kind_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_kind_versions_filter& v);
    static ores::reporting::messaging::cut_kind_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_cut_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cut_kinds_request& v);
    static ores::reporting::messaging::list_cut_kinds_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_cut_kinds_request& v);
    static ores::reporting::messaging::list_cut_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_cut_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cut_kinds_response& v);
    static ores::reporting::messaging::list_cut_kinds_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_cut_kinds_response& v);
    static ores::reporting::messaging::list_cut_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_kind_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_kind_request& v);
    static ores::reporting::messaging::get_cut_kind_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_kind_request& v);
    static ores::reporting::messaging::get_cut_kind_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_kind_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_kind_response& v);
    static ores::reporting::messaging::get_cut_kind_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_kind_response& v);
    static ores::reporting::messaging::get_cut_kind_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_cut_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_cut_kinds_request& v);
    static ores::reporting::messaging::get_many_cut_kinds_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_cut_kinds_request& v);
    static ores::reporting::messaging::get_many_cut_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_cut_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_cut_kinds_response& v);
    static ores::reporting::messaging::get_many_cut_kinds_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_cut_kinds_response& v);
    static ores::reporting::messaging::get_many_cut_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_cut_kind_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_cut_kind_request& v);
    static ores::reporting::messaging::put_cut_kind_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_cut_kind_request& v);
    static ores::reporting::messaging::put_cut_kind_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_cut_kind_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_cut_kind_response& v);
    static ores::reporting::messaging::put_cut_kind_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_cut_kind_response& v);
    static ores::reporting::messaging::put_cut_kind_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_cut_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_many_cut_kinds_request& v);
    static ores::reporting::messaging::put_many_cut_kinds_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_cut_kinds_request& v);
    static ores::reporting::messaging::put_many_cut_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_cut_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_many_cut_kinds_response& v);
    static ores::reporting::messaging::put_many_cut_kinds_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_cut_kinds_response& v);
    static ores::reporting::messaging::put_many_cut_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::delete_cut_kind_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_cut_kind_request& v);
    static ores::reporting::messaging::delete_cut_kind_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::delete_cut_kind_request& v);
    static ores::reporting::messaging::delete_cut_kind_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::delete_cut_kind_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_cut_kind_response& v);
    static ores::reporting::messaging::delete_cut_kind_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::delete_cut_kind_response& v);
    static ores::reporting::messaging::delete_cut_kind_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_cut_kinds_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_cut_kinds_request& v);
    static ores::reporting::messaging::delete_many_cut_kinds_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_cut_kinds_request& v);
    static ores::reporting::messaging::delete_many_cut_kinds_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_cut_kinds_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_cut_kinds_response& v);
    static ores::reporting::messaging::delete_many_cut_kinds_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_cut_kinds_response& v);
    static ores::reporting::messaging::delete_many_cut_kinds_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_cut_kind_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_cut_kind_versions_request& v);
    static ores::reporting::messaging::list_cut_kind_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_cut_kind_versions_request& v);
    static ores::reporting::messaging::list_cut_kind_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_cut_kind_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_cut_kind_versions_response& v);
    static ores::reporting::messaging::list_cut_kind_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_cut_kind_versions_response& v);
    static ores::reporting::messaging::list_cut_kind_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_cut_kind_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_cut_kind_version_request& v);
    static ores::reporting::messaging::get_cut_kind_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_cut_kind_version_request& v);
    static ores::reporting::messaging::get_cut_kind_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_cut_kind_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_cut_kind_version_response& v);
    static ores::reporting::messaging::get_cut_kind_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_cut_kind_version_response& v);
    static ores::reporting::messaging::get_cut_kind_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
