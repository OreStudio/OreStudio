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
#ifndef ORES_REFDATA_API_MESSAGING_BUNDLE_IMPORT_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_BUNDLE_IMPORT_PROTOCOL_HPP

#include "ores.refdata.api/domain/bundle_import_codec.hpp"
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

struct bundle_import_key {
    boost::uuids::uuid id;

    friend bool operator==(const bundle_import_key&, const bundle_import_key&) = default;
};

struct bundle_import_write {
    boost::uuids::uuid id;
    std::optional<std::string> source;
    std::string state;
    std::optional<int> families_expected;
    std::chrono::system_clock::time_point expires_at;

    friend bool operator==(const bundle_import_write&, const bundle_import_write&) = default;
};

struct bundle_import_change {
    bundle_import_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const bundle_import_change&, const bundle_import_change&) = default;
};

struct bundle_import_removal {
    bundle_import_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const bundle_import_removal&, const bundle_import_removal&) = default;
};

struct bundle_import_lookup {
    bundle_import_key key;
    std::optional<ores::refdata::domain::bundle_import> bundle_import;

    friend bool operator==(const bundle_import_lookup&, const bundle_import_lookup&) = default;
};

struct bundle_imports_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const bundle_imports_filter&, const bundle_imports_filter&) = default;
};

struct bundle_import_event {
    boost::uuids::uuid event_id;
    bundle_import_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const bundle_import_event&, const bundle_import_event&) = default;
};

struct bundle_import_version_key {
    bundle_import_key bundle_import;
    std::uint32_t version{};

    friend bool operator==(const bundle_import_version_key&,
                           const bundle_import_version_key&) = default;
};

struct bundle_import_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const bundle_import_versions_filter&,
                           const bundle_import_versions_filter&) = default;
};

struct list_bundle_imports_request {
    using response_type = struct list_bundle_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports.list";
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
    std::optional<bundle_imports_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_bundle_imports_request&,
                           const list_bundle_imports_request&) = default;
};

struct list_bundle_imports_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bundle_import> bundle_imports;
    std::uint64_t total{};

    friend bool operator==(const list_bundle_imports_response&,
                           const list_bundle_imports_response&) = default;
};

struct get_bundle_import_request {
    using response_type = struct get_bundle_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_key key;

    friend bool operator==(const get_bundle_import_request&,
                           const get_bundle_import_request&) = default;
};

struct get_bundle_import_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bundle_import> bundle_import;

    friend bool operator==(const get_bundle_import_response&,
                           const get_bundle_import_response&) = default;
};

struct get_many_bundle_imports_request {
    using response_type = struct get_many_bundle_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bundle_import_key> keys;

    friend bool operator==(const get_many_bundle_imports_request&,
                           const get_many_bundle_imports_request&) = default;
};

struct get_many_bundle_imports_response {
    ores::utility::domain::result result;
    std::vector<bundle_import_lookup> entries;

    friend bool operator==(const get_many_bundle_imports_response&,
                           const get_many_bundle_imports_response&) = default;
};

struct put_bundle_import_request {
    using response_type = struct put_bundle_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_bundle_import_request&,
                           const put_bundle_import_request&) = default;
};

struct put_bundle_import_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bundle_import> bundle_import;

    friend bool operator==(const put_bundle_import_response&,
                           const put_bundle_import_response&) = default;
};

struct put_many_bundle_imports_request {
    using response_type = struct put_many_bundle_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bundle_import_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_bundle_imports_request&,
                           const put_many_bundle_imports_request&) = default;
};

struct put_many_bundle_imports_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bundle_import> bundle_imports;

    friend bool operator==(const put_many_bundle_imports_response&,
                           const put_many_bundle_imports_response&) = default;
};

struct delete_bundle_import_request {
    using response_type = struct delete_bundle_import_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_bundle_import_request&,
                           const delete_bundle_import_request&) = default;
};

struct delete_bundle_import_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_bundle_import_response&,
                           const delete_bundle_import_response&) = default;
};

struct delete_many_bundle_imports_request {
    using response_type = struct delete_many_bundle_imports_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<bundle_import_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_bundle_imports_request&,
                           const delete_many_bundle_imports_request&) = default;
};

struct delete_many_bundle_imports_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_bundle_imports_response&,
                           const delete_many_bundle_imports_response&) = default;
};

struct list_bundle_import_versions_request {
    using response_type = struct list_bundle_import_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<bundle_import_versions_filter> filter;

    friend bool operator==(const list_bundle_import_versions_request&,
                           const list_bundle_import_versions_request&) = default;
};

struct list_bundle_import_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::bundle_import> versions;
    std::uint64_t total{};

    friend bool operator==(const list_bundle_import_versions_response&,
                           const list_bundle_import_versions_response&) = default;
};

struct get_bundle_import_version_request {
    using response_type = struct get_bundle_import_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.bundle_imports_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bundle_import_version_key key;

    friend bool operator==(const get_bundle_import_version_request&,
                           const get_bundle_import_version_request&) = default;
};

struct get_bundle_import_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::bundle_import> version;

    friend bool operator==(const get_bundle_import_version_response&,
                           const get_bundle_import_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace bundle_import_event_subjects {
inline constexpr std::string_view created = "refdata.v1.events.bundle_imports.created";
inline constexpr std::string_view updated = "refdata.v1.events.bundle_imports.updated";
inline constexpr std::string_view deleted = "refdata.v1.events.bundle_imports.deleted";
}
/**
 * @brief The members of bundle_import_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &bundle_import_key::id},
    };
}

/**
 * @brief The members of bundle_import_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &bundle_import_write::id},
        utility::codec::field{"source", &bundle_import_write::source},
        utility::codec::field{"state", &bundle_import_write::state},
        utility::codec::field{"families_expected", &bundle_import_write::families_expected},
        utility::codec::field{"expires_at", &bundle_import_write::expires_at},
    };
}

/**
 * @brief The members of bundle_import_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &bundle_import_change::write},
        utility::codec::field{"precondition", &bundle_import_change::precondition},
    };
}

/**
 * @brief The members of bundle_import_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &bundle_import_removal::key},
        utility::codec::field{"precondition", &bundle_import_removal::precondition},
    };
}

/**
 * @brief The members of bundle_import_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &bundle_import_lookup::key},
        utility::codec::field{"bundle_import", &bundle_import_lookup::bundle_import},
    };
}

/**
 * @brief The members of bundle_imports_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_imports_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &bundle_imports_filter::id_one_of},
    };
}

/**
 * @brief The members of bundle_import_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &bundle_import_event::event_id},
        utility::codec::field{"key", &bundle_import_event::key},
        utility::codec::field{"action", &bundle_import_event::action},
        utility::codec::field{"version", &bundle_import_event::version},
        utility::codec::field{"occurred_at", &bundle_import_event::occurred_at},
        utility::codec::field{"correlation_id", &bundle_import_event::correlation_id},
    };
}

/**
 * @brief The members of bundle_import_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"bundle_import", &bundle_import_version_key::bundle_import},
        utility::codec::field{"version", &bundle_import_version_key::version},
    };
}

/**
 * @brief The members of bundle_import_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bundle_import_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &bundle_import_versions_filter::version},
        utility::codec::field{"from_version", &bundle_import_versions_filter::from_version},
        utility::codec::field{"to_version", &bundle_import_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_bundle_imports_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_bundle_imports_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_bundle_imports_request::offset},
        utility::codec::field{"limit", &list_bundle_imports_request::limit},
        utility::codec::field{"order", &list_bundle_imports_request::order},
        utility::codec::field{"filter", &list_bundle_imports_request::filter},
        utility::codec::field{"as_of", &list_bundle_imports_request::as_of},
    };
}

/**
 * @brief The members of list_bundle_imports_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_bundle_imports_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_bundle_imports_response::result},
        utility::codec::field{"bundle_imports", &list_bundle_imports_response::bundle_imports},
        utility::codec::field{"total", &list_bundle_imports_response::total},
    };
}

/**
 * @brief The members of get_bundle_import_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_bundle_import_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_bundle_import_request::key},
    };
}

/**
 * @brief The members of get_bundle_import_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_bundle_import_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_bundle_import_response::result},
        utility::codec::field{"bundle_import", &get_bundle_import_response::bundle_import},
    };
}

/**
 * @brief The members of get_many_bundle_imports_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_bundle_imports_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_bundle_imports_request::keys},
    };
}

/**
 * @brief The members of get_many_bundle_imports_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_bundle_imports_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_bundle_imports_response::result},
        utility::codec::field{"entries", &get_many_bundle_imports_response::entries},
    };
}

/**
 * @brief The members of put_bundle_import_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_bundle_import_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_bundle_import_request::change},
        utility::codec::field{"intent", &put_bundle_import_request::intent},
    };
}

/**
 * @brief The members of put_bundle_import_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_bundle_import_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_bundle_import_response::result},
        utility::codec::field{"bundle_import", &put_bundle_import_response::bundle_import},
    };
}

/**
 * @brief The members of put_many_bundle_imports_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_bundle_imports_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_bundle_imports_request::changes},
        utility::codec::field{"intent", &put_many_bundle_imports_request::intent},
    };
}

/**
 * @brief The members of put_many_bundle_imports_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_bundle_imports_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_bundle_imports_response::result},
        utility::codec::field{"bundle_imports", &put_many_bundle_imports_response::bundle_imports},
    };
}

/**
 * @brief The members of delete_bundle_import_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_bundle_import_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_bundle_import_request::removal},
        utility::codec::field{"intent", &delete_bundle_import_request::intent},
    };
}

/**
 * @brief The members of delete_bundle_import_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_bundle_import_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_bundle_import_response::result},
    };
}

/**
 * @brief The members of delete_many_bundle_imports_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_bundle_imports_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_bundle_imports_request::removals},
        utility::codec::field{"intent", &delete_many_bundle_imports_request::intent},
    };
}

/**
 * @brief The members of delete_many_bundle_imports_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_bundle_imports_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_bundle_imports_response::result},
    };
}

/**
 * @brief The members of list_bundle_import_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_bundle_import_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_bundle_import_versions_request::key},
        utility::codec::field{"offset", &list_bundle_import_versions_request::offset},
        utility::codec::field{"limit", &list_bundle_import_versions_request::limit},
        utility::codec::field{"order", &list_bundle_import_versions_request::order},
        utility::codec::field{"filter", &list_bundle_import_versions_request::filter},
    };
}

/**
 * @brief The members of list_bundle_import_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_bundle_import_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_bundle_import_versions_response::result},
        utility::codec::field{"versions", &list_bundle_import_versions_response::versions},
        utility::codec::field{"total", &list_bundle_import_versions_response::total},
    };
}

/**
 * @brief The members of get_bundle_import_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_bundle_import_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_bundle_import_version_request::key},
    };
}

/**
 * @brief The members of get_bundle_import_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_bundle_import_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_bundle_import_version_response::result},
        utility::codec::field{"version", &get_bundle_import_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_key& v);
    static ores::refdata::messaging::bundle_import_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_key& v);
    static ores::refdata::messaging::bundle_import_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_write& v);
    static ores::refdata::messaging::bundle_import_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_write& v);
    static ores::refdata::messaging::bundle_import_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_change& v);
    static ores::refdata::messaging::bundle_import_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_change& v);
    static ores::refdata::messaging::bundle_import_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_removal& v);
    static ores::refdata::messaging::bundle_import_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_removal& v);
    static ores::refdata::messaging::bundle_import_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_lookup& v);
    static ores::refdata::messaging::bundle_import_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_lookup& v);
    static ores::refdata::messaging::bundle_import_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_imports_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_imports_filter& v);
    static ores::refdata::messaging::bundle_imports_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_imports_filter& v);
    static ores::refdata::messaging::bundle_imports_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_event& v);
    static ores::refdata::messaging::bundle_import_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_event& v);
    static ores::refdata::messaging::bundle_import_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::bundle_import_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_version_key& v);
    static ores::refdata::messaging::bundle_import_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::bundle_import_version_key& v);
    static ores::refdata::messaging::bundle_import_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::bundle_import_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::bundle_import_versions_filter& v);
    static ores::refdata::messaging::bundle_import_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::bundle_import_versions_filter& v);
    static ores::refdata::messaging::bundle_import_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_bundle_imports_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_bundle_imports_request& v);
    static ores::refdata::messaging::list_bundle_imports_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_imports_request& v);
    static ores::refdata::messaging::list_bundle_imports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_bundle_imports_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_bundle_imports_response& v);
    static ores::refdata::messaging::list_bundle_imports_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_imports_response& v);
    static ores::refdata::messaging::list_bundle_imports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_bundle_import_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_bundle_import_request& v);
    static ores::refdata::messaging::get_bundle_import_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_bundle_import_request& v);
    static ores::refdata::messaging::get_bundle_import_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_bundle_import_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_bundle_import_response& v);
    static ores::refdata::messaging::get_bundle_import_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_bundle_import_response& v);
    static ores::refdata::messaging::get_bundle_import_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_bundle_imports_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_bundle_imports_request& v);
    static ores::refdata::messaging::get_many_bundle_imports_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_bundle_imports_request& v);
    static ores::refdata::messaging::get_many_bundle_imports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_bundle_imports_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_bundle_imports_response& v);
    static ores::refdata::messaging::get_many_bundle_imports_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_bundle_imports_response& v);
    static ores::refdata::messaging::get_many_bundle_imports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_bundle_import_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_bundle_import_request& v);
    static ores::refdata::messaging::put_bundle_import_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_bundle_import_request& v);
    static ores::refdata::messaging::put_bundle_import_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_bundle_import_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_bundle_import_response& v);
    static ores::refdata::messaging::put_bundle_import_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_bundle_import_response& v);
    static ores::refdata::messaging::put_bundle_import_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_bundle_imports_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_bundle_imports_request& v);
    static ores::refdata::messaging::put_many_bundle_imports_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_bundle_imports_request& v);
    static ores::refdata::messaging::put_many_bundle_imports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_bundle_imports_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_bundle_imports_response& v);
    static ores::refdata::messaging::put_many_bundle_imports_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_bundle_imports_response& v);
    static ores::refdata::messaging::put_many_bundle_imports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_bundle_import_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_bundle_import_request& v);
    static ores::refdata::messaging::delete_bundle_import_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_bundle_import_request& v);
    static ores::refdata::messaging::delete_bundle_import_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_bundle_import_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_bundle_import_response& v);
    static ores::refdata::messaging::delete_bundle_import_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_bundle_import_response& v);
    static ores::refdata::messaging::delete_bundle_import_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_bundle_imports_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_bundle_imports_request& v);
    static ores::refdata::messaging::delete_many_bundle_imports_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_bundle_imports_request& v);
    static ores::refdata::messaging::delete_many_bundle_imports_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_bundle_imports_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_bundle_imports_response& v);
    static ores::refdata::messaging::delete_many_bundle_imports_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_bundle_imports_response& v);
    static ores::refdata::messaging::delete_many_bundle_imports_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bundle_import_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bundle_import_versions_request& v);
    static ores::refdata::messaging::list_bundle_import_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_import_versions_request& v);
    static ores::refdata::messaging::list_bundle_import_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_bundle_import_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_bundle_import_versions_response& v);
    static ores::refdata::messaging::list_bundle_import_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_bundle_import_versions_response& v);
    static ores::refdata::messaging::list_bundle_import_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bundle_import_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bundle_import_version_request& v);
    static ores::refdata::messaging::get_bundle_import_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bundle_import_version_request& v);
    static ores::refdata::messaging::get_bundle_import_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_bundle_import_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_bundle_import_version_response& v);
    static ores::refdata::messaging::get_bundle_import_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_bundle_import_version_response& v);
    static ores::refdata::messaging::get_bundle_import_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
