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
#ifndef ORES_REPORTING_API_MESSAGING_CUT_GAP_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CUT_GAP_PROTOCOL_HPP

#include "ores.reporting.api/domain/cut_gap.hpp"
#include "ores.reporting.api/export.hpp"
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

namespace ores::reporting::messaging {

struct cut_gap_key {
    boost::uuids::uuid id;

    friend bool operator==(const cut_gap_key&, const cut_gap_key&) = default;
};

struct cut_gap_write {
    boost::uuids::uuid id;
    boost::uuids::uuid cut_id;
    std::string part_code;
    std::string item;
    std::string reason;

    friend bool operator==(const cut_gap_write&, const cut_gap_write&) = default;
};

struct cut_gap_change {
    cut_gap_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const cut_gap_change&, const cut_gap_change&) = default;
};

struct cut_gap_removal {
    cut_gap_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const cut_gap_removal&, const cut_gap_removal&) = default;
};

struct cut_gap_lookup {
    cut_gap_key key;
    std::optional<ores::reporting::domain::cut_gap> cut_gap;

    friend bool operator==(const cut_gap_lookup&, const cut_gap_lookup&) = default;
};

struct cut_gaps_filter {
    std::optional<boost::uuids::uuid> cut_id;
    std::optional<boost::uuids::uuid> party_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> cut_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;

    friend bool operator==(const cut_gaps_filter&, const cut_gaps_filter&) = default;
};

struct cut_gap_event {
    boost::uuids::uuid event_id;
    cut_gap_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const cut_gap_event&, const cut_gap_event&) = default;
};

struct cut_gap_version_key {
    cut_gap_key cut_gap;
    std::uint32_t version;

    friend bool operator==(const cut_gap_version_key&, const cut_gap_version_key&) = default;
};

struct cut_gap_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const cut_gap_versions_filter&,
                           const cut_gap_versions_filter&) = default;
};

struct list_cut_gaps_request {
    using response_type = struct list_cut_gaps_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.list";
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
    std::optional<cut_gaps_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_cut_gaps_request&, const list_cut_gaps_request&) = default;
};

struct list_cut_gaps_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_gap> gaps;
    std::uint64_t total;

    friend bool operator==(const list_cut_gaps_response&, const list_cut_gaps_response&) = default;
};

struct get_cut_gap_request {
    using response_type = struct get_cut_gap_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_gap_key key;

    friend bool operator==(const get_cut_gap_request&, const get_cut_gap_request&) = default;
};

struct get_cut_gap_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_gap> cut_gap;

    friend bool operator==(const get_cut_gap_response&, const get_cut_gap_response&) = default;
};

struct get_many_cut_gaps_request {
    using response_type = struct get_many_cut_gaps_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_gap_key> keys;

    friend bool operator==(const get_many_cut_gaps_request&,
                           const get_many_cut_gaps_request&) = default;
};

struct get_many_cut_gaps_response {
    ores::utility::domain::result result;
    std::vector<cut_gap_lookup> entries;

    friend bool operator==(const get_many_cut_gaps_response&,
                           const get_many_cut_gaps_response&) = default;
};

struct put_cut_gap_request {
    using response_type = struct put_cut_gap_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_gap_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_cut_gap_request&, const put_cut_gap_request&) = default;
};

struct put_cut_gap_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_gap> cut_gap;

    friend bool operator==(const put_cut_gap_response&, const put_cut_gap_response&) = default;
};

struct put_many_cut_gaps_request {
    using response_type = struct put_many_cut_gaps_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_gap_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_cut_gaps_request&,
                           const put_many_cut_gaps_request&) = default;
};

struct put_many_cut_gaps_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_gap> gaps;

    friend bool operator==(const put_many_cut_gaps_response&,
                           const put_many_cut_gaps_response&) = default;
};

struct delete_cut_gap_request {
    using response_type = struct delete_cut_gap_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_gap_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_cut_gap_request&, const delete_cut_gap_request&) = default;
};

struct delete_cut_gap_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_cut_gap_response&,
                           const delete_cut_gap_response&) = default;
};

struct delete_many_cut_gaps_request {
    using response_type = struct delete_many_cut_gaps_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<cut_gap_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_cut_gaps_request&,
                           const delete_many_cut_gaps_request&) = default;
};

struct delete_many_cut_gaps_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_cut_gaps_response&,
                           const delete_many_cut_gaps_response&) = default;
};

struct list_by_cut_id_cut_gaps_request {
    using response_type = struct list_by_cut_id_cut_gaps_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps.list_by_cut_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid cut_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cut_gaps_filter> filter;

    friend bool operator==(const list_by_cut_id_cut_gaps_request&,
                           const list_by_cut_id_cut_gaps_request&) = default;
};

struct list_by_cut_id_cut_gaps_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_gap> gaps;
    std::uint64_t total;

    friend bool operator==(const list_by_cut_id_cut_gaps_response&,
                           const list_by_cut_id_cut_gaps_response&) = default;
};

struct list_cut_gap_versions_request {
    using response_type = struct list_cut_gap_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_gap_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<cut_gap_versions_filter> filter;

    friend bool operator==(const list_cut_gap_versions_request&,
                           const list_cut_gap_versions_request&) = default;
};

struct list_cut_gap_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::cut_gap> versions;
    std::uint64_t total;

    friend bool operator==(const list_cut_gap_versions_response&,
                           const list_cut_gap_versions_response&) = default;
};

struct get_cut_gap_version_request {
    using response_type = struct get_cut_gap_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.cut_gaps_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    cut_gap_version_key key;

    friend bool operator==(const get_cut_gap_version_request&,
                           const get_cut_gap_version_request&) = default;
};

struct get_cut_gap_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::cut_gap> version;

    friend bool operator==(const get_cut_gap_version_response&,
                           const get_cut_gap_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace cut_gap_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.cut_gaps.created";
inline constexpr std::string_view updated = "reporting.v1.events.cut_gaps.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.cut_gaps.deleted";
}
/**
 * @brief The members of cut_gap_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_key&) {
    return std::tuple{
        utility::codec::field{"id", &cut_gap_key::id},
    };
}

/**
 * @brief The members of cut_gap_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_write&) {
    return std::tuple{
        utility::codec::field{"id", &cut_gap_write::id},
        utility::codec::field{"cut_id", &cut_gap_write::cut_id},
        utility::codec::field{"part_code", &cut_gap_write::part_code},
        utility::codec::field{"item", &cut_gap_write::item},
        utility::codec::field{"reason", &cut_gap_write::reason},
    };
}

/**
 * @brief The members of cut_gap_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_change&) {
    return std::tuple{
        utility::codec::field{"write", &cut_gap_change::write},
        utility::codec::field{"precondition", &cut_gap_change::precondition},
    };
}

/**
 * @brief The members of cut_gap_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_removal&) {
    return std::tuple{
        utility::codec::field{"key", &cut_gap_removal::key},
        utility::codec::field{"precondition", &cut_gap_removal::precondition},
    };
}

/**
 * @brief The members of cut_gap_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &cut_gap_lookup::key},
        utility::codec::field{"cut_gap", &cut_gap_lookup::cut_gap},
    };
}

/**
 * @brief The members of cut_gaps_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gaps_filter&) {
    return std::tuple{
        utility::codec::field{"cut_id", &cut_gaps_filter::cut_id},
        utility::codec::field{"party_id", &cut_gaps_filter::party_id},
        utility::codec::field{"id_one_of", &cut_gaps_filter::id_one_of},
        utility::codec::field{"cut_id_one_of", &cut_gaps_filter::cut_id_one_of},
        utility::codec::field{"party_id_one_of", &cut_gaps_filter::party_id_one_of},
    };
}

/**
 * @brief The members of cut_gap_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &cut_gap_event::event_id},
        utility::codec::field{"key", &cut_gap_event::key},
        utility::codec::field{"action", &cut_gap_event::action},
        utility::codec::field{"version", &cut_gap_event::version},
        utility::codec::field{"occurred_at", &cut_gap_event::occurred_at},
        utility::codec::field{"correlation_id", &cut_gap_event::correlation_id},
    };
}

/**
 * @brief The members of cut_gap_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_version_key&) {
    return std::tuple{
        utility::codec::field{"cut_gap", &cut_gap_version_key::cut_gap},
        utility::codec::field{"version", &cut_gap_version_key::version},
    };
}

/**
 * @brief The members of cut_gap_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const cut_gap_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &cut_gap_versions_filter::version},
        utility::codec::field{"from_version", &cut_gap_versions_filter::from_version},
        utility::codec::field{"to_version", &cut_gap_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_cut_gaps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cut_gaps_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_cut_gaps_request::offset},
        utility::codec::field{"limit", &list_cut_gaps_request::limit},
        utility::codec::field{"order", &list_cut_gaps_request::order},
        utility::codec::field{"filter", &list_cut_gaps_request::filter},
        utility::codec::field{"as_of", &list_cut_gaps_request::as_of},
    };
}

/**
 * @brief The members of list_cut_gaps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cut_gaps_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_cut_gaps_response::result},
        utility::codec::field{"gaps", &list_cut_gaps_response::gaps},
        utility::codec::field{"total", &list_cut_gaps_response::total},
    };
}

/**
 * @brief The members of get_cut_gap_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cut_gap_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_cut_gap_request::key},
    };
}

/**
 * @brief The members of get_cut_gap_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cut_gap_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_cut_gap_response::result},
        utility::codec::field{"cut_gap", &get_cut_gap_response::cut_gap},
    };
}

/**
 * @brief The members of get_many_cut_gaps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_cut_gaps_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_cut_gaps_request::keys},
    };
}

/**
 * @brief The members of get_many_cut_gaps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_cut_gaps_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_cut_gaps_response::result},
        utility::codec::field{"entries", &get_many_cut_gaps_response::entries},
    };
}

/**
 * @brief The members of put_cut_gap_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_cut_gap_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_cut_gap_request::change},
        utility::codec::field{"intent", &put_cut_gap_request::intent},
    };
}

/**
 * @brief The members of put_cut_gap_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_cut_gap_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_cut_gap_response::result},
        utility::codec::field{"cut_gap", &put_cut_gap_response::cut_gap},
    };
}

/**
 * @brief The members of put_many_cut_gaps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_cut_gaps_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_cut_gaps_request::changes},
        utility::codec::field{"intent", &put_many_cut_gaps_request::intent},
    };
}

/**
 * @brief The members of put_many_cut_gaps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_cut_gaps_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_cut_gaps_response::result},
        utility::codec::field{"gaps", &put_many_cut_gaps_response::gaps},
    };
}

/**
 * @brief The members of delete_cut_gap_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_cut_gap_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_cut_gap_request::removal},
        utility::codec::field{"intent", &delete_cut_gap_request::intent},
    };
}

/**
 * @brief The members of delete_cut_gap_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_cut_gap_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_cut_gap_response::result},
    };
}

/**
 * @brief The members of delete_many_cut_gaps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_cut_gaps_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_cut_gaps_request::removals},
        utility::codec::field{"intent", &delete_many_cut_gaps_request::intent},
    };
}

/**
 * @brief The members of delete_many_cut_gaps_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_cut_gaps_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_cut_gaps_response::result},
    };
}

/**
 * @brief The members of list_by_cut_id_cut_gaps_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_cut_id_cut_gaps_request&) {
    return std::tuple{
        utility::codec::field{"cut_id", &list_by_cut_id_cut_gaps_request::cut_id},
        utility::codec::field{"scope", &list_by_cut_id_cut_gaps_request::scope},
        utility::codec::field{"offset", &list_by_cut_id_cut_gaps_request::offset},
        utility::codec::field{"limit", &list_by_cut_id_cut_gaps_request::limit},
        utility::codec::field{"order", &list_by_cut_id_cut_gaps_request::order},
        utility::codec::field{"filter", &list_by_cut_id_cut_gaps_request::filter},
    };
}

/**
 * @brief The members of list_by_cut_id_cut_gaps_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_cut_id_cut_gaps_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_cut_id_cut_gaps_response::result},
        utility::codec::field{"gaps", &list_by_cut_id_cut_gaps_response::gaps},
        utility::codec::field{"total", &list_by_cut_id_cut_gaps_response::total},
    };
}

/**
 * @brief The members of list_cut_gap_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cut_gap_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_cut_gap_versions_request::key},
        utility::codec::field{"offset", &list_cut_gap_versions_request::offset},
        utility::codec::field{"limit", &list_cut_gap_versions_request::limit},
        utility::codec::field{"order", &list_cut_gap_versions_request::order},
        utility::codec::field{"filter", &list_cut_gap_versions_request::filter},
    };
}

/**
 * @brief The members of list_cut_gap_versions_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_cut_gap_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_cut_gap_versions_response::result},
        utility::codec::field{"versions", &list_cut_gap_versions_response::versions},
        utility::codec::field{"total", &list_cut_gap_versions_response::total},
    };
}

/**
 * @brief The members of get_cut_gap_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cut_gap_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_cut_gap_version_request::key},
    };
}

/**
 * @brief The members of get_cut_gap_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_cut_gap_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_cut_gap_version_response::result},
        utility::codec::field{"version", &get_cut_gap_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_key& v);
    static ores::reporting::messaging::cut_gap_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_key& v);
    static ores::reporting::messaging::cut_gap_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_write& v);
    static ores::reporting::messaging::cut_gap_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_write& v);
    static ores::reporting::messaging::cut_gap_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_change& v);
    static ores::reporting::messaging::cut_gap_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_change& v);
    static ores::reporting::messaging::cut_gap_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_removal& v);
    static ores::reporting::messaging::cut_gap_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_removal& v);
    static ores::reporting::messaging::cut_gap_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_lookup& v);
    static ores::reporting::messaging::cut_gap_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_lookup& v);
    static ores::reporting::messaging::cut_gap_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gaps_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gaps_filter& v);
    static ores::reporting::messaging::cut_gaps_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gaps_filter& v);
    static ores::reporting::messaging::cut_gaps_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_event& v);
    static ores::reporting::messaging::cut_gap_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_event& v);
    static ores::reporting::messaging::cut_gap_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_version_key& v);
    static ores::reporting::messaging::cut_gap_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_version_key& v);
    static ores::reporting::messaging::cut_gap_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::cut_gap_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::cut_gap_versions_filter& v);
    static ores::reporting::messaging::cut_gap_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::cut_gap_versions_filter& v);
    static ores::reporting::messaging::cut_gap_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_cut_gaps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cut_gaps_request& v);
    static ores::reporting::messaging::list_cut_gaps_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_cut_gaps_request& v);
    static ores::reporting::messaging::list_cut_gaps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::list_cut_gaps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::list_cut_gaps_response& v);
    static ores::reporting::messaging::list_cut_gaps_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::list_cut_gaps_response& v);
    static ores::reporting::messaging::list_cut_gaps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_gap_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_gap_request& v);
    static ores::reporting::messaging::get_cut_gap_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_gap_request& v);
    static ores::reporting::messaging::get_cut_gap_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::get_cut_gap_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_gap_response& v);
    static ores::reporting::messaging::get_cut_gap_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::get_cut_gap_response& v);
    static ores::reporting::messaging::get_cut_gap_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_cut_gaps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_cut_gaps_request& v);
    static ores::reporting::messaging::get_many_cut_gaps_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_cut_gaps_request& v);
    static ores::reporting::messaging::get_many_cut_gaps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_cut_gaps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_many_cut_gaps_response& v);
    static ores::reporting::messaging::get_many_cut_gaps_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_cut_gaps_response& v);
    static ores::reporting::messaging::get_many_cut_gaps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_cut_gap_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_cut_gap_request& v);
    static ores::reporting::messaging::put_cut_gap_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_cut_gap_request& v);
    static ores::reporting::messaging::put_cut_gap_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::put_cut_gap_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_cut_gap_response& v);
    static ores::reporting::messaging::put_cut_gap_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::put_cut_gap_response& v);
    static ores::reporting::messaging::put_cut_gap_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_cut_gaps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_many_cut_gaps_request& v);
    static ores::reporting::messaging::put_many_cut_gaps_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_cut_gaps_request& v);
    static ores::reporting::messaging::put_many_cut_gaps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_cut_gaps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_many_cut_gaps_response& v);
    static ores::reporting::messaging::put_many_cut_gaps_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_cut_gaps_response& v);
    static ores::reporting::messaging::put_many_cut_gaps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::delete_cut_gap_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_cut_gap_request& v);
    static ores::reporting::messaging::delete_cut_gap_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::delete_cut_gap_request& v);
    static ores::reporting::messaging::delete_cut_gap_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::delete_cut_gap_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::delete_cut_gap_response& v);
    static ores::reporting::messaging::delete_cut_gap_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::delete_cut_gap_response& v);
    static ores::reporting::messaging::delete_cut_gap_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_cut_gaps_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_cut_gaps_request& v);
    static ores::reporting::messaging::delete_many_cut_gaps_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_cut_gaps_request& v);
    static ores::reporting::messaging::delete_many_cut_gaps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_cut_gaps_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_cut_gaps_response& v);
    static ores::reporting::messaging::delete_many_cut_gaps_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_cut_gaps_response& v);
    static ores::reporting::messaging::delete_many_cut_gaps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_cut_id_cut_gaps_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_cut_id_cut_gaps_request& v);
    static ores::reporting::messaging::list_by_cut_id_cut_gaps_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_cut_id_cut_gaps_request& v);
    static ores::reporting::messaging::list_by_cut_id_cut_gaps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_by_cut_id_cut_gaps_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_by_cut_id_cut_gaps_response& v);
    static ores::reporting::messaging::list_by_cut_id_cut_gaps_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_by_cut_id_cut_gaps_response& v);
    static ores::reporting::messaging::list_by_cut_id_cut_gaps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_cut_gap_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_cut_gap_versions_request& v);
    static ores::reporting::messaging::list_cut_gap_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_cut_gap_versions_request& v);
    static ores::reporting::messaging::list_cut_gap_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_cut_gap_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_cut_gap_versions_response& v);
    static ores::reporting::messaging::list_cut_gap_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_cut_gap_versions_response& v);
    static ores::reporting::messaging::list_cut_gap_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_cut_gap_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_cut_gap_version_request& v);
    static ores::reporting::messaging::get_cut_gap_version_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_cut_gap_version_request& v);
    static ores::reporting::messaging::get_cut_gap_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_cut_gap_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_cut_gap_version_response& v);
    static ores::reporting::messaging::get_cut_gap_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_cut_gap_version_response& v);
    static ores::reporting::messaging::get_cut_gap_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
